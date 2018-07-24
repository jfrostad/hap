# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 06/12/2018
# Purpose: Collapse data for HAP
# source("/homes/jfrostad/_code/hap//extract/3_collapse.R", echo=T)
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j/"
  h_root <- "/homes/jfrostad/"
  arg <- commandArgs()[-(1:3)] # First args are for unix use only
  
  if (length(arg)==0) {
    # arg <- c("IND", #current project iteration
    #          "8", #output version
    #          1) #number of cores provided to multicore functions
  }
  
  package_lib    <- sprintf('%s_code/_lib/pkg',h_root)
  ## Load libraries and  MBG project functions.
  .libPaths(package_lib)
  
  # necessary to set this option in order to read in a non-english character shapefile on a linux system (cluster)
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
} else {
  j_root <- "J:"
  h_root <- "H:"
  # arg <- c("IND", #current project iteration
  #          "4", #output version
  #          1) #number of cores provided to multicore functions
}

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#load packages
pacman::p_load(data.table, dplyr, feather, readxl)

#options
date <- "2018_07_17" #date of working post-extraction
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path(j_root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/')
doc.dir <- file.path(j_root, 'WORK/11_geospatial/hap/documentation')
def.file <- file.path(doc.dir, 'definitions.xlsx')

###Output###
out.dir  <- file.path(j_root,'LIMITED_USE/LU_GEOSPATIAL/collapse/hap/')
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#general functions#
central.function.dir <- file.path(h_root, "_code/_lib/functions")
# this pulls the general misc helper functions
file.path(central.function.dir, "misc.R") %>% source
#hap functions#
hap.function.dir <- file.path(h_root, '_code/hap/extract/functions')
#this pulls hap collapse helper functions
file.path(hap.function.dir, '/collapse_fx.R') %>% source
#shared functions#
shared.function.dir <- file.path(j_root,  "temp/central_comp/libraries/current/r")
file.path(shared.function.dir, 'get_location_metadata.R') %>% source
file.path(shared.function.dir, 'get_ids.R') %>% source
file.path(shared.function.dir, 'get_covariate_estimates.R') %>% source
#***********************************************************************************************************************

# ---COLLAPSE-----------------------------------------------------------------------------------------------------------
#loop over points and polygons to collapse
collapseData <- function(point, census, this.family) {
  
  message("Loading data...[point=", point, "]/[census=", census, "]")

  #TODO set this up to handle IPUMS when extracted
  if (census) {
    ipums <- T
    ipums_dir <- '/home/j/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/IPUMS_feathers'
    files <- list.files(ipums_dir, '.feather')
    files_length <- length(files)
  } else {
    ipums <- F
    # Load data
    raw <- paste0(data.dir, ifelse(point, 'points_', 'poly_'), date, ".feather") %>% 
      read_feather %>% 
      as.data.table
  }

  #loop over various families of indicators
  message(paste('->Processing:', this.family))

  #### Subset & Shape Data ####
  dt <- initialClean(raw, var.fam=this.family, is.point=point) %>% 
    defIndicator(., var.fam=this.family, definitions=def.file)

  #### Address Missingness ####
  message("\nBegin Addressing Missingness...")
  
  # ID clusters with more than 20% weighted missingness
  #TODO set this up to loop over all vars
  missing.vars <- idMissing(dt, this.var="bin_cooking_fuel_mapped", criteria=.2, wt.var='hh_size') 
  dt <- dt[!(cluster_id %in% missing.vars)] #remove these clusters

  #Remove cluster_ids with missing hhweight or invalid 
  #TODO confirm with Ani why zero tolerance for this? id #534 only has one missing weight
  missing.wts <- idMissing(dt, this.var="hhweight", criteria=0, wt.var=NA)
  dt <- dt[!(cluster_id %in% missing.wts)] #remove these clusters
  #TODO, investigate these rows, about 25% of data & they always have missing hh_size too
  invalid.wts <- unique(dt[hhweight==0, cluster_id]) 
  dt <- dt[!(cluster_id %in% invalid.wts)] #remove these clusters
  #TODO, none of these after the last filter, but there are missing hhsizes to investigate...
  invalid.sizes <- unique(dt[hh_size<=0, cluster_id]) 
  dt <- dt[!(cluster_id %in% invalid.sizes)] #remove these clusters
  #ID missing hh sizes, talk to ani about crosswalk specs
  missing.sizes <- idMissing(dt, this.var="hh_size", criteria=0, wt.var=NA)
  dt <- dt[!(cluster_id %in% missing.sizes)] #remove these clusters

  # Crosswalk missing household size data
  #TODO discuss this part with ani after learning more, for now just remove the missing HH sizes
  
  # message("Crosswalking HH Sizes...")
  # if (!ipums) {
  #   ptdat <- hh_cw_reg(data = ptdat)
  # } else {
  #   ptdat <- assign_ipums_hh()
  # }

  message("\nBegin Collapsing Variables")

  #### Aggregate Data ####
  # Aggregate indicator to cluster level
  agg.dt <- aggIndicator(dt, var.fam=this.family, is.point=point) #list of variables to aggregate

  # Skip the rest of the process if no rows of data are left
  if (nrow(dt) == 0) message('no data left to return!')
  else return(agg.dt)

}

#Run fx for each family
cooking <- mapply(collapseData, point=T:F, census=F, this.family='cooking', SIMPLIFY=F) %>% rbindlist

#Redfine the row_id
cooking[, row_id := .I] 
setkey(cooking, row_id)

#save poly and point collapses
#TODO loop over all fams in fx
this.family='cooking'
paste0(out.dir, "/", "data_", this.family, '_', today, ".feather") %>%
  write_feather(cooking, path=.)
