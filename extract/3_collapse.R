# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 06/12/2018
# Purpose: Prepare CGF data for the CIAF project
# source("/home/j/temp/jfrostad/ciaf/code/prep.R", echo=T)
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

#load packages
pacman::p_load(data.table, dplyr, feather, readxl)

#options
date <- "2018_07_17" #date of working post-extraction
all.indicators <- c('cooking')
file.type <- "!PUMS" #need to change this structure when i add in IPUMS extractions and review
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
for (point in T:F) {
  
  message("Loading data, point=[", point, "]")

  # Load data
    raw <- paste0(data.dir, ifelse(point, 'points_', 'poly_'), date, ".feather") %>% 
      read_feather %>% 
      as.data.table

  #TODO set this up to handle IPUMS when extracted
  if (file.type == 'ipums') {
    ipums <- T
    ipums_dir <- '/home/j/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/IPUMS_feathers'
    files <- list.files(ipums_dir, '.feather')
    files_length <- length(files)
    indicators <- all.indicators
  } else {
    ipums <- F
    indicators <- all.indicators
  }

  #loop over various families of indicators
  for (this.family in indicators) {

    message(paste('Processing:', this.family))

    #### Subset & Shape Data ####
    message("Initial Cleaning...")
    dt <- initialClean(raw)

    #### Define Indicator ####
    message("Defining Indicator...")
    dt <- defIndicator(dt)

    #### Address Missingness ####
    message("Addressing Missingness...")
    
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

    # Skip the rest of the process if no rows of data are left
    if (nrow(dt) == 0) next
    
    # Crosswalk missing household size data
    #TODO discuss this part with ani after learning more, for now just remove the missing HH sizes
    
    # message("Crosswalking HH Sizes...")
    # if (!ipums) {
    #   ptdat <- hh_cw_reg(data = ptdat)
    # } else {
    #   ptdat <- assign_ipums_hh()
    # }

    message("Collapsing vars for: ", this.family)

    #### Aggregate Data ####
    # Bookmarking dataset so it can be looped over for conditional switch
    # ptdat_preagg <- ptdat
    
    # Conditional switch is to switch collapsing for conditional vs unconditional indicators
    #conditional <- 'unconditional' 
    #TODO talk to ani about this option

    # Aggregate indicator to cluster level
    agg.dt <- aggIndicator(dt) #list of variables to aggregate

    # Skip the rest of the process if no rows of data are left
    if (nrow(dt) == 0) next

    #save poly and point collapses
    message("Saving Collapsed Data...")
    today <- Sys.Date() %>% gsub("-", "_", .)

    paste0(out.dir, "/", ifelse(point, 'points_', 'poly_'), "data_", this.family, '_', today, ".feather") %>%
      write_feather(agg.dt, path=.)

    
  }
}
