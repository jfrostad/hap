# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 06/12/2018
# Purpose: Collapse data for HAP
# source("/homes/jfrostad/_code/lbd/housing/extract/3_collapse.R", echo=T)
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j/"
  h_root <- file.path("/homes", Sys.info()["user"])
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
#TODO verify which of these are actually necessary, took from a random image in Ani's wash dir
#/share/geospatial/mbg/wash/s_imp/model_image_history
pkg.list <- c('RMySQL', 'data.table', 'dismo', 'doParallel', 'dplyr', 'foreign', 'gbm', 'ggplot2', 'glmnet', 
              'grid', 'gridExtra', 'gtools', 'magrittr', 'pacman', 'parallel', 'plyr', 'raster', 'rgdal', 'rgeos',
              'seegMBG', 'seegSDM', 'tictoc') #will be loaded by MBG setup

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
date <- "2018_10_02" #date of current post-extraction
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path(j_root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/')
census.dir <- file.path(j_root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/census')
doc.dir <- file.path(j_root, 'WORK/11_geospatial/hap/documentation')
def.file <- file.path(doc.dir, 'definitions.xlsx')

###Output###
out.dir  <- file.path(j_root,'LIMITED_USE/LU_GEOSPATIAL/collapse/hap/')
model.dir  <- file.path(j_root, 'WORK/11_geospatial/10_mbg/input_data/hap')
share.model.dir  <- file.path('/share/geospatial/mbg/input_data/')
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#general functions#
central.function.dir <- file.path(h_root, "_code/_lib/functions")
# this pulls the general misc helper functions
file.path(central.function.dir, "misc.R") %>% source
#hap functions#
hap.function.dir <- file.path(h_root, '_code/lbd/housing/extract/functions')
#this pulls hap collapse helper functions
file.path(hap.function.dir, '/collapse_fx.R') %>% source
#shared functions#
gbd.shared.function.dir <- file.path(j_root,  "temp/central_comp/libraries/current/r")
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source
file.path(gbd.shared.function.dir, 'get_ids.R') %>% source
file.path(gbd.shared.function.dir, 'get_covariate_estimates.R') %>% source

lbd.shared.function.dir <- file.path(h_root, "_code/lbd/lbd_core/mbg_central")
file.path(lbd.shared.function.dir, 'setup.R') %>% source
  mbg_setup(repo=lbd.shared.function.dir, package_list=pkg.list) #load mbg functions
#***********************************************************************************************************************

# ---COLLAPSE-----------------------------------------------------------------------------------------------------------
#loop over points and polygons to collapse
collapseData <- function(this.family,
                         point=NULL,
                         census.file=NULL,
                         out.temp=NULL) {
  
  message("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  #if you want to output temp files, create this dir
  if (!is.null(out.temp)) file.path(out.temp, this.family) %>% dir.create(recursive=T)

  #ipums files are done in parallel due to size
  if (!missing(census.file)) {
    message("CENSUS FILE=", census.file)
    census <- T
    # Load data
    raw <- read_feather(census.file) %>% 
      as.data.table
    # Determine if point or poly
    point <- !all(raw[, lat] %>% unique %>% is.na)
    
  } else {
    census <- F
    # Load data
    raw <- paste0(data.dir, ifelse(point, 'points_', 'poly_'), date, ".feather") %>% 
      read_feather %>% 
      as.data.table
  }
  
  message("Loading data...[point=", point, "]/[census=", census, "]")

  #loop over various families of indicators
  message(paste('->Processing:', this.family))

  #### Subset & Shape Data ####
  dt <- initialClean(raw, var.fam=this.family, is.point=point, this.out.temp=out.temp) %>% 
    defIndicator(., var.fam=this.family, definitions=def.file, debug=F)

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
  
  #browser()

  #### Aggregate Data ####
  # Aggregate indicator to cluster level
  message("\nBegin Collapsing Variables")
  agg.dt <- aggIndicator(dt, var.fam=this.family, is.point=point) #list of variables to aggregate
  agg.dt[, polygon := !(point)]
  message("->Complete!")
  
  # Skip the rest of the process if no rows of data are left
  if (nrow(dt) == 0) message('no data left to return!')
  else return(agg.dt)

}
  
#populate vector of IPUMS filepaths
ipums.files = list.files(census.dir, full.names = T)

#Run fx for each family
cooking <- mapply(collapseData, point=T:F, this.family='cooking', SIMPLIFY=F) %>% rbindlist
cooking.census <- mapply(collapseData, census.file=ipums.files, this.family='cooking', SIMPLIFY=F) %>% rbindlist
# housing <- mapply(collapseData, point=F, census=F, this.family='housing', SIMPLIFY=F,
#                   out.temp='/share/geospatial/jfrostad') %>% rbindlist

#Redfine the row_id
cooking[, row_id := .I]
setkey(cooking, row_id)

#save poly and point collapses
#TODO loop over all fams in fx
this.family='cooking'
paste0(out.dir, "/", "data_", this.family, '_', today, ".feather") %>%
  write_feather(cooking, path=.)
#***********************************************************************************************************************

# ---RESAMPLE-----------------------------------------------------------------------------------------------------------
#prep for resampling
cooking[, cooking_fuel := N * bin_cooking_fuel_mapped]

#drop weird shapefiles for now
#TODO investigate these issues
cooking <- cooking[!(shapefile %like% "2021")] 
cooking <-cooking[!(shapefile %like% "gadm_3_4_vnm_adm3")]

#only work on PER for now
#cooking <- cooking[iso3 == "PER"]

#run core resampling code
dt <- resample_polygons(data = cooking,
                        cores = 10,
                        indic = 'cooking_fuel',
                        density = 0.001,
                        gaul_list = lapply(unique(cooking$iso3) %>% tolower, get_gaul_codes) %>% unlist %>% unique)
                        #gaul_list = lapply(c('stage1','stage2'), get_gaul_codes) %>% unlist %>% unique)

#save resampled data
paste0(model.dir, "/", "data_", this.family, '_', today, ".feather") %>%
  write_feather(dt, path=.)

#prep for MDG
setnames(dt,
         c('iso3', 'int_year'),
         c('country', 'year'))

#TODO should simplify dataset by dropping useless vars
dt <- dt[, list(nid, country, year, latitude, longitude, survey_series, urban, cooking_fuel, N, sum_of_sample_weights,
                cluster_id, polygon, shapefile, location_code, weight, pseudocluster)]

#save into MDG dir
file.path(share.model.dir, 'cooking_clean.RDS') %>% saveRDS(dt, file=.)
file.path(share.model.dir, 'cooking_clean.csv') %>% write.csv(dt, file=., row.names=F)
#save both ways so that you can try modelling from either end
file.path(share.model.dir, 'cooking_dirty.RDS') %>% saveRDS(dt, file=.)
file.path(share.model.dir, 'cooking_dirty.csv') %>% write.csv(dt, file=., row.names=F)
