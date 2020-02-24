# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 09/05/2018
# Purpose: Run custom functions to create HAP plots 
# source("/homes/jfrostad/_code/lbd/hap/post_estimation/plot.R", echo=T)
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
  

  # necessary to set this option in order to read in a non-english character shapefile on a linux system (cluster)
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
} else {
  j_root <- "J:"
  h_root <- "H:"
  # arg <- c("IND", #current project iteration
  #          "4", #output version
  #          1) #number of cores provided to multicore functions
}

#use cairo to render instead of quartz (quartz causes big slowdowns with geom_sf)
if(!identical(getOption("bitmapType"), "cairo") && isTRUE(capabilities()[["cairo"]])){
  options(bitmapType = "cairo")
}

## Set core_repo location and indicator group
user            <- Sys.info()['user']
core_repo       <- '/homes/jfrostad/_code/lbd/hap/'
commondir       <- paste(core_repo, 'mbg_central/share_scripts/common_inputs', sep = '/')

#load packages
package_lib    <- sprintf('%s_code/_lib/pkg',h_root)
## Load libraries and  MBG project functions.
.libPaths(package_lib)
pacman::p_load(data.table, scales, ggplot2, RColorBrewer, sf, viridis, farver, reldist) 
package_list    <- package_list <- fread('/share/geospatial/mbg/common_inputs/package_list.csv') %>% t %>% c

# Use setup.R functions to load common LBD packages and mbg_central "function" scripts
message('Loading in required R packages and MBG functions')
source(paste0(core_repo, '/mbg_central/setup.R'))
  mbg_setup(package_list = package_list, repos = core_repo)

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
run_date <- '2020_02_12_13_44_44'
#run_date <- '2020_02_07_23_37_07'
lri_run_date <- '2019_10_23_16_13_17'

indicator_group <- 'cooking'
indicator <- 'hap'
type <- 'mean'
raked <- F
start_year <- 2000
end_year <- 2017
cores <- 10
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path('/ihme/geospatial/mbg/cooking/pafs', run_date)
hap.paths <- data.table(admin2=file.path(data.dir, 'admin_2_summary.csv'))
hap.paths.d <- data.table(admin2=file.path(data.dir, 'admin_2_delta_summary.csv'))

###Output###
out.dir  <- file.path('/ihme/geospatial/mbg/cooking/maps', run_date) %T>% dir.create(recursive = T)
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#map functions#
hap.function.dir <- file.path(core_repo, 'post_estimation/_lib')
#this pulls hap collapse helper functions
file.path(hap.function.dir, '/map_fx.R') %>% source

#gbd fx
gbd.shared.function.dir <- '/ihme/cc_resources/libraries/current/r/'
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source
#***********************************************************************************************************************

# ---PREP DATA----------------------------------------------------------------------------------------------------------
#read LRI counts
lri_rate <- 'has_lri_admin_2_raked_mortality_summary.csv'
lri_counts <- 'has_lri_c_admin_2_raked_mortality_summary.csv'

has_lri <- file.path('/ihme/geospatial/mbg/lri/has_lri/output', 
                     lri_run_date, 'pred_derivatives/admin_summaries', lri_rate) %>% 
  fread %>% 
  .[, .(ADM0_CODE, ADM2_CODE, year, lri=mean)]

has_lri_c <- file.path('/ihme/geospatial/mbg/lri/has_lri/output', 
                       lri_run_date, 'pred_derivatives/admin_summaries', lri_counts) %>% 
  fread %>% 
  .[, .(ADM0_CODE, ADM2_CODE, year, lri_c=mean)]

#combine and save all ad2 level results
dt <-
list.files(data.dir, pattern='ad2_tap_results', full.names = T) %>% 
  lapply(., fread) %>% 
  rbindlist(use.names=T, fill=T) %>% 
  merge(has_lri, by=c('ADM0_CODE', 'ADM2_CODE', 'year')) %>% 
  merge(has_lri_c, by=c('ADM0_CODE', 'ADM2_CODE', 'year')) %>% 
  .[, tap_lri := lri * 1000 * tap_paf] %>% #do some postestimation
  .[, hap_lri := lri * 1000 * tap_paf*hap_pct] %>% 
  .[, aap_lri := lri * 1000 * tap_paf*(1-hap_pct)] %T>% 
  write.csv(., file.path(data.dir, 'admin_2_summary.csv'), row.names = F)

#merge sr region names/IDs
locs <- get_location_metadata(location_set_id = 35, gbd_round_id = 6) %>% 
  .[, .(iso3=ihme_loc_id, location_name, super_region_id, super_region_name, region_id, region_name)] #subset to relevant columns
dt <- merge(dt, locs, by='iso3', all.x=T)

#calculate GINI/MAD at country level
dt_ineq <- dt[year %in% c(2000, 2017), .(iso3, year, ADM0_CODE, ADM2_CODE, dfu, super_region_id, super_region_name, region_id, region_name)]
dt_ineq[, gini := gini(dfu), by=.(ADM0_CODE, year)]
dt_ineq[, mad := mad(dfu, center = mean(dfu)), by=.(ADM0_CODE, year)]
dt_ineq[, mean := mean(dfu, na.rm=T), by=.(ADM0_CODE, year)]
dt_ineq[, max := max(dfu, na.rm=T), by=.(ADM0_CODE, year)]
dt_ineq[, min := min(dfu, na.rm=T), by=.(ADM0_CODE, year)]
dt_ineq[, range := max-min]
dt_ineq <- unique(dt_ineq[year==2017], by=c('ADM0_CODE', 'year'))
dt_ineq <- dt_ineq[dt_ineq[,do.call(order, .SD), .SDcols = c('super_region_id', 'mean')]]
dt_ineq[, country := factor(iso3, levels=unique(iso3))]

#GINI results
summary(dt_ineq$gini)
dt_ineq[year==2000 & gini>mean(gini, na.rm=T), uniqueN(iso3)]
dt_ineq[year==2017 & gini>mean(gini, na.rm=T), uniqueN(iso3)]
dt_ineq[gini>mean(gini, na.rm=T), table(iso3, year)]

#TODO move this to be at pixel level
#calculate rates of change
these_cols <- c('hap_pct', 'dfu', 'tap_paf')
d_cols <- paste0(these_cols, '_d')
dr_cols <- paste0(these_cols, '_dr')
dt_d <- setkey(dt, ADM0_CODE, ADM2_CODE) %>% 
  .[year %in% c(2000, 2017)] %>% 
  .[, (d_cols) := .SD-data.table::shift(.SD,n=1), .SDcols=these_cols, by=key(dt)] %>% 
  .[, (dr_cols) := (.SD-data.table::shift(.SD,n=1))/data.table::shift(.SD,n=1), .SDcols=these_cols, by=key(dt)] 

write.csv(dt_d, file.path(data.dir, 'admin_2_delta_summary.csv'), row.names = F)

#read in input data and prepare it for mapping
tic('total')
tic('loading data')

map_data <-  
  load_map_results(indicator, indicator_group, run_date, raked, 
                   start_year=2000, end_year=2017,
                   custom_path = hap.paths,
                   geo_levels=c('admin2'),
                   cores=cores)
map_data_d <-
  load_map_results(indicator, indicator_group, run_date, raked, 
                   start_year=2000, end_year=2017,
                   custom_path = hap.paths.d,
                   geo_levels=c('admin2'),
                   cores=cores)
#***********************************************************************************************************************

# ---GLOBAL-------------------------------------------------------------------------------------------------------------

#***********************************************************************************************************************

# ---COUNTRY------------------------------------------------------------------------------------------------------------
#testing
ctry.name <- 'Nigeria'
ctry_data <- data$admin2 %>% 
  copy %>% 
  filter(NAME_0==ctry.name)
ctry.zoom <- data.table(x1=1, x2=15, y1=3, y2=15) #NGA

#***********************************************************************************************************************

# ---SCRAPS-------------------------------------------------------------------------------------------------------------


#***********************************************************************************************************************