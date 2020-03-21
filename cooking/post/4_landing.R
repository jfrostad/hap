# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 09/05/2018
# Purpose: Produce HAP results and sub-analyses
# source("/homes/jfrostad/_code/lbd/hap/cooking/post/4_landing.R", echo=T)
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
run_date <- '2020_02_24_12_20_17'
#run_date <- '2020_02_07_23_37_07'
lri_run_date <- '2019_10_23_16_13_17'
shapefile <- "2019_09_10"

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
#results
data.dir <- file.path('/ihme/geospatial/mbg/cooking/pafs', run_date)

lri_dir <- '/ihme/geospatial/mbg/lri/has_lri/output'
  lri_rate_path <- 'has_lri_admin_2_raked_mortality_summary.csv'
  lri_counts_path <- 'has_lri_c_admin_2_raked_mortality_summary.csv'
  
#link
global_link_dir <- file.path('/home/j/WORK/11_geospatial/admin_shapefiles', shapefile) #TODO make official
  
###Output###
hap.paths <- data.table(admin2=file.path(data.dir, 'admin_2_summary.csv'))
hap.paths.d <- data.table(admin2=file.path(data.dir, 'admin_2_delta_summary.csv'))
out.dir  <- file.path('/ihme/geospatial/mbg/cooking/maps', run_date) %T>% dir.create(recursive = T)
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#PE functions#
hap.function.dir <- file.path(core_repo, 'post_estimation/_lib')
#this pulls hap collapse helper functions
file.path(hap.function.dir, '/map_fx.R') %>% source

#gbd fx
gbd.shared.function.dir <- '/ihme/cc_resources/libraries/current/r/'
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source

##custom fx##
#TODO move to custom fx scripts
#spatial functions
#return the max/min districts in a country and the distance between them
exploreRange <- function(explore_country, shp, dt, var) {
  
  out <- dt %>% 
    copy %>% 
    setnames(., var, 'var') %>% 
    .[year==analysis_year & iso3==explore_country & !is.na(var), .(var, ADM2_NAME)] %>% 
    .[order(var)] %>% 
    .[c(1, nrow(.)), .(var, ADM2_NAME)]
  
  st_distance(filter(shp, NAME_2==out[1, ADM2_NAME]), 
              filter(shp, NAME_2==out[2, ADM2_NAME])) %>% 
    as.numeric %>% 
    round %>% 
    {if (length(.)>1) message('warning: multipolygon, returning min distance'); min(.)} %>% 
    message('\nDistance is ', ./1e3, ' km')
  
  return(out)
  
}

#function to find polygons that share a border (rook neighbors)
st_rook <- function(a, b = a) st_relate(a, b, pattern = "F***1****") 
#***********************************************************************************************************************

# ---PREP DATA----------------------------------------------------------------------------------------------------------
##read in and prep datasets for analysis##
#read in link_table
global_link_table <- file.path(global_link_dir, "lbd_full_link.rds") %>% readRDS %>%  as.data.table
ad2_links <- global_link_table[, .(ADM0_NAME, ADM2_NAME, ADM2_CODE)] %>% unique

#read in shps
stage1 <- st_read('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/shps_by_stage/stage1_ad2_gadm.shp')
stage2 <- st_read('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/shps_by_stage/stage2_ad2_gadm.shp')
adm2 <- rbind(stage1, stage2)

#read LRI rates/counts
has_lri <- file.path(lri_dir, lri_run_date, 'pred_derivatives/admin_summaries', lri_rate_path) %>% 
  fread %>% 
  .[, .(ADM0_CODE, ADM2_CODE, year, lri=mean)]

has_lri_c <- file.path('/ihme/geospatial/mbg/lri/has_lri/output', 
                       lri_run_date, 'pred_derivatives/admin_summaries', lri_counts_path) %>% 
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
  .[, aap_lri := lri * 1000 * tap_paf*(1-hap_pct)] %>% 
  .[, tap_lri_c := lri_c * tap_paf] %>% #do some postestimation
  .[, hap_lri_c := lri_c * tap_paf*hap_pct] %>% 
  .[, aap_lri_c := lri_c * tap_paf*(1-hap_pct)] %T>% 
  #also output the file for later
  write.csv(., file.path(data.dir, 'admin_2_summary.csv'), row.names = F)

#merge sr region names/IDs
locs <- get_location_metadata(location_set_id = 35, gbd_round_id = 6) %>% 
  .[, .(iso3=ihme_loc_id, location_name, super_region_id, super_region_name, region_id, region_name)] #subset to relevant columns
dt <- merge(dt, locs, by='iso3', all.x=T)
dt <- merge(dt, ad2_links, by='ADM2_CODE')

#intermediate measures
dt[, u5_pct := pop/pop_total]

#also combine and save all ad0 level results
dt_ad0 <-
  list.files(data.dir, pattern='ad0_tap_results', full.names = T) %>% 
  lapply(., fread) %>% 
  rbindlist(use.names=T, fill=T) %>% 
  merge(., locs, by='iso3', all.x=T) %T>%
  #also output the file for later   
  write.csv(., file.path(data.dir, 'admin_0_summary.csv'), row.names = F)

#also create ad2 sf object for spatial analyses
data <-  
  load_map_results(indicator, indicator_group, run_date, raked, 
                   start_year=2000, end_year=2017,
                   custom_path = hap.paths,
                   geo_levels=c('admin2'),
                   cores=cores)

data_d <-  
  load_map_results(indicator, indicator_group, run_date, raked, 
                   start_year=2000, end_year=2017,
                   custom_path = hap.paths.d,
                   geo_levels=c('admin2'),
                   cores=cores)
#***********************************************************************************************************************

# ---SPATIAL VARIATION--------------------------------------------------------------------------------------------------
##2017 patterns##
#TODO verify that popweighting shoudl be done with u5 pop, if not switch pop vars here
#country level
analysis_year <- 2017
dt_ad0[year==analysis_year, sum(pop_total*dfu)] #pop exposed to dfu in 2017
dt_ad0[year==analysis_year, sum(pop*dfu)] #u5 exposed to dfu in 2017
dt_ad0[year==analysis_year, weighted.mean(dfu, w=pop)] #avg proportion
dt_ad0[year==analysis_year & super_region_name=='Sub-Saharan Africa', weighted.mean(dfu, w=pop)] #avg proportion in SSA
dt_ad0[year==analysis_year, .(mean=weighted.mean(dfu, w=pop)), by=region_name] %>% .[order(mean)]
dt_ad0[year==analysis_year & super_region_name=='Sub-Saharan Africa', .(mean=weighted.mean(dfu, w=pop)), by=iso3] %>% .[order(mean)]
dt_ad0[year==analysis_year & super_region_name=='Latin America and Caribbean', .(mean=weighted.mean(dfu, w=pop)), by=iso3] %>% .[order(mean)]

#district level
#counts based on threshold
threshold <- .95
dt[year==analysis_year, .(mean=weighted.mean(dfu, w=pop)), by=.(ADM2_CODE, iso3, super_region_name)] %>% 
  .[, .(count=sum(mean>threshold, na.rm=T), N=.N), by=super_region_name] %>% 
  .[, .(pct=count/N), by=super_region_name]
dt[year==analysis_year, .(mean=weighted.mean(dfu, w=pop)), by=.(ADM2_CODE, iso3, region_name)] %>% 
  .[, .(count=sum(mean>threshold, na.rm=T), N=.N), by=region_name] %>% 
  .[, .(pct=count/N), by=region_name]
dt[year==analysis_year & super_region_name=='Sub-Saharan Africa', .(mean=weighted.mean(dfu, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count=sum(mean>threshold, na.rm=T), N=.N), by=iso3] %>% 
  .[, .(pct=count/N), by=iso3] %>% 
  .[order(pct)]
dt[year==analysis_year & super_region_name!='Sub-Saharan Africa', .(mean=weighted.mean(dfu, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count=sum(mean>threshold, na.rm=T), N=.N), by=iso3] %>% 
  .[, .(pct=count/N), by=iso3] %>% 
  .[order(pct)]

dt[year==analysis_year & super_region_name=='Sub-Saharan Africa', .(mean=weighted.mean(dfu, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count=sum(mean<(1-threshold), na.rm=T), N=.N), by=iso3] %>% 
  .[, .(pct=count/N), by=iso3] %>% 
  .[order(pct)]
dt[year==analysis_year & super_region_name!='Sub-Saharan Africa', .(mean=weighted.mean(dfu, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count=sum(mean<(1-threshold), na.rm=T), N=.N), by=iso3] %>% 
  .[, .(pct=count/N), by=iso3] %>% 
  .[order(pct)]

#counts of exposed
dt[year==analysis_year, .(count=sum(dfu*pop_total, na.rm=T)), by=.(ADM2_CODE, ADM2_NAME, iso3)] %>% 
  .[order(count)]
#***********************************************************************************************************************

# ---INEQUALITY---------------------------------------------------------------------------------------------------------
##produce metrics of inequality for 2017##
#calculate GINI/MAD at country level
dt_ineq <- dt[year %in% c(2000, analysis_year), .(iso3, year, ADM0_CODE, ADM2_CODE, ADM2_NAME, dfu, 
                                         super_region_id, super_region_name, region_id, region_name)]
dt_ineq[, gini := gini(dfu), by=.(iso3, year)]
dt_ineq[, mad := mad(dfu, center = mean(dfu)), by=.(iso3, year)]
dt_ineq[, mean := mean(dfu, na.rm=T), by=.(iso3, year)]
dt_ineq[, max := max(dfu, na.rm=T), by=.(iso3, year)]
dt_ineq[, min := min(dfu, na.rm=T), by=.(iso3, year)]
dt_ineq[, range := max-min]

#range results
dt_ineq[year==analysis_year, .(range=max-min), by=iso3] %>% 
  unique %>% 
  .[order(range)]

exploreRange('LBR', shp=adm2, dt=dt, var='dfu')
exploreRange('EGY', shp=adm2, dt=dt, var='dfu')
exploreRange('PHL', shp=adm2, dt=dt, var='dfu')
exploreRange('NGA', shp=adm2, dt=dt, var='dfu')

#find the most extreme rook neighbors
data_17 <- filter(data$admin2, year==analysis_year) #TODO could be written more eloquently
rooks <- filter(data$admin2, year==analysis_year) %>% 
  dplyr::select(dfu, ADM0_CODE, ADM0_NAME, ADM1_CODE, ADM1_NAME, ADM2_CODE, ADM2_NAME, year) %>%  
  st_join(data_17, st_rook, suffix=c('_og', '_rook')) %>% 
  mutate(range=dfu_og-dfu_rook) 

summary(rooks$range)
quantile(rooks$range, p=.97, na.rm=T)
filter(rooks, range==max(range, na.rm=T)) #biggest contrast
extreme_rooks <- filter(rooks, range>quantile(rooks$range, p=.99, na.rm=T)) #top 1% of contrasts
as.character(extreme_rooks$ADM0_NAME_og) %>% table #which countries had the most districts with large contrast

#find which countries have the highest percentage of districts with at least one extreme rook
distinct(extreme_rooks, ADM2_NAME_og, .keep_all = T) %>% 
  count('ADM0_NAME_og') %>% 
  merge(., count(data_17, 'ADM0_NAME'), by.y='ADM0_NAME', by.x='ADM0_NAME_og') %>% 
  mutate(pct=freq.x/freq.y) %>% 
  arrange(pct)
  
#GINI results
summary(dt_ineq$gini)
dt_ineq[year==2000 & gini>mean(gini, na.rm=T), uniqueN(iso3)]
dt_ineq[year==analysis_year & gini>mean(gini, na.rm=T), uniqueN(iso3)]
dt_ineq[gini>mean(gini, na.rm=T), table(iso3, year)]
#***********************************************************************************************************************

# ---TEMPORAL VARIATION-------------------------------------------------------------------------------------------------
##analyses of trends/change over study period##
#TODO move this to be at pixel level
#calculate rates of change
these_cols <- c('hap_pct', 'dfu', 'tap_paf', 'tap_pc')
d_cols <- paste0(these_cols, '_d')
dr_cols <- paste0(these_cols, '_dr')
aroc_cols <- paste0(these_cols, '_aroc')
dt_d <- setkey(dt, ADM0_CODE, ADM2_CODE) %>% 
  .[year %in% c(2000, analysis_year)] %>% 
  .[, (d_cols) := .SD-data.table::shift(.SD,n=1), .SDcols=these_cols, by=key(dt)] %>% 
  .[, (dr_cols) := (.SD-data.table::shift(.SD,n=1))/data.table::shift(.SD,n=1), .SDcols=these_cols, by=key(dt)] %T>% 
  .[, (aroc_cols) := (.SD-data.table::shift(.SD,n=1))/(analysis_year-2000), .SDcols=these_cols, by=key(dt)] %T>% 
  #also output the file for later
  write.csv(., file.path(data.dir, 'admin_2_delta_summary.csv'), row.names = F)

start_year <- min(dt_ad0$year, na.rm=T) #define start of sample

#global results
#TODO why not use dt_d for this
dt_ad0[year==start_year, sum(pop_total*dfu)] - dt_ad0[year==analysis_year, sum(pop_total*dfu)] #change in pop exposed to dfu in 2000-2017
#pct decrease in proportion at global level
(dt_ad0[year==analysis_year, 
        weighted.mean(dfu, w=pop)] - dt_ad0[year==start_year, 
                                            weighted.mean(dfu, w=pop)])/ dt_ad0[year==start_year, 
                                                                                weighted.mean(dfu, w=pop)]
#regional/ad0 results
#regional values at start
dt_ad0[year==start_year & super_region_name=='Sub-Saharan Africa', weighted.mean(dfu, w=pop)] #avg proportion in SSA
dt_ad0[year==start_year, .(mean=weighted.mean(dfu, w=pop)), by=region_name] %>% .[order(mean)]
dt_ad0[year==start_year & super_region_name=='Sub-Saharan Africa', 
       .(mean=weighted.mean(dfu, w=pop)), by=iso3] %>% .[order(mean)]
dt_ad0[year==start_year & super_region_name=='Latin America and Caribbean', 
       .(mean=weighted.mean(dfu, w=pop)), by=iso3] %>% .[order(mean)]

#regional change 
dt_d[year==analysis_year & super_region_name=='Sub-Saharan Africa',
     weighted.mean(dfu_dr, w=pop, na.rm=T)] #avg proportion change
dt_d[year==analysis_year, .(mean=weighted.mean(dfu_dr, w=pop, na.rm=T)), by=region_name] %>% .[order(mean)]
dt_d[year==analysis_year & region_name %like% "Andean|Tropical", .(mean=weighted.mean(dfu_dr, w=pop, na.rm=T))]
dt_d[year==analysis_year & super_region_name=='Sub-Saharan Africa', 
     .(mean=weighted.mean(dfu_dr, w=pop, na.rm=T)), by=iso3] %>% .[order(mean)]
dt_d[year==analysis_year & super_region_name!='Sub-Saharan Africa', 
     .(mean=weighted.mean(dfu_dr, w=pop, na.rm=T)), by=iso3] %>% .[order(mean)]
dt_d[year==analysis_year & super_region_name!='Sub-Saharan Africa', 
     .(mean=weighted.mean(dfu_d, w=pop, na.rm=T)), by=iso3] %>% .[order(mean)]

#regional change
# these_cols <- c('pop', 'dfu')
# dfu_d <- dt_ad0[year %in% c(start_year, analysis_year), .(iso3, year, pop, pop_total, dfu, super_region_name, region_name)] %>% 
#   copy %>% 
#   .[, paste0(these_cols, '_shift') := data.table::shift(.SD, n=1), .SDcols=these_cols, by=iso3] %>% 
#   .[year==analysis_year]
# dfu_d[super_region_name=='Sub-Saharan Africa', weighted.mean(dfu_shift, w=pop_shift)-weighted.mean(dfu, w=pop)/weighted.mean(dfu_shift, w=pop_shift)] #avg proportion in SSA
# dfu_d[, .(mean=weighted.mean(dfu_shift, w=pop_shift)-weighted.mean(dfu, w=pop)/weighted.mean(dfu_shift, w=pop_shift)), by=region_name] %>% .[order(mean)]
# dfu_d[super_region_name=='Sub-Saharan Africa', .(mean=weighted.mean(dfu_shift, w=pop_shift)-weighted.mean(dfu, w=pop)/weighted.mean(dfu_shift, w=pop_shift)), by=iso3] %>% .[order(mean)]
# dfu_d[super_region_name=='Latin America and Caribbean', .(mean=weighted.mean(dfu_shift, w=pop_shift)-weighted.mean(dfu, w=pop)/weighted.mean(dfu_shift, w=pop_shift)), by=iso3] %>% .[order(mean)]

#district level
#biggest change
dt_d[!is.na(dfu_d)] %>% 
  .[order(dfu_d)]

dt_d[!is.na(dfu_d) & dfu_d >= 0] # number of districts making no progress
nrow(dt_d[!is.na(dfu_d) & dfu_d >= 0]) / nrow(dt_d)

#counts based on threshold of improving less than 1%
threshold <- -.01
q75_dfu <- dt_ad0[year==analysis_year, quantile(dfu, p=.75, na.rm=T)]
q75_countries <- dt_ad0[year==analysis_year & dfu>=q75_dfu, unique(iso3)]
dt_d[year==analysis_year & iso3 %in% q75_countries & dfu_dr >= threshold] %>% nrow 
nrow(dt_d[year==analysis_year & iso3 %in% q75_countries & dfu_dr >= threshold]) / nrow(dt_d)

dt_d[year==analysis_year & iso3 %in% q75_countries, .(mean=weighted.mean(dfu_dr, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count=sum(mean>threshold, na.rm=T), N=.N), by=iso3] %>% 
  .[, .(pct=count/N), by=iso3] %>% 
  .[order(pct)]
dt_d[year==analysis_year & super_region_name!='Sub-Saharan Africa', .(mean=weighted.mean(dfu_dr, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count=sum(mean>threshold, na.rm=T), N=.N), by=iso3] %>% 
  .[, .(pct=count/N), by=iso3] %>% 
  .[order(pct)]

dt_d[year==analysis_year & super_region_name=='Sub-Saharan Africa', .(mean=weighted.mean(dfu_dr, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count=sum(mean<(1-threshold), na.rm=T), N=.N), by=iso3] %>% 
  .[, .(pct=count/N), by=iso3] %>% 
  .[order(pct)]
dt_d[year==analysis_year & super_region_name!='Sub-Saharan Africa', .(mean=weighted.mean(dfu_dr, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count=sum(mean<(1-threshold), na.rm=T), N=.N), by=iso3] %>% 
  .[, .(pct=count/N), by=iso3] %>% 
  .[order(pct)]

#range results
dt_d[year==analysis_year, .(range=max(dfu_d, na.rm=T)-min(dfu_d, na.rm=T)), by=iso3] %>% 
  unique %>% 
  .[order(range)]

#explore the range to find inequality
exploreRange('MEX', shp=adm2, dt=dt_d, var='dfu_d')

#explore the range to find equality
#select countries that improved by more than 10%
threshold <- -.10
progress_countries <- dt_d[year==analysis_year, .(mean=weighted.mean(dfu_d, w=pop, na.rm=T)), by=iso3] %>% 
  .[mean<threshold, unique(iso3)]

dt_d[year==analysis_year & iso3 %in% progress_countries, .(range=max(dfu_d, na.rm=T)-min(dfu_d, na.rm=T)), by=iso3] %>% 
  unique %>% 
  .[order(range)]

exploreRange('BLZ', shp=adm2, dt=dt_d, var='dfu_d')
#***********************************************************************************************************************

# ---SDG PROJECTIONS----------------------------------------------------------------------------------------------------
##Predict SDG 7.1 attainability##
#TODO move this to be at pixel level
#TODO predict the SEV instead
dt_d[year==analysis_year, dfu_pred := dfu + dfu_aroc * (2030-year)] #naive AROC pred for pre-sub
dt_d[year==analysis_year, .(mean=weighted.mean(dfu_pred, w=pop, na.rm=T)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count=sum(mean>.05, na.rm=T), N=.N), by=iso3] %>% 
  .[, .(pct=count/N), by=iso3] %>% 
  .[order(pct)]
#***********************************************************************************************************************
 
# ---AIR POLLUTION------------------------------------------------------------------------------------------------------
##analyze relationship to AAP; TAP; HAP_SHARE##

##global levels/change
#current tap_pc/hap_pct/tap_paf global avg
dt[year==analysis_year, weighted.mean(tap_pc, w=pop, na.rm=T)]
dt[year==analysis_year, weighted.mean(hap_pct, w=pop, na.rm=T)]
dt[year==analysis_year, weighted.mean(tap_paf, w=pop, na.rm=T)]
#pct decreases at global level
dt_d[, weighted.mean(tap_pc_dr, w=pop, na.rm=T)]
dt_d[, weighted.mean(hap_pct_dr, w=pop, na.rm=T)]
dt_d[, weighted.mean(tap_paf_dr, w=pop, na.rm=T)]

##superregion levels
dt[year==analysis_year, weighted.mean(tap_pc, w=pop, na.rm=T), by=super_region_name]
dt[year==analysis_year, weighted.mean(hap_pct, w=pop, na.rm=T), by=super_region_name]
dt[year==analysis_year, weighted.mean(tap_paf, w=pop, na.rm=T), by=super_region_name]
#superregion pct decrease
dt_d[, weighted.mean(tap_pc_dr, w=pop, na.rm=T), by=super_region_name]
dt_d[, weighted.mean(hap_pct_dr, w=pop, na.rm=T), by=super_region_name]
dt_d[, weighted.mean(tap_paf_dr, w=pop, na.rm=T), by=super_region_name]

##region levels
dt[year==analysis_year, weighted.mean(tap_pc, w=pop, na.rm=T), by=region_name]
dt[year==analysis_year, weighted.mean(hap_pct, w=pop, na.rm=T), by=region_name]
dt[year==analysis_year, weighted.mean(tap_paf, w=pop, na.rm=T), by=region_name]
#region pct decrease
dt_d[, weighted.mean(tap_pc_dr, w=pop, na.rm=T), by=region_name]
dt_d[, weighted.mean(hap_pct_dr, w=pop, na.rm=T), by=region_name]
dt_d[, weighted.mean(tap_paf_dr, w=pop, na.rm=T), by=region_name]

##country levels
dt[year==analysis_year, .(mean=weighted.mean(tap_pc, w=pop, na.rm=T)), by=iso3] %>% .[order(mean)]
dt[year==analysis_year, .(mean=weighted.mean(hap_pct, w=pop, na.rm=T)), by=iso3] %>% .[order(mean)]
dt[year==analysis_year, .(mean=weighted.mean(tap_paf, w=pop, na.rm=T)), by=iso3] %>% .[order(mean)]
#country pct decreases
dt_d[, .(mean=weighted.mean(tap_pc_dr, w=pop, na.rm=T)), by=iso3] %>% .[order(mean)]
dt_d[, .(mean=weighted.mean(hap_pct_dr, w=pop, na.rm=T)), by=iso3] %>% .[order(mean)]
dt_d[, .(mean=weighted.mean(tap_paf_dr, w=pop, na.rm=T)), by=iso3] %>% .[order(mean)]

#how many districts did hap share stagnate or increase
threshold <- -0.01
dt_d[, .(mean=weighted.mean(hap_pct_d, w=pop, na.rm=T)), by=.(ADM2_CODE, iso3, region_name)] %>% 
  .[, .(count=sum(mean>threshold, na.rm=T), N=.N)]
dt_d[, .(mean=weighted.mean(hap_pct_d, w=pop, na.rm=T)), by=.(ADM2_CODE, iso3, region_name)] %>% 
  .[, .(count=sum(mean>threshold, na.rm=T), N=.N), by=region_name] %>% 
  .[, .(pct=count/N), by=region_name]

#pct breakdown of stagnating or increasing districts by reg
stagnators <-
  dt_d[, .(mean=weighted.mean(hap_pct_d, w=pop, na.rm=T)), by=.(ADM2_CODE, iso3, region_name)] %>% 
    .[mean>threshold, unique(ADM2_CODE)] 
dt_d[ADM2_CODE %in% stagnators, region_name] %>% table %>% prop.table %>% round(2)

#explore stagnators in LAC
dt_d[ADM2_CODE %in% stagnators & tap_pc_d > 0, summary(dfu)]
dt_d[ADM2_CODE %in% stagnators, tap_pc_d > 0] %>% table
dt_d[ADM2_CODE %in% stagnators, tap_pc_d > 0] %>% table %>% prop.table %>% round(2)

#range of hap pct
dt[year==analysis_year, .(range=max(hap_pct, na.rm=T)-min(hap_pct, na.rm=T)), by=iso3] %>% 
  unique %>% 
  .[order(range)]

exploreRange('PAK', shp=adm2, dt=dt, var='hap_pct')

#range of tap dose
dt[year==analysis_year, .(range=max(tap_pc, na.rm=T)-min(tap_pc, na.rm=T)), by=iso3] %>% 
  unique %>% 
  .[order(range)]

exploreRange('TCD', shp=adm2, dt=dt, var='tap_pc')
exploreRange('SOM', shp=adm2, dt=dt, var='tap_pc')

#find the most extreme rook neighbors for HAP share
rooks <- filter(data$admin2, year==analysis_year) %>% 
  dplyr::select(hap_pct, ADM0_CODE, ADM0_NAME, ADM1_CODE, ADM1_NAME, ADM2_CODE, ADM2_NAME) %>%  
  st_join(data_17, st_rook, suffix=c('_og', '_rook')) %>% 
  mutate(range=hap_pct_og-hap_pct_rook) 

summary(rooks$range)
quantile(rooks$range, p=.97, na.rm=T)
filter(rooks, range==max(range, na.rm=T)) #biggest contrast
extreme_rooks <- filter(rooks, range>quantile(rooks$range, p=.99, na.rm=T)) #top 1% of contrasts
as.character(extreme_rooks$ADM0_NAME_og) %>% table #which countries had the most districts with large contrast

#correlation analyses
threshold <- 0 #very weak negative correlation
dt[year==analysis_year, cor < threshold] %>% table %>% prop.table %>% round(2) 

for (reg in unique(dt$region_name)) {
  
  message(reg)
  dt[year==analysis_year & region_name==reg, cor < threshold] %>% table %>% prop.table %>% round(2) %>% print
  
}

threshold <- -.5 #strong negative correlation
for (reg in unique(dt$region_name)) {
  
  message(reg)
  dt[year==analysis_year & region_name==reg, cor < threshold] %>% table %>% prop.table %>% round(2) %>% print
  
}
#***********************************************************************************************************************

# ---ATTRIBUTABLE LRI---------------------------------------------------------------------------------------------------
##analyses of attributable LRI##

#what was the u5 mortality rate of LRI in regions where more than half of LRI was attributed to TAP


#how many children died in districts where more than half of LRI was attributed to TAP or HAP
dt[year==analysis_year & tap_paf > .5, sum(lri_c)]
dt[year==analysis_year & (tap_paf*hap_pct) > .5, sum(lri_c)]


#***********************************************************************************************************************
 
# ---PLOTTING-----------------------------------------------------------------------------------------------------------
##make some plots for figures##
#set up plot data
plot.dt <- dt %>% 
  copy %>% 
  .[year %in% c(2000, 2017)] %>% 
  .[year==2017, year := 2018]

dt_ineq_plot <- unique(dt_ineq[year==2017], by=c('ADM0_CODE', 'year'))
dt_ineq_plot <- dt_ineq_plot[dt_ineq[,do.call(order, .SD), .SDcols = c('super_region_id', 'mean')]]
dt_ineq_plot[, country := factor(iso3, levels=unique(iso3))]

#plot absolute inequality
ggplot(dt_ineq_plot[year==2017], aes(x=country, y=mean, ymax=max, ymin=min, color=super_region_name)) +
  geom_errorbar() +
  geom_point(stat='identity') + 
  scale_color_brewer(palette='Dark2') +
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90)) 
ggsave(filename=file.path(out.dir, 'dfu_inequality_2017.png'),
       width=10, height=6, units='in', dpi=600)

#plot change in HAP share
ggplot(dt_d[year==2017], aes(x=(hap_pct-.5), y=hap_pct_d, color=super_region_name, alpha=log(tap_pc))) + 
  geom_point() + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  #scale_size_area('TAP dose', max_size=3) +
  scale_color_brewer(palette='Dark2') +
  xlim(c(-.6, .6)) +
  ylim(c(-.6, .3)) +
  theme_bw() 
ggsave(filename=file.path(out.dir, 'hap_share_change.png'),
       width=15, height=10, units='in', dpi=900)

#facet plot change in HAP share
ggplot(dt[year %in% c(2000,2017)], aes(x=(hap_pct-.5), y=tap_pc, color=super_region_name)) + 
  geom_point() + 
  #geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  facet_wrap(~year) +
  #scale_size_area('TAP dose', max_size=3) +
  scale_color_brewer(palette='Dark2') +
  xlim(c(-.6, .6)) +
  #ylim(c(-.6, .3))  +
  coord_flip() +
  theme_bw() 
ggsave(filename=file.path(out.dir, 'hap_share_change_facet.png'),
       width=15, height=10, units='in', dpi=900)

plot <- 
  ggplot(plot.dt[iso3 %in% c('ETH', 'KEN', 'IND', 'MNG', 'THA')], 
         aes(x=hap_pct, y=tap_paf*lri*1e3, color=iso3, shape=year %>% as.factor, group=ADM2_CODE)) + 
  geom_point() + 
  geom_line(alpha=.1) +
  geom_vline(xintercept=.5) +
  scale_color_brewer('Country', palette='Dark2') +
  scale_shape_manual('Year', values=c(1, 16)) +
  scale_x_continuous("HAP / TAP Share", limits=c(0, 1)) +
  scale_y_continuous("Rate/1000 of LRI Attributable to TAP", limits=c(0,4)) +
  coord_flip() +
  theme_bw(base_size = 16) +
  theme(
    legend.position = c(.90, .30),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(filename=file.path(out.dir, 'presub_figure_2a.png'),
       width=16, height=8, units='in', dpi=900)

plot <- 
  ggplot(plot.dt[iso3 %in% c('ETH', 'KEN', 'IND', 'MNG', 'THA')], 
         aes(x=hap_pct, y=tap_paf, color=location_name, shape=year %>% as.factor, group=ADM2_CODE)) + 
  geom_point() + 
  geom_line(alpha=.1) +
  geom_vline(xintercept=.5) +
  scale_color_brewer('Country', palette='Dark2') +
  scale_shape_manual('Year', values=c(1, 16)) +
  scale_x_continuous("HAP / TAP Share", limits=c(0, 1)) +
  scale_y_continuous("Population Attributable Fraction of LRI to TAP", limits=c(.2,.6)) +
  coord_flip() +
  theme_bw(base_size = 16) +
  theme(
    legend.position = c(.87, .65),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(filename=file.path(out.dir, 'presub_figure_2b.png'),
       width=12, height=8, units='in', dpi=900)

makeFigure2 <- function(country) {
  
  message('plotting ', country)
  
  plot <- 
    ggplot(plot.dt[iso3 %in% country], 
           aes(x=hap_pct, y=tap_paf, color=iso3, shape=year %>% as.factor, group=ADM2_CODE)) + 
    geom_point() + 
    geom_line(alpha=.1) +
    geom_vline(xintercept=.5) +
    scale_color_brewer('Country', palette='Dark2') +
    scale_shape_manual('Year', values=c(1, 16)) +
    scale_x_continuous("HAP / TAP Share", limits=c(0, 1)) +
    scale_y_continuous("Population Attributable Fraction of LRI to TAP", limits=c(0,1)) +
    ggtitle(country) +
    coord_flip() +
    theme_bw(base_size = 16) +
    theme(
      legend.position = c(.10, .90),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    )
  
  print(plot)
  
  return(NULL)
  
}

pdf(paste0(out.dir, '/presub_figure_2_all_countries.pdf'),
    height=8, width=12)

lapply(unique(plot.dt$iso3), makeFigure2)
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
plot(tmp[1], col = "red", main = "Neighbors: st_intersection")
plot(nc[spare_mtx[[50]], "NAME"], add = TRUE)
plot(nc[50, "AREA"], add = TRUE)

FIRSTdegreeNeighbors <- function(x) { 
  
  # -- use sf functionality and output to sparse matrix format for minimizing footprint 
  first.neighbor <- st_touches(x, x, sparse = TRUE) 
  
  # -- convert results to data.frame (via sparse matrix)
  n.ids <- sapply(first.neighbor, length)
  vals <- unlist(first.neighbor) 
  out <- sparseMatrix(vals, rep(seq_along(n.ids), n.ids))

  out.summ <- summary(out)  # -- this is currently only generating row values, need to map to actual obs [next line]
  data.frame(country = x[out.summ$j,]$NAME, 
             countryid = x[out.summ$j,]$CNTY_ID, 
             firstdegreeneighbors = x[out.summ$i,]$NAME, 
             firstdegreeneighborid = x[out.summ$i,]$CNTY_ID, 
             stringsAsFactors = FALSE)
}

x = st_join(tmp, tmp, join = st_rook)

sel = unlist(st_relate(tmp, tmp, pattern = "F***0****"))
sel = st_touches(tmp, tmp) %>% unlist
plot(st_geometry(tmp))
plot(st_geometry(tmp[sel,]), add = TRUE, col = 'grey')

#***********************************************************************************************************************