# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 04/08/2020
# Purpose: Produce HAP paper results
# source("/homes/jfrostad/_code/lbd/hap/cooking/rover/opportunity.R", echo=T)
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
my_repo         <- '/homes/jfrostad/_code/lbd/hap/'
core_repo       <- '/homes/jfrostad/_code/lbd/hap/'
commondir       <- paste(core_repo, 'mbg_central/share_scripts/common_inputs', sep = '/')

#load packages
package_lib    <- sprintf('%s_code/_lib/pkg',h_root)
## Load libraries and  MBG project functions.
.libPaths(package_lib)
pacman::p_load(data.table, fst, scales, ggplot2, RColorBrewer, sf, viridis, farver, reldist) 
package_list    <- package_list <- fread('/share/geospatial/mbg/common_inputs/package_list.csv') %>% t %>% c

# Use setup.R functions to load common LBD packages and mbg_central "function" scripts
message('Loading in required R packages and MBG functions')
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
run_date <- '2020_05_17_11_40_28' #collab submission
run_date <- '2020_09_01_11_42_52' #first submission
lri_run_date <- '2020_06_11_11_19_26'
var_types <- c('lower', 'mean', 'upper')
new_gbd_estimates <- F

indicator_group <- 'cooking'
indicator <- 'cooking_fuel_solid'

config_par <- 'hap_sp_fine'
cov_par <- 'cooking_VNM'
type <- 'mean'
raked <- F
start_year <- 2000
end_year <- 2019
analysis_year <- 2018
cores <- 10
modeling_shapefile_version <- "2019_09_10"
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path('/ihme/geospatial/mbg/cooking/post', run_date)
global_link_dir <- file.path('/home/j/WORK/11_geospatial/admin_shapefiles', modeling_shapefile_version) #TODO make official
hap.paths <- data.table(admin2=file.path(data.dir, 'new_admin_2_summary.csv'))
hap.paths.d <- data.table(admin2=file.path(data.dir, 'admin_2_delta_summary.csv'))

###Output###
out.dir  <- file.path('/ihme/geospatial/mbg/cooking/maps', run_date) %T>% dir.create(recursive = T)
share.model.dir  <- file.path('/share/geospatial/mbg/input_data/')
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#PE functions#
file.path(my_repo, '_lib', 'post', 'map_fx.R') %>% source

#gbd fx
gbd.shared.function.dir <- '/ihme/cc_resources/libraries/current/r/'
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source
file.path(gbd.shared.function.dir, 'get_age_metadata.R') %>% source
file.path(gbd.shared.function.dir, 'get_draws.R') %>% source
file.path(gbd.shared.function.dir, 'get_outputs.R') %>% source
file.path(gbd.shared.function.dir, 'get_population.R') %>% source

##custom fx##
#TODO move to custom fx scripts

#helper fx to pull/prep the appropriate files from our list of SDG projection objects
prepCasts <- function(id, type, list=sdg_files, id_dt=NA, id_var=NA) {
  
  #format ID var if necessary
  if(nchar(id)==4) id <- as.character(id) #if the ID is a year, format as character
  
  #helper function to extract the correct object
  extractObj <- ifelse(type!='aroc',
                       function(x) list[[x]][[type]][[id]] %>% as.data.table,
                       function(x) list[[x]][[type]] %>% as.data.table ) #aroc only has one object
  
  #do the formatting and extractions
  lapply(1:length(list), extractObj) %>% 
    rbindlist %>% 
    { if(id_var %>% is.na) cbind(., id_dt[id,]) else .[, (id_var) := id] } %>% 
    return
  
}

#spatial functions
#return the max/min districts in a country and the distance between them
exploreRange <- function(explore_country, shp, dt, var, types=var_types, stub=NULL) {

 if (stub %>% is.null) new_vars <- paste(var, types, sep='_')
 else new_vars <- paste(var, types, stub, sep='_')
  
  out <- dt %>% 
    copy %>% 
    setnames(., new_vars, c('var_lower', 'var_mean', 'var_upper')) %>% 
    .[iso3==explore_country & !is.na(var_mean), .(var_lower, var_mean, var_upper, ADM2_NAME)] %>% 
    .[order(var_mean)] %>% 
    .[c(1, nrow(.)), .(var_lower, var_mean, var_upper, ADM2_NAME)]

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

tabulateR <- function(results_dt=results, 
                      lvl='ad2', years=analysis_year, types='HAP', terms='lvl',
                      ind='prev', metrics=var_types, stub='',
                      filter=NULL, #should be provided as list(var=,vals=)
                      cleanup=T,
                      sorted='down') {
  
  #subset DT to requested results
  dt <- results_dt[dimension==lvl & year%in%years & type%in%types & term%in%terms]
  
  #define ind vars in order to remove irrelevant ones
  ind_vars <- names(dt) %>% .[. %like% paste(metrics, collapse='|')]
  irrel_vars <- ind_vars %>% .[!(. %like% ind)]
  rel_vars <- paste(ind, metrics, sep="_")
  mean_var <- rel_vars %>% .[. %like% 'mean']
  name_vars <- names(dt) %>% .[. %like% 'name|NAME']
  
  #remove irrelevant vars and missing rows
  dt[is.na(get(mean_var)), .N] %>% message('missing #', ., ' rows of ', mean_var)
  dt <- dt[!is.na(get(mean_var)), -c(irrel_vars), with=F] 
  setcolorder(dt, neworder=c('year', 'type', rel_vars, name_vars)) #reorder for legibility
  
  #filter if requested
  if(!is.null(filter)) { 
    if(filter$type) dt <- dt[get(filter$var) %in% filter$vals] #inclusive
    else dt <- dt[!(get(filter$var) %in% filter$vals)] #exclusive
  }
  
  #cleanup irrelevant vars if requested
  if(cleanup) dt <- Filter(function(x) !all(is.na(x)), dt)

  #return table, sorted if requested
  if(sorted=='down') setorderv(dt, cols=c('term', mean_var), order = -1) #descending order
  else if(sorted=='up') setorderv(dt, cols=c('term', mean_var)) #ascending order
  
  
  return(dt)
  
}

#helper functions
#TODO add AROC
absChange <- function(x) x-data.table::shift(x,n=1)
relChange <- function(x) (x-data.table::shift(x, n=1))/data.table::shift(x,n=1)
#***********************************************************************************************************************

# ---PREP DATA----------------------------------------------------------------------------------------------------------
##read in and prep datasets for analysis##
## Read config file and save all parameters in memory
config <- set_up_config(repo            = my_repo,
                        indicator_group = indicator_group,
                        indicator       = indicator,
                        config_name     = paste0('/model/configs/config_', config_par),
                        covs_name       = paste0('/model/configs/covs_', cov_par),
                        run_tests       = F,
                        post_est_only   = T,
)

#read in the proper annotations (borders, lakes, mask)
annotations_path <- file.path(out.dir, 'annotations.RDs')
check <- file.exists(annotations_path)
annotations <- ifelse(
  check,
  readRDS(annotations_path),
  load_map_annotations()
)
if(!check) saveRDS(annotations, file=annotations_path)

#read in link_table
global_link_table <- file.path(global_link_dir, "lbd_full_link.rds") %>% readRDS %>% as.data.table
adm_links <- global_link_table[, .(ADM0_NAME, ADM0_CODE, ADM1_NAME, ADM1_CODE, ADM2_NAME, ADM2_CODE)] %>% unique

#create file to crosswalk AD0 to iso3
iso3_map <- dplyr::select(adm2, iso3, ADM0_CODE=gadm_geoid) 
iso3_map$geometry <- NULL
iso3_map <- as.data.table(iso3_map) %>% unique
locs <- merge(locs, iso3_map, by='iso3')
adm_links <- merge(adm_links, locs, by=c('ADM0_CODE'), all.x=T)

#read in shps
stage1 <- st_read('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/shps_by_stage/stage1_ad2_gadm.shp')
stage2 <- st_read('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/shps_by_stage/stage2_ad2_gadm.shp')
adm2 <- rbind(stage1, stage2)

#read in mbg region info
stages <- file.path(j_root, 'WORK/11_geospatial/10_mbg/stage_master_list.csv') %>% fread #read info about stages

#read in the input data
#TODO define using the indicator name
input_dt <- file.path(share.model.dir, "cooking_fuel_solid.csv") %>% fread
ker_dt <- file.path(share.model.dir, "cooking_fuel_kerosene.csv") %>% fread

#read in results
results <- file.path(data.dir, 'all_summary.fst') %>% read_fst(as.data.table=T)
dt <- results[dimension=='ad2' & term=='lvl'] %>% Filter(function(x) !all(is.na(x)), .)
dt_d <- results[dimension=='ad2' & term%like%'change'] %>% Filter(function(x) !all(is.na(x)), .)

#merge sr region names/IDs
locs <- get_location_metadata(location_set_id = 35, gbd_round_id = 6) %>% 
  .[, .(iso3=ihme_loc_id, location_name, super_region_id, super_region_name, region_id, region_name)] #subset to relevant columns

# #merge sr region names/IDs
# dt <- merge(dt, locs, by='ADM0_CODE', all.x=T)
# dt <- merge(dt, adm_links, by=c('ADM0_CODE', 'ADM2_CODE'))

#read in results for lri/children
# dt_d <- file.path(data.dir, 'admin_2_delta_summary.csv') %>% fread
# dt_d <- merge(dt_d, locs, by='ADM0_CODE')

#read in input data and prepare it for mapping
data <- load_map_results(indicator, indicator_group, run_date, raked, 
                   year_list=c(2000:2018),
                   custom_path = list('admin2'=dt),
                   geo_levels=c('admin2'),
                   cores=cores)
data_d <-
  load_map_results(indicator, indicator_group, run_date, raked, 
                   year_list=2018,
                   custom_path = list('admin2'=dt_d),
                   geo_levels=c('admin2'),
                   cores=cores)
#define extent of map
zoom.afr <- data.table(x1=-10, x2=50, y1=-20, y2=40)
zoom.global <- data.table(x1=-120, x2=150, y1=-40, y2=55)
#***********************************************************************************************************************

# ---LOAD GBD RESULTS---------------------------------------------------------------------------------------------------
#also pull the GBD2019 attributable results for comparison
#build the connector in order to define which locs to pull
# add connector object between ADM codes and GBD loc ids
connector <-
  data.table(get_gbd_locs(rake_subnational = T,
                          reg = "all",
                          shapefile_version = raking_shapefile_version))

ad0_dt <- results[dimension=='ad0' & term=='lvl' & type=='HAP'] %>% 
  Filter(function(x) !all(is.na(x)), .)

ad1_dt <- results[dimension=='ad1' & term=='lvl' & type=='HAP'] %>% 
  Filter(function(x) !all(is.na(x)), .)

#merge the connector, using the rake levels for most detailed identifier
#then collapse results to most detailed level
gbd_dt <-  
  list(
    merge(ad1_dt, 
          connector[rak_level==1, .(location_id, ADM0_CODE, ADM1_CODE, rak_level)], 
          by=c('ADM0_CODE','ADM1_CODE')),
    merge(ad0_dt, 
          connector[rak_level==0, .(location_id, ADM0_CODE, rak_level)], 
          by='ADM0_CODE')
  ) %>% rbindlist(use.names=T, fill=T)

#reload estimates from the central db if they have changed
if (new_gbd_estimates) {

  #cause=322(LRI)//rei=87(HAP)
  loc_ids <- unique(gbd_dt$location_id)
  
  hap <- get_outputs("rei", rei_id = 87, cause_id = 322, metric_id = 1, measure_id = 1, 
                     location_id=loc_ids,
                     year_id = c(2000, 2010, 2019),
                     sex_id = 3, age_group_id=1, gbd_round_id = 6, 
                     compare_version_id = 7244, decomp_step = "step4")
  
  setnames(hap, c('val', 'upper', 'lower'), c('gbd_atr_count_mean', 'gbd_atr_count_upper', 'gbd_atr_count_lower'))
  
  lri <- get_outputs("cause", cause_id = 322, metric_id = 1, measure_id = 1, 
                     location_id=loc_ids,
                     year_id = c(2000, 2010, 2019),
                     sex_id = 3, age_group_id=1, gbd_round_id = 6, 
                     compare_version_id = 7244, decomp_step = "step5")

  setnames(lri, c('val', 'upper', 'lower'), c('gbd_count_mean', 'gbd_count_upper', 'gbd_count_lower'))
  
  #merge
  gbd <- merge(hap, lri, by=names(hap) %>% .[. %in% names(lri)])
  
  # gbd <- get_draws(
  #   gbd_id_type=c('cause_id', 'rei_id'),
  #   gbd_id=c(322, 87),
  #   source='burdenator', 
  #   decomp_step="step5",
  #   location_id=loc_ids, 
  #   sex_id=3, age_group_id=1,
  #   metric_id=1, measure_id=1,
  #   num_workers=8,
  #   year_id=c(2000, 2010, 2018), 
  #   gbd_round_id=6, 
  #   status='best'
  # )
  # 
  setnames(gbd, 'year_id', 'year')
  
  # #summarize
  # draw.cols <- paste0('draw_', 0:999) #i hate zero indexing
  # gbd[, lower := apply(.SD, 1, quantile, c(.025)), .SDcols=draw.cols]
  # gbd[, mean := apply(.SD, 1, mean), .SDcols=draw.cols]
  # gbd[, upper := apply(.SD, 1, quantile, c(.975)), .SDcols=draw.cols]
  # gbd[, (draw.cols) := NULL] #no longer need
  
  write.csv(gbd, 
            file=file.path('/share/geospatial/jfrostad', indicator_group, 'data', 
                           paste0('gbd_2019_best_', indicator, '_attrib_mortality.csv')),
            row.names=F)
  
} else {

  gbd <- file.path('/share/geospatial/jfrostad', indicator_group, 'data', 
                   paste0('gbd_2019_best_', indicator, '_attrib_mortality.csv')) %T>% 
    message('reading GBD best estimates from this path\n', .) %>% 
    fread 
  
}

#merge ad2 results and GBD results using connector
gbd_dt <- merge(gbd_dt, gbd, by=c('location_id', 'year'))
#***********************************************************************************************************************

# ---GENERAL METRICS----------------------------------------------------------------------------------------------------
#number of LMICs
stages[Stage %in% c('1', '2a', '2b'), uniqueN(iso3)]

#number of countries with data
uniqueN(input_dt$ihme_loc_id)

#number of surveys
uniqueN(input_dt$nid)

#number of people
sum(input_dt$N, na.rm=T)

#number of points/polys
table(input_dt$point)

#explore kerosene data
ker_dt <- merge(ker_dt, locs, by.x='ihme_loc_id', by.y='iso3')
ker_dt[, ad0_kerosene := weighted.mean(cooking_fuel_kerosene, w=N), by=.(ihme_loc_id, year)]
ggplot(ker_dt, aes(x=year, y=ad0_kerosene, color=super_region_name)) +
  geom_hline(yintercept=.1, color='grey10', linetype='dashed') +
  geom_point() +
  geom_smooth() +
  scale_color_brewer(type='qual', palette = 'Paired') +
  facet_wrap(~ihme_loc_id) +
  theme_minimal()
#***********************************************************************************************************************

# ---SPATIAL VARIATION--------------------------------------------------------------------------------------------------
##2017 patterns##
#TODO verify that popweighting shoudl be done with u5 pop, if not switch pop vars here
#country level
results[['ad2']][year==analysis_year & type=='HAP', lapply(.SD, function(x) sum(pop_total*x, na.rm=T)), 
                 .SDcols=paste('prev', var_types, sep='_')] #pop exposed to dfu 
results[['ad2']][year==analysis_year & type=='HAP', lapply(.SD, function(x) sum(pop*x, na.rm=T)), 
                 .SDcols=paste('prev', var_types, sep='_')] #under 5 exposed to dfu in
results[['ad2']][year==analysis_year & type=='HAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
                 .SDcols=paste('prev', var_types, sep='_')] #avg proportion
# results[['ad0']][year==analysis_year & type=='HAP' & super_region_name=='Sub-Saharan Africa', 
#                  lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
#                  .SDcols=c('prev_lower', 'prev_mean', 'prev_upper')] #avg proportion in SSA
# dt_ad0[year==analysis_year, .(mean=weighted.mean(dfu, w=pop)), by=region_name] %>% .[order(mean)]
# dt_ad0[year==analysis_year & super_region_name=='Sub-Saharan Africa', .(mean=weighted.mean(dfu_mean, w=pop)), by=iso3] %>% .[order(mean)]
# dt_ad0[year==analysis_year & super_region_name=='Latin America and Caribbean', .(mean=weighted.mean(dfu, w=pop)), by=iso3] %>% .[order(mean)]

#district level
#counts based on threshold
threshold <- .95

#how many districts were about .95 in 2018
dt[year==analysis_year & type=='HAP' & term=='lvl', lapply(.SD, function(x) sum(x>threshold, na.rm=T)/.N), 
   .SDcols=paste0('prev_', var_types)]

#worst superregion
results[['ad2']][year==analysis_year & type=='HAP',
                 .(mean=weighted.mean(prev_mean, w=pop)), by=.(ADM2_CODE, iso3, super_region_name)] %>% 
  .[, .(count=sum(mean>threshold, na.rm=T), N=.N), by=super_region_name] %>% 
  .[, .(count, N, pct=count/N), by=super_region_name]

#worst region
results[['ad2']][year==analysis_year & type=='HAP',
                 .(lower=weighted.mean(prev_lower, w=pop), 
                   mean=weighted.mean(prev_mean, w=pop),
                   upper=weighted.mean(prev_upper, w=pop)), by=.(ADM2_CODE, iso3, region_name)] %>% 
  .[, .(count_lower=sum(lower>threshold, na.rm=T), 
        count_mean=sum(mean>threshold, na.rm=T),
        count_upper=sum(upper>threshold, na.rm=T), N=.N), by=region_name] %>% 
  .[, .(count_lower, count_mean, count_upper, N, pct=count_mean/N), by=region_name]

#worst country
results[['ad2']][year==analysis_year & type=='HAP',
                 .(lower=weighted.mean(prev_lower, w=pop), 
                   mean=weighted.mean(prev_mean, w=pop),
                   upper=weighted.mean(prev_upper, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count_lower=sum(lower>threshold, na.rm=T), 
        count_mean=sum(mean>threshold, na.rm=T),
        count_upper=sum(upper>threshold, na.rm=T), N=.N), by=iso3] %>% 
  .[, .(count_lower, count_mean, count_upper, N, pct=count_mean/N), by=iso3] %>% 
  .[order(pct)]

#worst country outside of SSA
results[['ad2']][year==analysis_year & type=='HAP' & super_region_id!=166,
                 .(lower=weighted.mean(prev_lower, w=pop), 
                   mean=weighted.mean(prev_mean, w=pop),
                   upper=weighted.mean(prev_upper, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count_lower=sum(lower>threshold, na.rm=T), 
        count_mean=sum(mean>threshold, na.rm=T),
        count_upper=sum(upper>threshold, na.rm=T), N=.N), by=iso3] %>% 
  .[, .(count_lower, count_mean, count_upper, N, pct=count_mean/N), by=iso3] %>% 
  .[order(pct)]

#worst country outside of SSA
results[['ad2']][year==analysis_year & type=='HAP' & super_region_id!=166,
                 .(lower=weighted.mean(prev_lower, w=pop), 
                   mean=weighted.mean(prev_mean, w=pop),
                   upper=weighted.mean(prev_upper, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count_lower=sum(lower>threshold, na.rm=T), 
        count_mean=sum(mean>threshold, na.rm=T),
        count_upper=sum(upper>threshold, na.rm=T), N=.N), by=iso3] %>% 
  .[, .(count_lower, count_mean, count_upper, N, pct=count_mean/N), by=iso3] %>% 
  .[order(pct)]

#worst country in Americas
results[['ad2']][year==analysis_year & type=='HAP' & super_region_id==103,
                 .(lower=weighted.mean(prev_lower, w=pop), 
                   mean=weighted.mean(prev_mean, w=pop),
                   upper=weighted.mean(prev_upper, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count_lower=sum(lower>threshold, na.rm=T), 
        count_mean=sum(mean>threshold, na.rm=T),
        count_upper=sum(upper>threshold, na.rm=T), N=.N), by=iso3] %>% 
  .[, .(count_lower, count_mean, count_upper, N, pct=count_mean/N), by=iso3] %>% 
  .[order(pct)]

#best country
results[['ad2']][year==analysis_year & type=='HAP',
                 .(lower=weighted.mean(prev_lower, w=pop), 
                   mean=weighted.mean(prev_mean, w=pop),
                   upper=weighted.mean(prev_upper, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count_lower=sum(lower<(1-threshold), na.rm=T), 
        count_mean=sum(mean<(1-threshold), na.rm=T),
        count_upper=sum(upper<(1-threshold), na.rm=T), N=.N), by=iso3] %>% 
  .[, .(count_lower, count_mean, count_upper, N, pct=count_mean/N), by=iso3] %>% 
  .[order(pct)]

#best country in Americas
results[['ad2']][year==analysis_year & type=='HAP' & super_region_id==103,
                 .(lower=weighted.mean(prev_lower, w=pop), 
                   mean=weighted.mean(prev_mean, w=pop),
                   upper=weighted.mean(prev_upper, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count_lower=sum(lower<(1-threshold), na.rm=T), 
        count_mean=sum(mean<(1-threshold), na.rm=T),
        count_upper=sum(upper<(1-threshold), na.rm=T), N=.N), by=iso3] %>% 
  .[, .(count_lower, count_mean, count_upper, N, pct=count_mean/N), by=iso3] %>% 
  .[order(pct)]

#best country in SSA
results[['ad2']][year==analysis_year & type=='HAP' & super_region_id==166,
                 .(lower=weighted.mean(prev_lower, w=pop), 
                   mean=weighted.mean(prev_mean, w=pop),
                   upper=weighted.mean(prev_upper, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
  .[, .(count_lower=sum(lower<(1-threshold), na.rm=T), 
        count_mean=sum(mean<(1-threshold), na.rm=T),
        count_upper=sum(upper<(1-threshold), na.rm=T), N=.N), by=iso3] %>% 
  .[, .(count_lower, count_mean, count_upper, N, pct=count_mean/N), by=iso3] %>% 
  .[order(pct)]

# dt[year==analysis_year & super_region_name=='Sub-Saharan Africa', .(mean=weighted.mean(dfu, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
#   .[, .(count=sum(mean>threshold, na.rm=T), N=.N), by=iso3] %>% 
#   .[, .(pct=count/N), by=iso3] %>% 
#   .[order(pct)]
# dt[year==analysis_year & super_region_name!='Sub-Saharan Africa', .(mean=weighted.mean(dfu, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
#   .[, .(count=sum(mean>threshold, na.rm=T), N=.N), by=iso3] %>% 
#   .[, .(pct=count/N), by=iso3] %>% 
#   .[order(pct)]
# 
# dt[year==analysis_year & super_region_name=='Sub-Saharan Africa', .(mean=weighted.mean(dfu, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
#   .[, .(count=sum(mean<(1-threshold), na.rm=T), N=.N), by=iso3] %>% 
#   .[, .(pct=count/N), by=iso3] %>% 
#   .[order(pct)]
# dt[year==analysis_year & super_region_name!='Sub-Saharan Africa', .(mean=weighted.mean(dfu, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
#   .[, .(count=sum(mean<(1-threshold), na.rm=T), N=.N), by=iso3] %>% 
#   .[, .(pct=count/N), by=iso3] %>% 
#   .[order(pct)]

#counts of exposed
results[['ad2']][year==analysis_year & type=='HAP', .(count_lower=sum(prev_lower*pop_total, na.rm=T),
                                                      count=sum(prev_mean*pop_total, na.rm=T),
                                                      count_upper=sum(prev_upper*pop_total, na.rm=T)
                                                      ), by=.(ADM2_CODE, ADM2_NAME, iso3)] %>% 
  .[order(count)]
#***********************************************************************************************************************

# ---INEQUALITY---------------------------------------------------------------------------------------------------------
##produce metrics of inequality for 2017##
#calculate GINI/MAD at country level
dt_ineq <- results[['ad2']][year==analysis_year & type=='HAP',
              .(iso3, year, ADM0_CODE, ADM2_CODE, ADM2_NAME, prev_mean, 
                super_region_id, super_region_name, region_id, region_name, pop_total)]
dt_ineq[, gini := gini(prev_mean, weights = pop_total), by=.(iso3, year)]
dt_ineq[, mean := weighted.mean(prev_mean, weights = pop_total), by=.(iso3, year)]
dt_ineq[, mad := mad(prev_mean, center = mean(prev_mean)), by=.(iso3, year)]
dt_ineq[, mean := mean(prev_mean, na.rm=T), by=.(iso3, year)]
dt_ineq[, max := max(prev_mean, na.rm=T), by=.(iso3, year)]
dt_ineq[, min := min(prev_mean, na.rm=T), by=.(iso3, year)]
dt_ineq[, range := max-min]

#range results
dt_ineq[year==analysis_year, .(range=max-min), by=iso3] %>% 
  unique %>% 
  .[order(range)]

exploreRange('SSD', shp=adm2, dt=results[['ad2']][year==analysis_year & type=='HAP'], var='prev')
exploreRange('EGY', shp=adm2, dt=results[['ad2']][year==analysis_year & type=='HAP'], var='prev')
exploreRange('HND', shp=adm2, dt=results[['ad2']][year==analysis_year & type=='HAP'], var='prev')
exploreRange('NGA', shp=adm2, dt=results[['ad2']][year==analysis_year & type=='HAP'], var='prev')

#find the most extreme rook neighbors
data_18 <- filter(data$admin2, year==analysis_year & type=='HAP') 
rooks <- data_18 %>%  #TODO could be written more eloquently
  dplyr::select(prev_mean, ADM0_CODE, ADM0_NAME, ADM1_CODE, ADM1_NAME, ADM2_CODE, ADM2_NAME, year) %>%  
  st_join(data_18, st_rook, suffix=c('_og', '_rook')) %>% 
  mutate(range=prev_mean_og-prev_mean_rook) 

summary(rooks$range)
filter(rooks, range==max(range, na.rm=T)) #biggest contrast
extreme_rook_limit <- quantile(rooks$range, p=.99, na.rm=T) #top 1% of contrasts
extreme_rooks <- filter(rooks, range > extreme_rook_limit) #which districts meet top 1% of contrasts
as.character(extreme_rooks$ADM0_NAME_og) %>% table #which countries had the most districts with large contrast

#find which countries have the highest percentage of their districts with at least one extreme rook
extreme_rooks %>% 
  as.data.table %>% 
  .[,geometry := NULL] %>% #no longer relevant in dt format 
  .[, extreme_count := .N, by=ADM0_NAME_og] %>% 
  unique(., by='ADM0_NAME_og') %>% 
  merge(., results[dimension=='ad2' & year==analysis_year & type=='HAP' & term=='lvl', .N, by=ADM0_NAME], 
        by.x='ADM0_NAME_og', by.y='ADM0_NAME') %>% 
  .[, pct := extreme_count/N] %>% 
  .[order(pct)] %>% 
  tail(n=20)

distinct(extreme_rooks, ADM2_NAME_og, .keep_all = T) %>% 
  count('ADM0_NAME_og') %>% 
  merge(., count(data_18, 'ADM0_NAME'), by.y='ADM0_NAME', by.x='ADM0_NAME_og') %>% 
  mutate(pct=freq.x/freq.y) %>% 
  arrange(pct)

#GINI results
summary(dt_ineq$gini)
dt_ineq[year==2000 & gini>mean(gini, na.rm=T), uniqueN(iso3)]
dt_ineq[year==analysis_year & gini>mean(gini, na.rm=T), uniqueN(iso3)]
dt_ineq[gini>mean(gini, na.rm=T), table(iso3, year)]

#AID results
#note that AID = gini * 2 * mean
aid.dt <-
na.omit(dt_ineq, cols='dfu_mean') %>% 
.[, .(aid=gini(dfu_mean, weights=pop_total) * 2 * weighted.mean(dfu_mean, weights = pop_total, na.rm=T),
      pop=sum(pop_total, na.rm=T)), 
  by=.(super_region_id, region_id, iso3, year)] %>% 
  unique(by=c('iso3', 'year')) %>% 
  .[order(aid),] %>% 
  .[year==min(year), aid_start := aid] %>% 
  .[, aid_start := min(aid_start, na.rm=T), by=.(iso3)] %>%
  .[, aid_d := (aid-aid_start)] %>% 
  .[, aid_dr := aid_d/aid_start] %>% 
  .[year==max(year), label := iso3]

#average change in AID
aid.dt[year==max(year)] %>% .[,weighted.mean(aid_dr, weights=pop)]

#regional changes
aid.dt[year==max(year) & region_id==159] #south asia
aid.dt[year==max(year) & region_id==167] #central sub-saharan africa
#***********************************************************************************************************************

# ---TEMPORAL VARIATION-------------------------------------------------------------------------------------------------
#global results
#abs/rel change in prevalence
tabulateR(lvl='global', types='HAP', ind='prev', terms=c('change', 'change_rate', 'lvl'), years=c(2000, 2018))

#absolute decrease in exposure
tabulateR(lvl='global', types='HAP', ind='prev', terms=c('lvl'), years=c(2000, 2018)) %>% 
  .[, lapply(.SD, function(x) x * pop_total), .SDcols=paste('prev', var_types, sep='_'), by=year] %T>% 
  print %>% 
  .[, lapply(.SD, absChange), .SDcols=paste('prev', var_types, sep='_')] %>% .[2]

#absolute decrease in proportion at global level
tabulateR(lvl='global', types='HAP', ind='share', terms=c('change', 'change_rate', 'lvl'), years=c(2000, 2018))

#regional/ad0 results
tabulateR(lvl='region', types='HAP', ind='prev', terms=c('change', 'change_rate'))
tabulateR(lvl='ad0', types='HAP', ind='prev', terms=c('change', 'change_rate'))

#ad2 results
tabulateR(lvl='ad2', types='HAP', ind='prev', terms=c('change'))

#regional change 
dt_d[year==analysis_year & type=='HAP' & super_region_name=='Sub-Saharan Africa',
     lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)),
     .SDcols=paste('prev', var_types, 'dr', sep='_')] #avg proportion change

dt_d[year==analysis_year & type=='HAP',
     lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)),
     .SDcols=paste('prev', var_types, 'dr', sep='_'), 
     by=super_region_name] %>% .[order(prev_mean_dr)]

dt_d[year==analysis_year & type=='HAP',
     lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)),
     .SDcols=paste('prev', var_types, 'dr', sep='_'), 
     by=region_name] %>% .[order(prev_mean_dr)]

dt_d[year==analysis_year & type=='HAP',
     lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)),
     .SDcols=paste('prev', var_types, 'd', sep='_'), 
     by=iso3] %>% .[order(prev_mean_d)]

# dt_d[year==analysis_year & region_name %like% "Andean|Tropical", .(mean=weighted.mean(dfu_dr, w=pop, na.rm=T))]
# dt_d[year==analysis_year & super_region_name=='Sub-Saharan Africa', 
#      .(mean=weighted.mean(dfu_dr, w=pop, na.rm=T)), by=iso3] %>% .[order(mean)]
# dt_d[year==analysis_year & super_region_name!='Sub-Saharan Africa', 
#      .(mean=weighted.mean(dfu_dr, w=pop, na.rm=T)), by=iso3] %>% .[order(mean)]
# dt_d[year==analysis_year & super_region_name!='Sub-Saharan Africa', 
#      .(mean=weighted.mean(dfu_d, w=pop, na.rm=T)), by=iso3] %>% .[order(mean)]

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
biggest_change <-
dt_d[type=='HAP'] %>%  
  .[!is.na(prev_mean_d)] %>% 
  .[order(prev_mean_d)] %>% 
  .[1, ADM2_CODE]
results[['ad2']][ADM2_CODE==biggest_change & type=='HAP' & year %in%c(start_year, end_year)]

dt_d[!is.na(dfu_d) & dfu_d >= 0] # number of districts making no progress
nrow(dt_d[!is.na(dfu_d) & dfu_d >= 0]) / nrow(dt_d)

#what percent of districts stagnated?
#counts based on threshold of improving less than 1%
threshold <- -.01
q75_dfu <- results[['ad0']][year==start_year & type=='HAP',  quantile(prev_mean, p=.75, na.rm=T)]
q75_countries <-  results[['ad0']][year==start_year & type=='HAP' & prev_mean>=q75_dfu, unique(iso3)]

dt_d[year==analysis_year & iso3 %in% q75_countries & type=='HAP', lapply(.SD, function(x) sum(x >= threshold)), 
     .SDcols=paste('prev', var_types, 'd', sep='_')]
dt_d[year==analysis_year & iso3 %in% q75_countries & type=='HAP', lapply(.SD, function(x) sum(x >= threshold)/.N), 
     .SDcols=paste('prev', var_types, 'd', sep='_')]

dt_d[year==analysis_year & iso3 %in% q75_countries & type=='HAP'] %>% 
  .[, .(count=sum(prev_mean_d>threshold, na.rm=T), N=.N), by=iso3] %>% 
  .[, .(pct=count/N), by=iso3] %>% 
  .[order(pct)]

dt_d[year==analysis_year & iso3 %in% q75_countries & type=='HAP', lapply(.SD, function(x) sum(x >= threshold)/.N), 
     .SDcols=paste('prev', var_types, 'd', sep='_'), by=iso3] %>% 
  .[order(prev_mean_d)]
  
# dt_d[year==analysis_year & super_region_name!='Sub-Saharan Africa', .(mean=weighted.mean(dfu_dr, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
#   .[, .(count=sum(mean>threshold, na.rm=T), N=.N), by=iso3] %>% 
#   .[, .(pct=count/N), by=iso3] %>% 
#   .[order(pct)]
# 
# dt_d[year==analysis_year & super_region_name=='Sub-Saharan Africa', .(mean=weighted.mean(dfu_dr, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
#   .[, .(count=sum(mean<(1-threshold), na.rm=T), N=.N), by=iso3] %>% 
#   .[, .(pct=count/N), by=iso3] %>% 
#   .[order(pct)]
# dt_d[year==analysis_year & super_region_name!='Sub-Saharan Africa', .(mean=weighted.mean(dfu_dr, w=pop)), by=.(ADM2_CODE, iso3)] %>% 
#   .[, .(count=sum(mean<(1-threshold), na.rm=T), N=.N), by=iso3] %>% 
#   .[, .(pct=count/N), by=iso3] %>% 
#   .[order(pct)]

#range results
# dt_d[year==analysis_year, .(range=max(dfu_d, na.rm=T)-min(dfu_d, na.rm=T)), by=iso3] %>% 
#   unique %>% 
#   .[order(range)]

#explore the range to find inequality
exploreRange('MEX', shp=adm2, 
             dt=merge(dt_d[year==analysis_year & type=='HAP'], adm_links[, .(ADM2_CODE, ADM2_NAME)], by='ADM2_CODE'),
             var='prev', stub='d')

#explore the range to find equality
#select countries that improved by more than 10%
# threshold <- -.10
# progress_countries <- dt_d[year==analysis_year, .(mean=weighted.mean(dfu_d, w=pop, na.rm=T)), by=iso3] %>% 
#   .[mean<threshold, unique(iso3)]
# 
# dt_d[year==analysis_year & iso3 %in% progress_countries, .(range=max(dfu_d, na.rm=T)-min(dfu_d, na.rm=T)), by=iso3] %>% 
#   unique %>% 
#   .[order(range)]
# 
# exploreRange('BLZ', shp=adm2, dt=dt_d, var='dfu_d')
#***********************************************************************************************************************

# ---SDG PROJECTIONS----------------------------------------------------------------------------------------------------
#SDG projection probabilities
#append the ADM2 files
sdg_files <-
  file.path(data.dir, 'sdg_projections') %>% list.files(pattern='admin_2', full.names = T) %>% 
  lapply(., readRDS)

#extract goal obj to index over
goals <- lapply(1:length(sdg_files), function(x) sdg_files[[x]]$goals) %>% rbindlist %>% unique

#create a dt with all probabilities
probs <- lapply(1:nrow(goals), prepCasts, type='probs', id_dt=goals) %>% 
  rbindlist %>% 
  setnames(c('target_year', 'spatial_idx'), c('year', 'ADM2_CODE')) %>% 
  merge(., adm_links[, .(ADM2_CODE, ADM0_CODE)], by='ADM2_CODE')  %>% 
  merge(., locs, by='ADM0_CODE') %>% 
  merge(., results[dimension=='ad2' & year==analysis_year & type=='HAP' & term=='lvl', pop_total, by=ADM2_CODE],
        by='ADM2_CODE')

#create a dt with all projections
projs <- 
  lapply(c(2018, seq(2020, 2030, 5)), prepCasts, type='proj', id_var='year') %>% 
  rbindlist %>% 
  setnames('spatial_idx', 'ADM2_CODE') %>% 
  melt(measure = patterns("V"), variable.name = "draw", value.name='sev')

#create a dt with aroc and combine
projs <- prepCasts(2018, type='aroc', id_var='year') %>% 
  melt(measure = patterns("V"), variable.name = "draw", value.name='aroc') %>% 
  merge(., projs, by=c('ADM2_CODE', 'year', 'draw'), all.y=T) %>% 
  merge(., adm_links[, .(ADM2_CODE, ADM0_CODE)], by='ADM2_CODE') 

#generate mean/ci
#cols <- paste0('V', 2:51) #TODO, weird why are they numbered like this..?
cols <- c('aroc', 'sev')
setkey(projs, year, ADM2_CODE)
projs[, paste0(cols, '_mean') := lapply(.SD, mean, na.rm=T), .SDcols=cols, by=key(projs)]
projs[, paste0(cols, '_lower') := lapply(.SD, quantile, probs=.025, na.rm=T), .SDcols=cols, by=key(projs)]
projs[, paste0(cols, '_upper') := lapply(.SD, quantile, probs=.975, na.rm=T), .SDcols=cols, by=key(projs)]
projs <- unique(projs, by=key(projs)) %>% 
  .[, c(cols, 'draw') := NULL]

#calculate relative uncertainty of SEV
projs[, sev_rel_uncertainty := (sev_upper-sev_lower)/2/sev_mean]
projs[sev_rel_uncertainty>1, sev_rel_uncertainty := 1] #cap at 1

#district level results
target_threshold <- .05
prob_threshold <- .95

#how many countries will succeed
ad0_probs <-
probs[target==target_threshold & year==2030, 
      .(prob=weighted.mean(absolute_goal_prob, w=pop_total), 
        pop=sum(pop_total, na.rm=T)), by=.(super_region_id, region_id, iso3)] %>% 
  .[, global_pop := sum(pop, na.rm=T)]

ad0_probs[, .(fail=sum(prob>=prob_threshold, na.rm=T), success=sum(prob<=(1-prob_threshold), na.rm=T))]
ad0_probs[prob>=prob_threshold, .(regs=uniqueN(region_id), pop=sum(pop, na.rm=T), global_pop=mean(global_pop))] %>% 
  .[, pop_share := pop/global_pop] %>% print
ad0_probs[prob<=(1-prob_threshold), .(regs=uniqueN(region_id), pop=sum(pop, na.rm=T), global_pop=mean(global_pop))] %>% 
  .[, pop_share := pop/global_pop] %>% print

#what percent of 

#how many districts have met in 2018
probs[target==target_threshold & year==2018] %>% 
  .[, .(count=sum(absolute_goal_prob>=prob_threshold, na.rm=T), N=.N)] %>% 
  .[, .(count, N, pct=count/N)]
#what share of pop
probs[target==target_threshold & year==2018 & absolute_goal_prob>=prob_threshold, 
      sum(pop_total, na.rm=T)] / probs[target==target_threshold & year==2018, sum(pop_total, na.rm=T)]

#what share of unmet districts will meet between 2018-2030
unmet_districts <- probs[target==target_threshold & year==2018 & absolute_goal_prob<prob_threshold, unique(ADM2_CODE)]
probs[target==target_threshold & year==2030 & ADM2_CODE%in%unmet_districts] %>% 
  .[, .(count=sum(absolute_goal_prob>=prob_threshold, na.rm=T), N=.N)] %>% 
  .[, .(count, N, pct=count/N)]
#what share of pop
probs[target==target_threshold & year==2030 & absolute_goal_prob>=prob_threshold & ADM2_CODE%in%unmet_districts, 
      sum(pop_total, na.rm=T)] / probs[target==target_threshold & year==2018, sum(pop_total, na.rm=T)]

#superregional breakdown
probs[target==target_threshold & year==2030 & ADM2_CODE%in%unmet_districts] %>% 
  .[, .(count=sum(absolute_goal_prob>=prob_threshold, na.rm=T), N=.N), by=super_region_name] %>% 
  .[, .(count, N, pct=count/N), by=super_region_name] %>% 
  .[order(pct)]

#regional breakdown
probs[target==target_threshold & year==2030 & ADM2_CODE%in%unmet_districts] %>% 
  .[, .(count=sum(absolute_goal_prob>=prob_threshold, na.rm=T), N=.N), by=region_name] %>% 
  .[, .(count, N, pct=count/N), by=region_name] %>% 
  .[order(pct)]

#country breakdown
probs[target==target_threshold & year==2030 & ADM2_CODE%in%unmet_districts] %>% 
  .[, .(count=sum(absolute_goal_prob>=prob_threshold, na.rm=T), N=.N), by=ADM0_NAME] %>% 
  .[, .(count, N, pct=count/N), by=ADM0_NAME] %>% 
  .[order(pct)]

#country breakdown for SSA
probs[target==target_threshold & year==2030 & ADM2_CODE%in%unmet_districts & region_name %like% 'Saharan'] %>% 
  .[, .(count=sum(absolute_goal_prob>=prob_threshold, na.rm=T), N=.N), by=ADM0_NAME] %>% 
  .[, .(count, N, pct=count/N), by=ADM0_NAME] %>% 
  .[order(pct)]

#country failure breakdown for 2030
probs[target==target_threshold & year==2030] %>% 
  .[, .(count=sum(absolute_goal_prob<=(1-prob_threshold), na.rm=T), N=.N), by=ADM0_NAME] %>% 
  .[, .(count, N, pct=count/N), by=ADM0_NAME] %>% 
  .[order(pct)]

#country failure breakdown for 2030 (ESA/Oceania)
probs[target==target_threshold & year==2030 & region_id %in% c(174, 21)] %>% 
  .[, .(count=sum(absolute_goal_prob<=(1-prob_threshold), na.rm=T), N=.N), by=ADM0_NAME] %>% 
  .[, .(count, N, pct=count/N, resid=N-count), by=ADM0_NAME] %>% 
  .[order(pct)]

#country with the largest divide
probs[target==target_threshold & year==2030] %>% 
  .[, .(count_fail=sum(absolute_goal_prob<=(1-prob_threshold), na.rm=T), 
        count_success=sum(absolute_goal_prob>=prob_threshold, na.rm=T),
        N=.N), by=ADM0_NAME] %>% 
  .[, ratio := count_fail/count_success] %>% 
  .[count_fail!=0&count_success!=0] %>% 
  .[order(ratio)]
#***********************************************************************************************************************

# ---AIR POLLUTION------------------------------------------------------------------------------------------------------
##analyze relationship to AAP; TAP; HAP_SHARE##

##global levels/change
#current tap_pc/tap_paf global avg
tabulateR(lvl='global', types='TAP', ind='pm_pc', terms=c('lvl', 'change', 'change_rate'))  #highest TAP
tabulateR(lvl='global', types='TAP', ind='paf', terms=c('lvl', 'change', 'change_rate')) #highest TAP PAF
tabulateR(lvl='global', types='HAP', ind='share', terms=c('lvl', 'change', 'change_rate')) #highest HAP share

tabulateR(lvl='super_region', types='TAP', ind='pm_pc') #highest TAP
tabulateR(lvl='super_region', types='TAP', ind='paf', years=c(start_year, end_year)) #highest TAP PAF
tabulateR(lvl='super_region', types='HAP', ind='share', terms=c('lvl', 'change', 'change_rate')) #highest HAP share

tabulateR(lvl='region', types='TAP', ind='pm_pc') #highest TAP
tabulateR(lvl='region', types='TAP', ind='paf', years=c(start_year, end_year)) #highest TAP PAF
tabulateR(lvl='region', types='HAP', ind='share', terms=c('lvl', 'change')) #highest HAP share
tabulateR(lvl='region', types='AAP', ind='pm_pc', terms=c('lvl', 'change'), years=c(start_year, end_year)) %>% 
  .[region_id==159]#change in AAP dose in South Asia

#pct decreases at global level
dt_d[type=='TAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
              .SDcols=apply(expand.grid(c('pm_pc', 'paf'), paste0(var_types, '_dr')), 1, paste, collapse="_") %>% 
                sort] 
dt_d[type=='HAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
     .SDcols=paste('share', var_types, 'dr', sep="_")]

##superregion levels
results[['ad2']][year==analysis_year & type=='TAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
                 .SDcols=apply(expand.grid(c('pm_pc', 'paf'), var_types), 1, paste, collapse="_") %>% 
                   sort, by=super_region_name]  
results[['ad2']][year==analysis_year & type=='HAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
                 .SDcols=paste('share', var_types, sep="_"), by=super_region_name] 

#superregion pct decrease
dt_d[type=='TAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
     .SDcols=apply(expand.grid(c('pm_pc', 'paf'), paste0(var_types, '_dr')), 1, paste, collapse="_") %>% 
       sort, by=super_region_name] 
dt_d[type=='HAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
     .SDcols=paste('share', var_types, 'dr', sep="_"), by=super_region_name] 

#region pct decrease
dt_d[type=='TAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
     .SDcols=apply(expand.grid(c('pm_pc', 'paf'), paste0(var_types, '_dr')), 1, paste, collapse="_") %>% 
       sort, by=region_name] 
dt_d[type=='HAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
     .SDcols=paste('share', var_types, 'dr', sep="_"), by=region_name] 

#region abs decrease
dt_d[type=='TAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
     .SDcols=apply(expand.grid(c('pm_pc', 'paf'), paste0(var_types, '_d')), 1, paste, collapse="_") %>% 
       sort, by=region_name] 
dt_d[type=='HAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
     .SDcols=paste('share', var_types, 'd', sep="_"), by=region_name] 

##country levels
tabulateR(lvl='ad0', types='TAP', ind='pm_pc') #highest TAP
tabulateR(lvl='ad0', types='TAP', ind='pm_pc', 
          filter=list(var='super_region_id', vals=166, type=F)) #highest TAP excluding SSA

tabulateR(lvl='ad0', types='HAP', ind='share', 
          filter=list(var='super_region_id', vals=166, type=F)) #highest HAP share excluding SSA

tabulateR(lvl='ad0', types='TAP', ind='pm_pc', sorted='up') #lowest TAP
tabulateR(lvl='ad0', types='AAP', ind='share', sorted='up') #lowest AAP share
tabulateR(lvl='ad0', types='AAP', ind='share') #highest AAP share

#country level HAP:AAP ratio, what percent of countries is HAP the main contributor vs AAP
tabulateR(lvl='ad0', types='AAP', ind='share') %>% .[share_mean>.5, .N] #highest HAP share
tabulateR(lvl='ad0', types='AAP', ind='share', years=start_year) %>% .[share_mean>.5, .N] #highest HAP share

#country pct decrease
dt_d[type=='TAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
     .SDcols=apply(expand.grid(c('pm_pc', 'paf'), paste0(var_types, '_dr')), 1, paste, collapse="_") %>% 
       sort, by=iso3] 
dt_d[type=='HAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
     .SDcols=paste('share', var_types, 'dr', sep="_"), by=iso3] 

#country abs decrease
dt_d[type=='TAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
     .SDcols=apply(expand.grid(c('pm_pc', 'paf'), paste0(var_types, '_d')), 1, paste, collapse="_") %>% 
       sort, by=iso3] 
dt_d[type=='HAP', lapply(.SD, function(x) weighted.mean(x, w=pop_total, na.rm=T)), 
     .SDcols=paste('share', var_types, 'd', sep="_"), by=iso3] 

#pct of population that lives below WHO threshold
threshold <- 10
dt[year==analysis_year & pm_pc_mean < threshold & cause=='lri' & type=='TAP', sum(pop, na.rm=T)] / dt[year==analysis_year & cause=='lri' & type=='TAP', sum(pop, na.rm=T)] 
dt[year==analysis_year & pm_pc_lower < threshold & cause=='lri' & type=='TAP', sum(pop, na.rm=T)] / dt[year==analysis_year & cause=='lri' & type=='TAP', sum(pop, na.rm=T)] 
dt[year==analysis_year & pm_pc_upper < threshold & cause=='lri' & type=='TAP', sum(pop, na.rm=T)] / dt[year==analysis_year & cause=='lri' & type=='TAP', sum(pop, na.rm=T)] 

#pct of population that lives below WHO threshold interim-1
threshold <- 35
dt[year==analysis_year & pm_pc_mean < threshold & cause=='lri' & type=='TAP', sum(pop, na.rm=T)] / dt[year==analysis_year & cause=='lri' & type=='TAP', sum(pop, na.rm=T)] 
dt[year==analysis_year & pm_pc_lower < threshold & cause=='lri' & type=='TAP', sum(pop, na.rm=T)] / dt[year==analysis_year & cause=='lri' & type=='TAP', sum(pop, na.rm=T)] 
dt[year==analysis_year & pm_pc_upper < threshold & cause=='lri' & type=='TAP', sum(pop, na.rm=T)] / dt[year==analysis_year & cause=='lri' & type=='TAP', sum(pop, na.rm=T)] 

#how many districts did hap share stagnate or increase
threshold <- -0.01
dt_d[, .(mean=weighted.mean(hap_pct_mean_d, w=pop, na.rm=T)), by=.(ADM2_CODE, iso3, region_name)] %>% 
  .[, .(count=sum(mean>threshold, na.rm=T), N=.N)]
dt_d[, .(mean=weighted.mean(hap_pct_mean_d, w=pop, na.rm=T)), by=.(ADM2_CODE, iso3, region_name)] %>% 
  .[, .(count=sum(mean>threshold, na.rm=T), N=.N), by=region_name] %>% 
  .[, .(pct=count/N), by=region_name]

#pct breakdown of stagnating or increasing districts by reg
stagnators <-
  dt_d[, .(mean=weighted.mean(hap_pct_mean_d, w=pop, na.rm=T)), by=.(ADM2_CODE, iso3, region_name)] %>% 
  .[mean>threshold, unique(ADM2_CODE)] 
dt_d[ADM2_CODE %in% stagnators & year==analysis_year, region_name] %>% table %>% prop.table %>% round(2)

#how many stagnators were above the average TAP_pc
dt_d[ADM2_CODE %in% stagnators & year==analysis_year, .N]
dt_d[ADM2_CODE %in% stagnators & year==analysis_year, sum(tap_pc_mean>weighted.mean(tap_pc_mean, weight=pop, na.rm=T))/.N]
     
#explore stagnators
dt_d[ADM2_CODE %in% stagnators & tap_pc_d > 0, summary(dfu)]
dt_d[ADM2_CODE %in% stagnators, tap_pc_d > 0] %>% table
dt_d[ADM2_CODE %in% stagnators, tap_pc_d > 0] %>% table %>% prop.table %>% round(2)

#range of hap pct
dt[year==analysis_year, .(range=max(hap_pct_mean, na.rm=T)-min(hap_pct_mean, na.rm=T)), by=iso3] %>% 
  unique %>% 
  .[order(range)]

exploreRange('PAK', shp=adm2, dt=dt, var='hap_pct_mean')

#range of tap dose
dt[year==analysis_year, .(range=max(tap_pc_mean, na.rm=T)-min(tap_pc_mean, na.rm=T)), by=iso3] %>% 
  unique %>% 
  .[order(range)]

exploreRange('TCD', shp=adm2, dt=dt, var='tap_pc_mean')
exploreRange('SOM', shp=adm2, dt=dt, var='tap_pc_mean')

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

#how many children died from LRI attributable to TAP
tabulateR(lvl='global', types='TAP', ind='atr_count', years=c(analysis_year, start_year),
          terms=c('lvl', 'change', 'change_rate')) 
tabulateR(lvl='global', types=c('HAP', 'AAP'), ind='atr_count', years=c(analysis_year, start_year),
          terms=c('lvl', 'change', 'change_rate')) 


tabulateR(lvl='super_region', types='TAP', ind='atr_count') #highest TAP count
tabulateR(lvl='region', types='TAP', ind='atr_count') #highest TAP count
tabulateR(lvl='ad0', types='TAP', ind='atr_count') #highest TAP count

#how did the HAP share change
tabulateR(lvl='global', types='HAP', ind='share', terms=c('lvl', 'change', 'change_rate')) 

#how did the PAFs change
tabulateR(lvl='global', types='TAP', ind='paf', terms=c('lvl', 'change', 'change_rate'), years=c(analysis_year, start_year))

#PAFs by reg/country/district
tabulateR(lvl='super_region', types='TAP', ind='paf') 
tabulateR(lvl='region', types='TAP', ind='paf')
tabulateR(lvl='ad0', types='TAP', ind='paf', sorted='up') 
tabulateR(lvl='ad2', types='TAP', ind='paf')

#How many districts are majority TAP attributable
dt[year==analysis_year & type=='TAP' & term=='lvl', lapply(.SD, function(x) sum(x>.5, na.rm=T)/.N), 
   .SDcols=paste0('paf_', var_types)]

#proportion of total deaths from HAP
#TODO this needs to be calculated from draws
tabulateR(lvl='global', types='HAP', ind='atr_count', years=c(analysis_year, start_year)) %>%
  .[, c(3:5)] %>%
  .[] / (tabulateR(lvl='global', types='TAP', ind='atr_count', years=c(analysis_year, start_year)) %>% .[, c(3:5)])

# tabulateR(lvl='ad0', types='AAP', ind='atr_count', years=c(analysis_year, start_year)) %>%
#   .[ADM0_NAME %in% c('Nigeria', 'India'), c(3:5)] %>%
#   .[] / (tabulateR(lvl='ad0', types='TAP', ind='atr_count', years=c(analysis_year, start_year)) %>%
#            .[ADM0_NAME %in% c('Nigeria', 'India'), c(3:5)])

#how many children died in districts where more than half of LRI was attributed to TAP or HAP
# dt[year==analysis_year & tap_paf_mean > .5, lapply(.SD, sum, na.rm=T), .SDcols=c('lri_c_lower', 'lri_c_mean', 'lri_c_upper')]
# dt[year==analysis_year, lapply(.SD, sum, na.rm=T), .SDcols=c('lri_c_lower', 'lri_c_mean', 'lri_c_upper')]
# dt[year==analysis_year & (tap_paf_mean*hap_pct_mean) > .5, sum(lri_c_mean, na.rm=T)]

#change in HAP share of deaths
# dt[year==analysis_year & cause=='lri' & grouping=='child', lapply(.SD, weighted.mean, w=tap_lri_c_mean, na.rm=T), .SDcols=c('hap_pct_mean')]

#***********************************************************************************************************************

# ---GBD COMPARISON-----------------------------------------------------------------------------------------------------
#merge locs
gbd_dt <- merge(gbd_dt, locs, by='location_name')

#TODO bug in phillipines
gbd_dt <- gbd_dt[rate_mean!=0]

#compare our burden results to GBD's in order to assess the impact of exposure misclassification in GBD
compare_vars <- lapply(var_types, function(x) paste0(c('count_', 'atr_count_'), x)) %>% unlist
gbd_dt[, paste0('diff_', compare_vars) := lapply(compare_vars, function(x) get(paste0('gbd_', x)) - get(x))]
gbd_dt[, paste0('diff_rate_', compare_vars) := lapply(compare_vars, 
                                                  function(x) (get(paste0('gbd_', x)) - get(x))/get(x))]
#some kind of bug in s america
#gbd_dt <- gbd_dt[!(diff_rate_count_mean < .1 & diff_rate_count_mean > -.1), table(ADM0_NAME)]

#simple regression comparison
lm(diff_rate_atr_count_mean~year %>% as.factor+diff_rate_count_mean, data=gbd_dt)

#scatter
gbd_dt[abs(diff_atr_count_mean)>750, loc_label := location_name]
ggplot(gbd_dt, aes(x=gbd_atr_count_mean, y=atr_count_mean, color=super_region_name, label=loc_label)) + 
  geom_point() + 
  geom_text_repel() +
  facet_wrap(~year) + 
  geom_abline(slope=1) +
  theme_minimal() + 
  scale_y_sqrt(limits=c(0, 50000)) + 
  scale_x_sqrt(limits=c(0, 50000))


#map the comparisons
comparison <- load_map_results(indicator, indicator_group, run_date, raked, 
                               year_list=c(2000:2019),
                               custom_path = list('admin1'=gbd_dt[rak_level==1],
                                                  'admin0'=gbd_dt[rak_level==0]),
                               geo_levels=c('admin1', 'admin0'),
                               cores=cores)

#plot the state comparisons
state <-
  comparison$admin1 %>% 
  filter(year==2019) %>% 
  plot_map(., this_var='diff_mean',
           annotations, limits=c(-6700, 3300), title='LBD counts - GBD counts', 
           legend_title='Difference in counts',
           zoom=zoom.global,
           debug=F)

ggsave(filename=file.path(out.dir, 'state_hap_lri_counts_compare.png'), plot=state, 
       width=12, height=8, units='in', dpi=300)

#plot the country comparisons
ctry <-
  comparison$admin0 %>% 
  filter(year==2019) %>% 
  plot_map(., this_var='diff_mean',
           annotations, limits=c(-4100, 3000), title='LBD counts - GBD counts', 
           legend_title='Difference in counts',
           zoom=zoom.global,
           debug=F)

ggsave(filename=file.path(out.dir, 'ctry_hap_lri_counts_compare.png'), plot=ctry, 
       width=12, height=8, units='in', dpi=300)
#***********************************************************************************************************************
 
# ---SCRAPS-------------------------------------------------------------------------------------------------------------
#testing
ctry.name <- 'Nigeria'
ctry_data <- data$admin2 %>% 
  copy %>% 
  filter(NAME_0==ctry.name)
ctry.zoom <- data.table(x1=1, x2=15, y1=3, y2=15) #NGA

#***********************************************************************************************************************