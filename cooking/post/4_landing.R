# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 09/05/2018
# Purpose: Produce HAP diagnostics, results and sub-analyses
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
core_repo       <- '/homes/jfrostad/_code/lbd/lbd_core/'
core_repo       <- '/homes/jfrostad/_code/lbd/hap/'
my_repo       <- '/homes/jfrostad/_code/lbd/hap/'
commondir       <- paste(core_repo, 'mbg_central/share_scripts/common_inputs', sep = '/')

#load packages
package_lib    <- sprintf('%s_code/_lib/pkg',h_root)
## Load libraries and  MBG project functions.
.libPaths(package_lib)
pacman::p_load(data.table, fst, scales, ggplot2, RColorBrewer, sf, stringr, viridis, farver, reldist) 
package_list    <- package_list <- fread('/share/geospatial/mbg/common_inputs/package_list.csv') %>% t %>% c

# Use setup.R functions to load common LBD packages and mbg_central "function" scripts
message('Loading in required R packages and MBG functions')
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
run_date <- '2020_05_17_11_40_28'
lri_run_date <- '2020_06_11_11_19_26'
shapefile <- "2019_09_10"

indicator_group <- 'cooking'
indicator <- 'cooking_fuel_solid'

config_par <- 'hap_sp_fine'
cov_par <- 'cooking_VNM'

type <- 'mean'
raked <- T
start_year <- 2000
end_year <- 2018
cores <- 10

#mapping options
#these are set centrally by kim and lucas and need to match their formatting
map_ind_gp <- 'hap'

#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#results
data.dir <- file.path('/ihme/geospatial/mbg/cooking/post', run_date)

lri_dir <- '/ihme/geospatial/mbg/lri/has_lri/output'
  lri_rate_path <- 'has_lri_raked_mortality_admin_draws_eb_bin0_0.RData'
  lri_counts_path <- 'has_lri_raked_mortality_c_admin_draws_eb_bin0_0.RData'
  
#link
global_link_dir <- file.path('/home/j/WORK/11_geospatial/admin_shapefiles', shapefile) #TODO make official
  
###Output###
hap.paths <- data.table(admin2=file.path(data.dir, 'admin_2_summary_children.csv'))
hap.paths.d <- data.table(admin2=file.path(data.dir, 'admin_2_delta_summary.csv'))
out.dir  <- file.path('/ihme/geospatial/mbg/cooking/maps', run_date) %T>% dir.create(recursive = T)
  annotations_path <- file.path(out.dir, 'annotations.RDs')
map.dir <- file.path(j_root, 'WORK/11_geospatial/09_MBG_maps', map_ind_gp)
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#PE functions#
file.path(my_repo, '_lib', 'post', 'map_fx.R') %>% source
file.path(my_repo, '_lib', 'post', 'landing_gear.R') %>% source

#gbd fx
gbd.shared.function.dir <- '/ihme/cc_resources/libraries/current/r/'
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source


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

#read in link_table
global_link_table <- file.path(global_link_dir, "lbd_full_link.rds") %>% readRDS %>% as.data.table
adm_links <- global_link_table[, .(ADM0_NAME, ADM0_CODE, ADM1_NAME, ADM1_CODE, ADM2_NAME, ADM2_CODE)] %>% unique

#TODO does the same thing
admins <- get_sp_hierarchy(shapefile_version = modeling_shapefile_version)

#read in shps
stage1 <- st_read('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/shps_by_stage/stage1_ad2_gadm.shp')
stage2 <- st_read('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/shps_by_stage/stage2_ad2_gadm.shp')
adm2 <- rbind(stage1, stage2)

#read in mbg region info
stages <- file.path(j_root, 'WORK/11_geospatial/10_mbg/stage_master_list.csv') %>% fread #read info about stages

#read in gbd location info
locs <- get_location_metadata(location_set_id = 35, gbd_round_id = 6) %>% 
  .[, .(iso3=ihme_loc_id, location_name, super_region_id, super_region_name, region_id, region_name)] #subset to relevant columns

#create file to crosswalk AD0 to iso3
iso3_map <- dplyr::select(adm2, iso3, ADM0_CODE=gadm_geoid) 
iso3_map$geometry <- NULL
iso3_map <- as.data.table(iso3_map) %>% unique
locs <- merge(locs, iso3_map, by='iso3')
adm_links <- merge(adm_links, locs, by=c('ADM0_CODE'), all.x=T)

#combine and save all ad2 level results
dt <-
  list.files(data.dir, pattern='ad2_draws.fst', full.names = T) %>% 
  lapply(., read_fst, as.data.table=T) %>% 
  rbindlist(use.names=T, fill=T) %>% 
  .[, `:=` (cause='lri', grouping='child')] #TODO fix this in the descent file

#for now, we are using 2017 results as if they were 2018
#TODO model 2018
dt <- copy(dt) %>% 
  .[year==2017] %>% 
  .[, year := 2018] %>% 
  list(dt, .) %>% rbindlist 

#cap PAFs at 0
#TODO why are there negative PAFs??
dt[paf<0, paf:=0]

#generate attributable LRI variables
#read LRI rates per 1000/counts
load(file.path(lri_dir, lri_run_date, lri_rate_path), verbose=T)
lri_dt <- admin_2 %>% 
  melt(.,
       measure = patterns("V"),
       variable.name = "draw",
       value.name = 'rate') %>% 
  .[, `:=` (cause='lri', grouping='child', pop=NULL, region=NULL)]

load(file.path(lri_dir, lri_run_date, lri_counts_path), verbose=T)
lri_dt <- admin_2 %>% 
  melt(.,
       measure = patterns("V"),
       variable.name = "draw",
       value.name = 'count') %>% 
  merge(., lri_dt, by=c('year', 'ADM2_CODE', 'draw')) %>% 
  .[, `:=` (cause='lri', grouping='child', pop=NULL, region=NULL)]

dt <- merge(dt, lri_dt, by=c('ADM2_CODE', 'year', 'draw', 'grouping', 'cause'), all.x=T) 
dt[, atr_rate := rate * paf]
dt[, atr_count := count * paf]

#merge sr region names/IDs/ADM1_CODES
dt <- merge(dt, locs, by='ADM0_CODE', all.x=T)
dt <- merge(dt, adm_links[, .(ADM1_CODE, ADM2_CODE)], by='ADM2_CODE')

#for some reason its missing TTO
#TODO update centrally
dt[ADM0_CODE==225, `:=` (iso3='TTO',
                         location_name='Trinidad and Tobago',
                         super_region_id=103,
                         super_region_name='Latin America and Caribbean',
                         region_id=120,
                         region_name='Andean Latin America')]

#define columns to summarize
ind_cols <- c('share', 'pm_pc', 'prev', 'paf', 'rate', 'count', 'atr_rate', 'atr_count')

#define list of aggregations
by_cols <- list('global'=c('year', 'type', 'cause', 'grouping'),
                'super_region'=c('year', 'type', 'cause', 'grouping', 'super_region_id'),
                'region'=c('year', 'type', 'cause', 'grouping', 'region_id'),
                'ad0'=c('year', 'type', 'cause', 'grouping', 'ADM0_CODE'),
                'ad1'=c('year', 'type', 'cause', 'grouping', 'ADM0_CODE', 'ADM1_CODE'),
                'ad2'=c('year', 'type', 'cause', 'grouping', 'ADM0_CODE','ADM2_CODE'))

#summarize and produce aggregations
results <- mclapply(1:length(by_cols), 
                    calcSummaryStats,
                    by_list=by_cols,
                    dt=dt, 
                    ind_cols=ind_cols,
                    mc.cores=6) %>% rbindlist(use.names=T, fill=T)

#save all aggregations
write.fst(results, file.path(data.dir, 'all_summary.fst'))

#helper function to save each aggregation type while cleaning up any irrelevant variables
saveResults <- function(agg, dt) {
  
  message('saving ', agg, ' results')
  
  dt[lvl==agg] %>% 
    Filter(function(x) !all(is.na(x)), .) %>% 
    write.csv(., paste0(data.dir, '/', agg, '_summary.csv'), row.names = F)
  
  return(NULL)
  
  
} 

lapply(by_cols %>% names, saveResults, dt=results)

stop()

#produce inequality metrics
#calculate GINI/MAD at country level
dt_ineq <- dt[year %in% c(start_year, end_year), .(iso3, year, ADM0_CODE, ADM2_CODE, ADM2_NAME, dfu_mean, 
                                                  super_region_id, super_region_name, region_id, region_name)]
dt_ineq[, gini := gini(dfu_mean), by=.(iso3, year)]
dt_ineq[, mad := mad(dfu_mean, center = mean(dfu_mean)), by=.(iso3, year)]
dt_ineq[, mean := mean(dfu_mean, na.rm=T), by=.(iso3, year)]
dt_ineq[, max := max(dfu_mean, na.rm=T), by=.(iso3, year)]
dt_ineq[, min := min(dfu_mean, na.rm=T), by=.(iso3, year)]
dt_ineq[, range := max-min]



#also output the file for later
write.csv(dt_d, file.path(data.dir, 'admin_2_delta_summary.csv'), row.names = F)

#also create ad2 sf object for spatial analyses
data <-  
  load_map_results(indicator, indicator_group, run_date, raked, 
                   year_list=c(start_year:end_year),
                   custom_path = hap.paths,
                   geo_levels=c('admin2'),
                   cores=cores,
                   debug=F)

data_d <-  
  load_map_results(indicator, indicator_group, run_date, raked, 
                   year_list=2018,
                   custom_path = list('admin2'=dt_d),
                   geo_levels=c('admin2'),
                   cores=cores)

#***********************************************************************************************************************

# ---RUN MODEL DIAGNOSTICS----------------------------------------------------------------------------------------------
#produce lineplots and other model diagnostics

#***********************************************************************************************************************

# ---SEV CELL_PREDS-----------------------------------------------------------------------------------------------------
#read in the proper annotations (borders, lakes, mask)
check <- file.exists(annotations_path)
annotations <- ifelse(
  check,
  readRDS(annotations_path),
  load_map_annotations()
)
if(!check) saveRDS(annotations, file=annotations_path)

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
  merge(., adm_links[, .(ADM2_CODE, ADM0_CODE)], by='ADM2_CODE')  

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

#make sf datasets for plotting
threshold <- .01
data_projs <-  
  load_map_results(indicator, indicator_group, run_date, raked, 
                   year_list=c(2018, 2020, 2030),
                   custom_path = list('admin2'=projs),
                   geo_levels=c('admin2'),
                   cores=cores)
data_sdg <-  
  load_map_results(indicator, indicator_group, run_date, raked, 
                   year_list=2030,
                   custom_path = list('admin2'=probs),
                   geo_levels=c('admin2'),
                   cores=cores)

#make colorscales
d_colors <- brewer_pal(palette='BrBG')(11) %>% rev 
d_values <- c(seq(-.2, -.1, length.out = 2), seq(-.1, 0, length.out = 7), seq(0, .2, length.out = 2)) %>%
  unique %>%
  rescale

p_colors <- brewer_pal(palette='BrBG')(11) %>% .[c(1, 3:9, 11)]
p_values <- c(seq(0, .05, length.out = 2), seq(.1, .9, length.out = 5), seq(.95, 1, length.out = 2)) %>%
  unique %>%
  rescale

s_values <- c(seq(0, .05, length.out = 2), seq(.1, 1, length.out = 9)) %>% 
  unique %>%
  rescale

#make ad2 plots
#plot AROCs
global <-
  data_projs$admin2 %>% 
  filter(year==2018) %>% 
  plot_map(., this_var='aroc_mean',
           annotations, limits=c(-.2, .2), title='Mean AROC of SEV, 2000-2018', 
           legend_colors=d_colors, legend_color_values = d_values,
           legend_title='AROC',
           #zoom=zoom.global,
           debug=F)
ggsave(filename=file.path(out.dir, 'sev_aroc.png'), plot=global, 
       width=12, height=8, units='in', dpi=500)

#plot projections
global <-
  data_projs$admin2 %>% 
  filter(year==2018) %>% 
  plot_map(., this_var='sev_mean',
           annotations, limits=c(0, 1), title='Mean SEV, 2020', 
           legend_color_values = p_values,
           legend_title='SEV',
           #zoom=zoom.global,
           debug=F)
ggsave(filename=file.path(out.dir, 'sev_2020.png'), plot=global, 
       width=12, height=8, units='in', dpi=500)

global <-
  data_projs$admin2 %>% 
  filter(year==2018) %>% 
  plot_map(., this_var='sev_lower',
           annotations, limits=c(0, 1), title='Lower C.I. for SEV, 2020', 
           legend_color_values = p_values,
           legend_title='SEV',
           #zoom=zoom.global,
           debug=F)
ggsave(filename=file.path(out.dir, 'sev_2020_lower.png'), plot=global, 
       width=12, height=8, units='in', dpi=500)

#make plots of probabilities of attainment
global <-  
  data_sdg$admin2 %>% 
  filter(target==threshold) %>% 
  plot_map(., this_var='absolute_goal_prob',
           annotations, limits=c(0, 1), title='Probability of achieving SDG 7.1 (<.01%) in 2030', 
           legend_colors=p_colors, legend_color_values = p_values,
           legend_title='Probability',
           #zoom=zoom.global,
           debug=F)
ggsave(filename=file.path(out.dir, 'sdg_7_probs.png'), plot=global, 
       width=12, height=8, units='in', dpi=500)

global <-
  data_sdg$admin2 %>% 
  filter(target==.05) %>% 
  plot_map(., this_var='absolute_goal_prob',
           annotations, limits=c(0, 1), title='Probability of achieving SDG 7.1 (<.05%) in 2030', 
           legend_colors=p_colors, legend_color_values = p_values,
           legend_title='Probability',
           #zoom=zoom.global,
           debug=F)
ggsave(filename=file.path(out.dir, 'sdg_7_probs_05.png'), plot=global, 
       width=12, height=8, units='in', dpi=500)

#testing country specific plots
ctry.name <- 'Sudan'
ctry_data <- data_sdg$admin2 %>% 
  filter(NAME_0==ctry.name & target==threshold) 
ctry.zoom <- data.table(x1=1, x2=15, y1=3, y2=15) #NGA

ctry.dfu <-
  plot_map(ctry_data, this_var='absolute_goal_prob',
           annotations, limits=c(0, 1), title='Probability of achieving SDG 7.1 (<.05%) in 2030', 
           legend_colors=viridis(10, direction=-1),
           legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
           legend_title='DFU %', legend_flip=T,
           zoom=T,
           debug=F)

ctry.name <- 'Pakistan'
ctry_data <- data_sdg$admin2 %>% 
  copy %>% 
  filter(NAME_0==ctry.name & target==threshold) 

ctry.dfu <-
  plot_map(ctry_data, this_var='absolute_goal_prob',
           annotations, limits=c(0, 1), title='Probability of achieving SDG 7.1 (<.05%) in 2030', 
           legend_colors=viridis(10, direction=-1),
           legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
           legend_title='DFU %', legend_flip=T,
           zoom=T,
           debug=F)

#create raster from probabilities
out <-
  prepare_aroc_proj_rasters(
    regions='all',
    indicator_group = indicator_group,
    indicator = indicator,
    run_date = run_date,
    measure='sev',
    pred_deriv='target_probs',
    raking='raked',
    uselogit=T,
    abs_rel = 'absolute',
    target_yr = '2030',
    target = .01,
    target_type = 'less',
    baseline_year = 2017,
    shapefile_version = modeling_shapefile_version,
    debug=F
  )

proj_raster <- lapply(out, function(x) x[['raster']])

#TODO move this section to spirit.R
#make plots of raster vals
rasterPlot <- function(x, this_var, legend_color_values=NA) {
  
  df <- as.data.frame(x, xy = TRUE) %>% as.data.table
  df[, plot_var := get(this_var)]
  
  if(legend_color_values %>% is.na) {
    
    message('->color values not provided, building from data IQR')
    
    data_min <- df$plot_var %>% min(na.rm=T) %>% floor
    data_p25 <- df$plot_var %>% quantile(probs=.2, na.rm=T)
    data_p75 <- df$plot_var %>% quantile(probs=.8, na.rm=T)
    data_max <- df$plot_var %>% max(na.rm=T) %>% ceiling
    
    legend_color_values <- c(seq(data_min, data_p25, length.out = 2), 
                             seq(data_p25, data_p75, length.out = 8), #bring out the variation in the IQR
                             seq(data_p75, data_max, length.out = 2)) %>%
      unique %T>% 
      print %>%
      rescale
    
  }
  
  plot <- ggplot() +
    geom_raster(data =df, aes(x = x, y = y, fill = plot_var)) + 
    scale_fill_viridis_c(guide=F, na.value="white", values = legend_color_values) + #matching ad2 aggs) +
    coord_quickmap() +
    theme_bw() +
    theme(axis.text=element_blank(), 
          axis.ticks=element_blank(),
          axis.title=element_blank())
  
  return(plot)
  
}

#make plot of all regions
grobs <- lapply(proj_raster, rasterPlot, this_var='layer', legend_color_values=ad2_values)
arrangeGrob(grobs=grobs) %>% grid.arrange

#plot ad2s aggregated from cells
proj_ad2 <- lapply(out, function(x) x[['ad2']]) %>%
  .[!sapply(., is.null)] %>% #remove the null tables (missing raster values)
  rbindlist(use.names=T, fill=T) %>%
  merge(., stages[, .(ADM0_CODE=gadm_geoid, mbg_reg, spr_reg_id)], by=c('ADM0_CODE')) %>%
  merge(., adm_links, by=c('ADM0_CODE', 'ADM2_CODE')) %T>%
  write.csv(., file.path(data.dir, 'agg_admin_2_sdg_summary.csv'), row.names = F)

#make ad2 plot at stricter threshold
data_sdg <-  
  load_map_results(indicator, indicator_group, run_date, raked, 
                   single_year=2030,
                   custom_path = list('admin2'=proj_ad2),
                   geo_levels=c('admin2'),
                   cores=cores)

global <-
  plot_map(data_sdg$admin2, this_var='mean',
           annotations, limits=c(0, 1), title='Probability of achieving SDG 7.1 (<.01%) in 2030', 
           legend_colors=viridis(10, direction=-1),
           legend_title='Probability',
           #zoom=zoom.global,
           debug=F)
ggsave(filename=file.path(out.dir, 'sdg_7_probs_01_agg.png'), plot=global, 
       width=12, height=8, units='in', dpi=500)
#***********************************************************************************************************************
 
# ---SAVE MAPPING INPUTS------------------------------------------------------------------------------------------------
#output files for Kim to produce key figures
##Figure 1: Selected AD2 Results for 2018##
#A: DFU levels
saveMappingInput(
  dt, 
  map_ind='dfu',
  data_ind='dfu',
  map_measure='mean',
  data_measure='mean'
)

#B: TAP PC levels
saveMappingInput(
  dt_d, 
  map_ind='tap_pc',
  data_ind='tap_pc',
  map_measure='mean',
  data_measure='mean'
)

#C: HAP PCT levels
saveMappingInput(
  dt_d, 
  map_ind='hap_pct',
  data_ind='hap_pct',
  map_measure='mean',
  data_measure='mean'
)

#D: Attributable LRI rates
saveMappingInput(
  dt, 
  map_ind='tap_lri',
  data_ind='tap_lri',
  map_measure='mean',
  data_measure='mean'
)

##Figure 2: Selected AD2 Trends 2000-2018##
#A: DFU levels
saveMappingInput(
  dt_d, 
  map_ind='dfu',
  data_ind='dfu',
  map_measure='change_rate',
  data_measure='mean_dr'
)

#B: Change rate for TAP_PC 2000-2018
saveMappingInput(
  dt_d, 
  map_ind='tap_pc',
  data_ind='tap_pc',
  map_measure='change_rate',
  data_measure='mean_dr'
)

#C: Change rate for attributable LRI 2000-2018
saveMappingInput(
  dt_d, 
  map_ind='tap_lri',
  data_ind='tap_lri',
  map_measure='change_rate',
  data_measure='mean_dr'
)

#D: SEV in 2018
saveMappingInput(
  projs, 
  map_ind='sev',
  data_ind='sev',
  map_measure='mean',
  data_measure='mean'
)

#E: Probability of hitting SDG threshold
saveMappingInput(
  projs, 
  map_ind='sev_uncertainty',
  data_ind='sev',
  map_measure='relative_uncertainty',
  data_measure='rel_uncertainty'
)

#F: Probability of hitting SDG threshold
threshold <- 0.05
saveMappingInput(
  probs[target==threshold], 
  map_ind='sdg_prob',
  data_ind='absolute_goal',
  map_measure='mean',
  data_measure='prob'
)
#***********************************************************************************************************************

# ---FIGURE 4-----------------------------------------------------------------------------------------------------------
#histogram/density plot of LRI deaths vs tap_pc
#setup plot data
plot.dt <- dt %>% 
  copy %>% 
  .[year %in% c(2000, 2017)] %>% 
  .[year==2017, year := 2018] %>% 
  #.[tap_pc>1500, tap_pc := 1500] %>%  #cap at 500 ug/m3
  na.omit(., cols=c('dfu', 'lri_c', 'lri', 'pop'))

by_vars <- c('year', 'pm', 'super_region_id')

plot.dt[, pm := round_any(tap_pc, 10)]
plot.dt[, n := sum(lri_c), by=by_vars]
plot.dt[, r := weighted.mean(lri, w=pop), by=by_vars]
plot.dt[, hap_share := weighted.mean(hap_pct, w=pop), by=by_vars]
plot.dt <- unique(plot.dt, by=by_vars)

ggplot(data=plot.dt[year==2000 & !is.na(super_region_id)], aes(x=pm,  y=r*1000, fill=hap_share)) +
  #geom_ribbon() + 
  facet_wrap(~super_region_name) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option='viridis') +
  scale_x_continuous(limits=c(0, 750)) +
  scale_y_continuous(limits=c(0, 10)) +
  coord_trans(y='sqrt') +
  ggtitle('2000') +
  theme_minimal()

ggplot(data=plot.dt[year==2018 & !is.na(super_region_id)], aes(x=pm,  y=r*1000, fill=hap_share)) +
  #geom_ribbon() + 
  facet_wrap(~super_region_name) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option='viridis') +
  scale_x_continuous(limits=c(0, 750)) +
  scale_y_continuous(limits=c(0, 10)) +
  coord_trans(y='sqrt') +
  ggtitle('2018') +
  theme_minimal()

ggplot(data=plot.dt[year==2000 & !is.na(super_region_id)], aes(x=pm,  y=n, fill=hap_share)) +
  #geom_ribbon() + 
  facet_wrap(~super_region_name) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option='viridis') +
  scale_x_continuous(limits=c(0, 750)) +
  scale_y_continuous(limits=c(0, 20000)) +
  coord_trans(y='sqrt') +
  ggtitle('2000') +
  theme_minimal()

ggplot(data=plot.dt[year==2018 & !is.na(super_region_id)], aes(x=pm,  y=n, fill=hap_share)) +
  #geom_ribbon() + 
  facet_wrap(~super_region_name) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option='viridis') +
  scale_x_continuous(limits=c(0, 750)) +
  scale_y_continuous(limits=c(0, 20000)) +
  coord_trans(y='sqrt') +
  ggtitle('2018') +
  theme_minimal()

# ggplot(data=plot.dt[year==2018], aes(tap_pc,  y = ..count.., weight = lri_c, fill=hap_pct>.5)) +
#   geom_density(binwidth = 5, position = "stack", alpha=.2) + 
#   theme_bw()
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


# proj_ad0 <- lapply(out, function(x) x[['ad0']]) %>% 
#   .[!sapply(., is.null)] %>% #remove the null tables (missing raster values)
#   rbindlist(use.names=T, fill=T) %>%
#   merge(., stages[, .(ADM0_CODE=gadm_geoid, mbg_reg, spr_reg_id)], by=c('ADM0_CODE')) %>% 
#   merge(., adm_links[, .(ADM0_CODE, ADM0_NAME)] %>% unique, by=c('ADM0_CODE')) %T>% 
#   write.csv(., file.path(data.dir, 'admin_0_sdg_summary.csv'), row.names = F)
# 
# proj_ad2 <- lapply(out, function(x) x[['ad2']]) %>% 
#   .[!sapply(., is.null)] %>% #remove the null tables (missing raster values)
#   rbindlist(use.names=T, fill=T) %>%
#   merge(., stages[, .(ADM0_CODE=gadm_geoid, mbg_reg, spr_reg_id)], by=c('ADM0_CODE')) %>% 
#   merge(., adm_links, by=c('ADM0_CODE', 'ADM2_CODE')) %T>% 
#   write.csv(., file.path(data.dir, 'admin_2_sdg_summary.csv'), row.names = F)

#***********************************************************************************************************************