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
pacman::p_load(data.table, scales, ggplot2, RColorBrewer, sf, viridis, farver, reldist) 
package_list    <- package_list <- fread('/share/geospatial/mbg/common_inputs/package_list.csv') %>% t %>% c

# Use setup.R functions to load common LBD packages and mbg_central "function" scripts
message('Loading in required R packages and MBG functions')
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
run_date <- '2020_05_06_22_40_43'
lri_run_date <- '2020_01_10_15_18_27'
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
data.dir <- file.path('/ihme/geospatial/mbg/cooking/pafs', run_date)

lri_dir <- '/ihme/geospatial/mbg/lri/has_lri/output'
  lri_rate_path <- 'has_lri_admin_2_raked_mortality_summary.csv'
  lri_counts_path <- 'has_lri_c_admin_2_raked_mortality_summary.csv'
  
#link
global_link_dir <- file.path('/home/j/WORK/11_geospatial/admin_shapefiles', shapefile) #TODO make official
  
###Output###
hap.paths <- data.table(admin2=file.path(data.dir, 'admin_2_summary_children.csv'))
hap.paths.d <- data.table(admin2=file.path(data.dir, 'admin_2_delta_summary.csv'))
out.dir  <- file.path('/ihme/geospatial/mbg/cooking/maps', run_date) %T>% dir.create(recursive = T)
map.dir <- file.path(j_root, 'WORK/11_geospatial/09_MBG_maps', map_ind_gp)
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#PE functions#
file.path(my_repo, '_lib', 'post', 'map_fx.R') %>% source
file.path(my_repo, '_lib', 'post', 'make_projections.R') %>% source

#gbd fx
gbd.shared.function.dir <- '/ihme/cc_resources/libraries/current/r/'
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source

#custom function to save files into the central mapping format
saveMappingInput <- function(dt, 
                             map_ind,
                             data_ind=map_ind, #but it could be different if misnamed
                             map_measure,
                             data_measure=map_measure, #but it could be different if misnamed 
                             raking_label=raked,
                             admin_level=2,
                             parent_dir=map.dir
) {
  
  #define dirs and paths (also create recursively)
  out_dir <- file.path(parent_dir, map_ind, run_date, 'inputs') %T>% dir.create(recursive=T) 
  out_path <- file.path(out_dir, paste0(map_ind, '_', map_measure,
                                        ifelse(raking_label, '_raked_', '_unraked_'), 
                                        'ad', admin_level, '.csv')) %T>%
    message('saving data for ind=', data_ind, ' (', map_measure, ') as\n',
            .)
  
  #setup dt
  out <- copy(dt) %>% 
    .[, c(paste0('ADM', admin_level, '_CODE'), paste0(data_ind, data_measure), 'year'), with=F] %>% 
    setnames(paste0(data_ind, data_measure), 'value')
  
  #save
  write.csv(out, file=out_path, row.names=F)
  
}
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

#read LRI rates/counts
has_lri <- file.path(lri_dir, lri_run_date, 'pred_derivatives/admin_summaries', lri_rate_path) %>% 
  fread %>% 
  .[, .(ADM0_CODE, ADM2_CODE, year, lri=mean, cause='lri', grouping='child')]

has_lri_c <- file.path('/ihme/geospatial/mbg/lri/has_lri/output', 
                       lri_run_date, 'pred_derivatives/admin_summaries', lri_counts_path) %>% 
  fread %>% 
  .[, .(ADM0_CODE, ADM2_CODE, year, lri_c=mean, cause='lri', grouping='child')]

#combine and save all ad2 level results
dt <-
  list.files(data.dir, pattern='ad2_tap_results', full.names = T) %>% 
  lapply(., fread) %>% 
  rbindlist(use.names=T, fill=T) %>% 
  merge(has_lri, by=c('ADM0_CODE', 'ADM2_CODE', 'year', 'cause', 'grouping'), all.x=T) %>% 
  merge(has_lri_c, by=c('ADM0_CODE', 'ADM2_CODE', 'year', 'cause', 'grouping'), all.x=T) %>% 
  .[, tap_lri := lri * 1000 * tap_paf] %>% #do some postestimation
  .[, hap_lri := lri * 1000 * tap_paf*hap_pct] %>% 
  .[, aap_lri := lri * 1000 * tap_paf*(1-hap_pct)] %>% 
  .[, tap_lri_c := lri_c * tap_paf] %>% #do some postestimation
  .[, hap_lri_c := lri_c * tap_paf*hap_pct] %>% 
  .[, aap_lri_c := lri_c * tap_paf*(1-hap_pct)] %T>% 
  #also output the file for later
  write.csv(., file.path(data.dir, 'admin_2_summary.csv'), row.names = F)

#also save a version just for children
dt[grouping=='child' & cause=='lri'] %>% 
  write.csv(., file.path(data.dir, 'admin_2_summary_children.csv'), row.names = F)

#for now, we are using 2017 results as if they were 2018
#TODO model 2018
dt <- copy(dt) %>% 
  .[year==2017] %>% 
  .[, year := 2018] %>% 
  list(dt, .) %>% rbindlist

#merge sr region names/IDs
dt <- merge(dt, locs, by='ADM0_CODE', all.x=T)
dt <- merge(dt, adm_links, by=c('ADM0_CODE', 'ADM2_CODE'))

#intermediate measures
dt[, u5_pct := pop/pop_total]

#format names
setnames(dt, 'hap', 'dfu')

#also combine and save all ad0 level results
dt_ad0 <-
  list.files(data.dir, pattern='ad0_tap_results', full.names = T) %>% 
  lapply(., fread) %>% 
  rbindlist(use.names=T, fill=T) %>% 
  merge(., locs, by='ADM0_CODE', all.x=T) %T>%
  #also output the file for later   
  write.csv(., file.path(data.dir, 'admin_0_summary.csv'), row.names = F)

#produce inequality metrics
#calculate GINI/MAD at country level
dt_ineq <- dt[year %in% c(start_year, end_year), .(iso3, year, ADM0_CODE, ADM2_CODE, ADM2_NAME, dfu, 
                                                  super_region_id, super_region_name, region_id, region_name)]
dt_ineq[, gini := gini(dfu), by=.(iso3, year)]
dt_ineq[, mad := mad(dfu, center = mean(dfu)), by=.(iso3, year)]
dt_ineq[, mean := mean(dfu, na.rm=T), by=.(iso3, year)]
dt_ineq[, max := max(dfu, na.rm=T), by=.(iso3, year)]
dt_ineq[, min := min(dfu, na.rm=T), by=.(iso3, year)]
dt_ineq[, range := max-min]

#produce change metrics
#TODO move this to be at pixel level
#calculate rates of change
these_cols <- c('hap_pct', 'dfu', 'tap_paf', 'tap_pc')
d_cols <- paste0(these_cols, '_d')
dr_cols <- paste0(these_cols, '_dr')
aroc_cols <- paste0(these_cols, '_aroc')
dt_d <- setkey(dt, ADM0_CODE, ADM2_CODE, grouping, cause) %>% 
  .[year %in% c(start_year, end_year)] %>% 
  .[grouping=='child' & cause=='lri'] %>%  #currently only estimating change for LRI
  .[, (d_cols) := .SD-data.table::shift(.SD,n=1), .SDcols=these_cols, by=key(dt)] %>% 
  .[, (dr_cols) := (.SD-data.table::shift(.SD,n=1))/data.table::shift(.SD,n=1), .SDcols=these_cols, by=key(dt)] %T>% 
  .[, (aroc_cols) := (.SD-data.table::shift(.SD,n=1))/(end_year-start_year), .SDcols=these_cols, by=key(dt)] %T>% 
  #also output the file for later
  write.csv(., file.path(data.dir, 'admin_2_delta_summary.csv'), row.names = F)

#also create ad2 sf object for spatial analyses
data <-  
  load_map_results(indicator, indicator_group, run_date, raked, 
                   start_year=start_year, end_year=end_year,
                   custom_path = hap.paths,
                   geo_levels=c('admin2'),
                   cores=cores,
                   debug=F)

data_d <-  
  load_map_results(indicator, indicator_group, run_date, raked, 
                   single_year=2018,
                   custom_path = list('admin2'=dt_d),
                   geo_levels=c('admin2'),
                   cores=cores)

#***********************************************************************************************************************

# ---SAVE MAPPING INPUTS------------------------------------------------------------------------------------------------
#output files for Kim to produce key figures
#Figure 1: Selected AD2 Results
#A: DFU levels
saveMappingInput(
  dt, 
  map_ind='dfu',
  data_ind='dfu',
  map_measure='mean',
  data_measure=''
)

#B: Change rate for TAP_PC 2000-2018
saveMappingInput(
  dt_d, 
  map_ind='tap_pc',
  data_ind='tap_pc',
  map_measure='change_rate',
  data_measure='_dr'
)

#C: Change rate for TAP_PAF 2000-2018
saveMappingInput(
  dt_d, 
  map_ind='tap_paf',
  data_ind='tap_paf',
  map_measure='change_rate',
  data_measure='_dr'
)

#D: Attributable LRI levels
saveMappingInput(
  dt, 
  map_ind='tap_lri',
  data_ind='tap_lri',
  map_measure='mean',
  data_measure=''
)

#***********************************************************************************************************************

# ---RUN MODEL DIAGNOSTICS----------------------------------------------------------------------------------------------
#produce lineplots and other model diagnostics

#***********************************************************************************************************************

# ---SEV CELL_PREDS-----------------------------------------------------------------------------------------------------
#read in the proper annotations (borders, lakes, mask)
annotations_path <- file.path(out.dir, 'annotations.RDs')
check <- file.exists(annotations_path)
annotations <- ifelse(
  check,
  readRDS(annotations_path),
  load_map_annotations()
)
if(!check) saveRDS(annotations, file=annotations_path)

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

proj_ad0 <- lapply(out, function(x) x[['ad0']]) %>% 
  .[!sapply(., is.null)] %>% #remove the null tables (missing raster values)
  rbindlist(use.names=T, fill=T) %>%
  merge(., stages[, .(ADM0_CODE=gadm_geoid, mbg_reg, spr_reg_id)], by=c('ADM0_CODE')) %>% 
  merge(., adm_links[, .(ADM0_CODE, ADM0_NAME)] %>% unique, by=c('ADM0_CODE')) %T>% 
  write.csv(., file.path(data.dir, 'admin_0_sdg_summary.csv'), row.names = F)

proj_ad2 <- lapply(out, function(x) x[['ad2']]) %>% 
  .[!sapply(., is.null)] %>% #remove the null tables (missing raster values)
  rbindlist(use.names=T, fill=T) %>%
  merge(., stages[, .(ADM0_CODE=gadm_geoid, mbg_reg, spr_reg_id)], by=c('ADM0_CODE')) %>% 
  merge(., adm_links, by=c('ADM0_CODE', 'ADM2_CODE')) %T>% 
  write.csv(., file.path(data.dir, 'admin_2_sdg_summary.csv'), row.names = F)

#save data for kim to make ARCmaps
#E: Attributable LRI levels
saveMappingInput(
  proj_ad2, 
  map_ind='sdg_prob',
  data_ind='mean',
  map_measure='mean',
  data_measure=''
)

#TODO move this section to spirit.R
#make plots of raster vals
rasterPlot <- function(x) {
  
  df <- as.data.frame(x, xy = TRUE)
  
  plot <- ggplot() +
    geom_raster(data =df, aes(x = x, y = y, fill = layer)) + 
    scale_fill_viridis_c(guide=F, na.value="white") +
    coord_quickmap() +
    theme_bw() +
    theme(axis.text=element_blank(), 
          axis.ticks=element_blank(),
          axis.title=element_blank())
  
  return(plot)
  
}

#make plot of all regions
grobs <- lapply(proj_raster, rasterPlot)
arrangeGrob(grobs=grobs) %>% grid.arrange
            
#make ad2 plot
data_sdg <-  
  load_map_results(indicator, indicator_group, run_date, raked, 
                   single_year=2030,
                   custom_path = list('admin2'=proj_ad2),
                   geo_levels=c('admin2'),
                   cores=cores)

global <-
  plot_map(data_sdg$admin2, this_var='mean',
           annotations, limits=c(0, 1), title='Probability of achieving SDG 7.1 in 2030', 
           # legend_color_values=color_values,
           legend_title='Probability',
           #zoom=zoom.global,
           debug=F)
ggsave(filename=file.path(out.dir, 'sdg_7_probs.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)
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

#***********************************************************************************************************************