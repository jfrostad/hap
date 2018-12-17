indicator        = as.character(commandArgs()[4])
indicator_group  = as.character(commandArgs()[5])
run_date         = as.character(commandArgs()[6])
pop_measure      = as.character(commandArgs()[7])
repo             = as.character(commandArgs()[8])
regions          = as.character(commandArgs()[9])
baseline_year    = as.numeric(commandArgs()[10])
goal_threshold   = as.numeric(commandArgs()[11])
diarrhea_measure = as.character(commandArgs()[12])
target_type      = as.character(commandArgs()[13])
shapefile_version = as.character(commandArgs()[14])

#diarrhea_measure <- 'incidence'
#if(indicator_group == 'education') target_type <- 'greater'
#if(indicator_group == 'education') diarrhea_measure <- goal_threshold
#if(indicator_group == 'diarrhea') target_type <- 'less'

if(indicator_group=='education') use_raked <- '_raked'
if(indicator_group!='education') use_raked <- ''

##################################################################################
## Setup.
##################################################################################
message('Setting up...')

## Define main directories.
results_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date, '/')
val_dir <- paste0(results_dir, 'validation_report')
dir.create(val_dir, showWarnings = FALSE)

## Load libraries and miscellaneous MBG project functions.
setwd(repo)
root <- "/home/j/"
package_lib <- '/snfs1/temp/ngraetz/special_packages/mbg_pkgs_conda' # Library for all MBG versioned packages. Ensures that none of this code is
#    dependent on the machine where the user runs the code.
.libPaths(package_lib)                                  # Ensures packages look for dependencies here when called with library().
#    Necessary for seeg libraries.
source('mbg_central/mbg_functions.R')                   # Functions to run MBG model.
source('mbg_central/prep_functions.R')                  # Functions to setup MBG run
source('mbg_central/covariate_functions.R')             # Functions to prep and transform 5*5 covariates
source('mbg_central/misc_functions.R')                  # Other computational MBG-related functions.
source('mbg_central/post_estimation_functions.R')
source('mbg_central/gbd_functions.R')
source('mbg_central/shiny_functions.R')
source('mbg_central/holdout_functions.R')
source('education/pop_splitting_functions.R')
source('mbg_central/seegMBG_transform_functions.R')
source('mbg_central/validation_report_functions.R')
package_list <- c('fields', 'gridGraphics' ,'grid', 'gridExtra', 'gstat', 'magrittr', 'ggplot2', 'doParallel', 'SDMTools', 'foreign', 'rgeos', 'data.table','raster','rgdal','INLA','seegSDM','seegMBG','plyr','dplyr','leaflet')
for(package in package_list) {
  library(package, lib.loc = package_lib, character.only=TRUE)
}

#all_gauls       = get_adm0_codes('africa')
year_list       = c(2000:2015)

## Check if has holdouts
n_holdouts <- check_for_holdouts(results_dir)

##################################################################################
## Common objects for all validation functions.
##################################################################################
##  - Raking factors
##  - Raked/unraked results
##  - Populations
##  - Shapes to define modeling area
##  - Make master list of countries, where each item contains a specific country's draws, pops, and templates.
##################################################################################
message('Defining and loading common objects...')

## Define path to csv of all raking factors, load in data.table.
in_dir  <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date)
default_rf_path <- paste0(in_dir, '/', indicator, '_rf.csv')
all_rfs <- fread(default_rf_path)

## Define path to .tif of results and raked results, load in raster bricks.
default_raked_results_path <- paste0(in_dir, '/', indicator, '_mean_raked_raster.tif')
results_raked <- brick(default_raked_results_path)
default_results_path <- paste0(in_dir, '/', indicator, '_mean_raster.tif')
results <- brick(default_results_path)

## Load admin2 raster
admin_level <- 2
shapes <- shapefile(get_admin_shapefile(admin_level, version = shapefile_version))
admin2_shapefile <- shapes

admin_level <- 1
shapes <- shapefile(get_admin_shapefile(admin_level, version = shapefile_version))
admin1_shapefile <- shapes

## Load regional pops
simple_polygon_list <- load_simple_polygon(gaul_list = get_adm0_codes(regions,
                                                                      shapefile_version = shapefile_version),
                                           buffer = 0.4,
                                           subset_only = FALSE,
                                           shapefile_version = shapefile_version)
subset_shape   <- simple_polygon_list[[1]]
simple_polygon <- simple_polygon_list[[2]]
pop_raster_annual <- suppressMessages(suppressWarnings(load_and_crop_covariates_annual(covs = 'worldpop',
                                                                                       measures = pop_measure,
                                                                                       simple_polygon = simple_polygon,
                                                                                       start_year  = min(year_list),
                                                                                       end_year    = max(year_list),
                                                                                       interval_mo = 12,
                                                                                       agebin      = 1)))

## Make master list
total_periods <- length(names(results))
#regions <- get_output_regions(in_dir)

admin_level <- 0
admin0 <- shapefile(get_admin_shapefile(admin_level, version = shapefile_version))

if(diarrhea_measure=='sex_compare') {
master_list <- lapply(regions, pull_country_draws_all_admins,
                      periods = total_periods,
                      in_dir = in_dir,
                      pop_measure = pop_measure,
                      start_year = 2000,
                      end_year = 2015,
                      admin2_shapes = admin2_shapefile,
                      admin1_shapes = admin1_shapefile,
                      all_region_pops = pop_raster_annual[[1]],
                      subtract = TRUE,
                      raked = use_raked,
                      shapefile_version = shapefile_version, 
                      subtract_cell_pred = paste0('/share/geospatial/mbg/education/edu_mean_20_24/output/2017_06_18_21_21_15/edu_mean_20_24_raked_cell_draws_eb_bin0_', regions, '_0.RData'))
master_list <- do.call(c, unlist(master_list, recursive=FALSE))
}
if(diarrhea_measure!='sex_compare') {
master_list <- lapply(regions, pull_country_draws_all_admins,
                      periods = total_periods,
                      in_dir = in_dir,
                      pop_measure = pop_measure,
                      start_year = 2000,
                      end_year = 2015,
                      admin2_shapes = admin2_shapefile,
                      admin1_shapes = admin1_shapefile,
                      raked = use_raked,
                      shapefile_version = shapefile_version, 
                      all_region_pops = pop_raster_annual[[1]])
master_list <- do.call(c, unlist(master_list, recursive=FALSE))
}

new_gaul_list <- c()
for(gaul in get_adm0_codes(regions, shapefile_version = shapefile_version)) {
  if(!is.null(master_list[[paste0('list_', gaul, '.draws_', gaul)]]) & gaul != 76) {
    new_gaul_list <- c(new_gaul_list, gaul)
  }
}

dir.create(paste0(results_dir, '/table_', baseline_year, '/percent_tables'))

build_table <- function(gaul, indicator, indicator_group, run_date,
                        nperiod, master_list, results_dir,
                        baseline_year, goal_threshold,
                        diarrhea_measure='',
                        shapefile_version) {

  message(paste0('Working on ', gaul, '...'))
  #survey_year <- 2015
  #nperiod <- total_periods
  #this_period <- survey_year - 2000 + 1

  ## Load draws and simple_raster by country. Create pixel_index so we don't mess up sorting.
  cell_pred.dt <- as.data.table(master_list[[paste0('list_', gaul, '.draws_', gaul)]])
  if(diarrhea_measure != 'sex_compare') cell_pred.dt[cell_pred.dt < 0] <- 0
  names <- names(cell_pred.dt)
  cols <- names(cell_pred.dt)
  period_index <- c()
  for(i in 1:nperiod) {
    period_index <- c(period_index, rep(paste0("period_", i), length(cell_pred.dt$V1)/nperiod))
  }
  pixel_id <- c(rep(1:(length(cell_pred.dt$V1)/nperiod),
                    nperiod))
  cell_pred.dt <- cbind(cell_pred.dt, period_index, pixel_id)

  ## Summarize and subset
  #cell_pred.dt <- cell_pred.dt[period_index == paste0('period_', this_period), ]

  ## Get pops
  country_pops <- master_list[[paste0('list_', gaul, '.pops_', gaul)]]
  country_pops <- crop(country_pops, extent(master_list[[paste0('list_', gaul, '.simple_', gaul)]]))
  country_pops <- setExtent(country_pops, master_list[[paste0('list_', gaul, '.simple_', gaul)]])
  country_pops <- mask(country_pops, master_list[[paste0('list_', gaul, '.simple_', gaul)]])

  ## Get admin2 codes
  country_admin2 <- master_list[[paste0('list_', gaul, '.admin2_', gaul)]]
  country_admin2 <- crop(country_admin2, extent(master_list[[paste0('list_', gaul, '.simple_', gaul)]]))
  country_admin2 <- setExtent(country_admin2, master_list[[paste0('list_', gaul, '.simple_', gaul)]])
  country_admin2 <- mask(country_admin2, master_list[[paste0('list_', gaul, '.simple_', gaul)]])

  ## Get admin1 codes
  country_admin1 <- master_list[[paste0('list_', gaul, '.admin1_', gaul)]]
  country_admin1 <- crop(country_admin1, extent(master_list[[paste0('list_', gaul, '.simple_', gaul)]]))
  country_admin1 <- setExtent(country_admin1, master_list[[paste0('list_', gaul, '.simple_', gaul)]])
  country_admin1 <- mask(country_admin1, master_list[[paste0('list_', gaul, '.simple_', gaul)]])

  ## Get ids for all cells
  cell_idx <- seegSDM:::notMissingIdx(master_list[[paste0('list_', gaul, '.simple_', gaul)]])

  ## Make full datatable of draws with admin2 and pop info
  #geo.dt <- copy(cell_pred.dt)
  #admin2_codes <- rep(extract(country_admin2, cell_idx), nperiod)
  pull_period_pops <- function(period) {
    geo.subset <- cell_pred.dt[period_index == paste0('period_', period), ]
    geo.subset <- geo.subset[, pops := raster::extract(country_pops[[period]], cell_idx)]
    geo.subset <- geo.subset[, admin2 := raster::extract(country_admin2, cell_idx)]
    geo.subset <- geo.subset[, admin1 := raster::extract(country_admin1, cell_idx)]
    return(geo.subset)
  }
  geo.dt <- rbindlist(lapply(1:nperiod, pull_period_pops))
  geo.dt <- geo.dt[is.na(pops), pops := 0] # weighted.mean doesn't like NA weights
  geo.dt <- geo.dt[, admin0 := gaul]
  setnames(geo.dt, 'period_index', 'year')
  geo.dt <- geo.dt[, year := as.numeric(gsub('period_','',year)) + 2000 - 1]

  if(indicator == 'had_diarrhea') {
    ## Get GBD country-/year-specific MI ratio
    gbd_mort <- load_gbd_data(gbd_type = "output",
                              gbd_name = 302,
                              gaul_list = gaul,
                              measure_id = 1,
                              age_group_id = 1,
                              metric_id = 3,
                              shapefile_version = shapefile_version, 
                              year_ids = c(2000:2015))
    gbd_inc <- load_gbd_data(gbd_type = "output",
                             gbd_name = 302,
                             gaul_list = gaul,
                             measure_id = 6,
                             age_group_id = 1,
                             shapefile_version = shapefile_version, 
                             metric_id = 3)
    gbd_inc <- interpolate_gbd(gbd_inc)
    setnames(gbd_mort, 'mean', 'mort')
    setnames(gbd_inc, 'mean', 'inc')
    gbd_mi <- merge(gbd_mort, gbd_inc, by=c('name','year'))
    gbd_mi <- gbd_mi[, mi_ratio := mort / inc]
    gbd_mi <- gbd_mi[, admin0 := name]
    gbd_mi <- gbd_mi[, c('admin0', 'year', 'mi_ratio')]
    ## Convert prevalence draws to mortality draws
    geo.dt <- merge(geo.dt, gbd_mi, by=c('admin0', 'year'))
    cols <- grep("^V", names(geo.dt))
    if(diarrhea_measure=='mortality') geo.dt <- geo.dt[ , (cols) := lapply(.SD, function(x) {(x / (4.3 / 365)) * mi_ratio}), .SDcols = cols] # Moratlity, all diarrhea
    if(diarrhea_measure=='incidence') geo.dt <- geo.dt[ , (cols) := lapply(.SD, function(x) {(x / (4.3 / 365)) * 0.069}), .SDcols = cols]    # Incidence, severe diarrhea
  }

  ## Make rate of change 2010-2015 and calculate 2025 prediction given linear rate
  change.dt <- geo.dt[year == 2000 | year == 2010 | year == 2015, ]
  change.dt <- change.dt[, mi_ratio := NULL]
  change.dt <- data.table::dcast(change.dt, pixel_id + admin0 + admin1 + admin2 ~
                                   year, value.var = c('pops', grep("^V", names(geo.dt), value = T)))
  for (i in 1:length(names)) {
    ## Forecast using baseline year specified (usually 2000 or 2010)
    change.dt <- change.dt[, ((paste0("pred_", i))) :=
                             log(get(paste0("V", i, "_2015")) / get(paste0("V", i, "_", baseline_year))) / (2015 - baseline_year)]
    change.dt <- change.dt[, ((paste0("pred_", i))) := exp(get((paste0("pred_", i))) * 10) * get(paste0("V", i, "_2015"))]
    ## Percent of 2010 for each year (2000, 2010, 2015, 2025)
    change.dt <- change.dt[, ((paste0("draw", i, "_percent_2000"))) := get(paste0("V", i, "_", 2000)) / get(paste0("V", i, "_", 2010))]
    change.dt <- change.dt[, ((paste0("draw", i, "_percent_2010"))) := get(paste0("V", i, "_", 2010)) / get(paste0("V", i, "_", 2010))]
    change.dt <- change.dt[, ((paste0("draw", i, "_percent_2015"))) := get(paste0("V", i, "_", 2015)) / get(paste0("V", i, "_", 2010))]
    change.dt <- change.dt[, ((paste0("draw", i, "_percent_2025"))) := get((paste0("pred_", i))) / get(paste0("V", i, "_", 2010))]
  }
  ## Make summaries of percent of 2010
  percent.dt <- change.dt[, pops_2025 := log(pops_2015 / get(paste0('pops_', baseline_year))) / (2015 - baseline_year)]
  percent.dt <- percent.dt[, pops_2025 := exp(pops_2025 * 10) * pops_2015]
  percent.dt <- percent.dt[pops_2025=="NaN", pops_2025 := 0]
  percent.dt <- melt(change.dt, id.vars = c("admin0", "admin1", "admin2"), measure = patterns(paste0('^draw', 1:length(names), '_'), 'pops_'), variable.factor = FALSE)
  percent.dt <- percent.dt[variable == 1, year := 2000]
  percent.dt <- percent.dt[variable == 2, year := 2010]
  percent.dt <- percent.dt[variable == 3, year := 2015]
  percent.dt <- percent.dt[variable == 4, year := 2025]
  setnames(percent.dt, paste0('value', length(names)+1), 'pops')
  natl_mean_draws <- percent.dt[, lapply(.SD, weighted.mean, w=pops, na.rm=TRUE), by=c('year', 'admin0'), .SDcols=grep("^value", names(percent.dt)) ]
  admin2_mean_draws <- percent.dt[, lapply(.SD, weighted.mean, w=pops, na.rm=TRUE), by=c('year', 'admin2', 'admin1', 'admin0'), .SDcols=grep("^value", names(percent.dt)) ]
  admin1_mean_draws <- percent.dt[, lapply(.SD, weighted.mean, w=pops, na.rm=TRUE), by=c('year', 'admin1', 'admin0'), .SDcols=grep("^value", names(percent.dt)) ]
  natl_pops <- percent.dt[, lapply(.SD, sum), by=c('year', 'admin0'), .SDcols='pops']
  admin2_pops <- percent.dt[, lapply(.SD, sum), by=c('year', 'admin2', 'admin1', 'admin0'), .SDcols='pops']
  admin1_pops <- percent.dt[, lapply(.SD, sum), by=c('year', 'admin1', 'admin0'), .SDcols='pops']
  ## Make national and admin2 summaries across draws
  natl_mean_draws <- natl_mean_draws[, lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^value", names(natl_mean_draws))]
  natl_mean_draws <- natl_mean_draws[, mean := apply(.SD, 1, mean), .SDcols=grep("^value", names(natl_mean_draws))]
  natl_mean_draws <- natl_mean_draws[, upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^value", names(natl_mean_draws))]
  admin2_mean_draws <- admin2_mean_draws[, lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^value", names(admin2_mean_draws))]
  admin2_mean_draws <- admin2_mean_draws[, mean := apply(.SD, 1, mean), .SDcols=grep("^value", names(admin2_mean_draws))]
  admin2_mean_draws <- admin2_mean_draws[, upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^value", names(admin2_mean_draws))]
  admin1_mean_draws <- admin1_mean_draws[, lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^value", names(admin1_mean_draws))]
  admin1_mean_draws <- admin1_mean_draws[, mean := apply(.SD, 1, mean), .SDcols=grep("^value", names(admin1_mean_draws))]
  admin1_mean_draws <- admin1_mean_draws[, upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^value", names(admin1_mean_draws))]
  #natl_mean_draws <- natl_mean_draws[, c('mean','upper','lower','year','admin0'), with=FALSE]
  #admin2_mean_draws <- admin2_mean_draws[, c('mean','upper','lower','year','admin2', 'admin1', 'admin0'), with=FALSE]
  #admin1_mean_draws <- admin1_mean_draws[, c('mean','upper','lower','year','admin1','admin0'), with=FALSE]
  all_percents <- rbind(natl_mean_draws, admin1_mean_draws, admin2_mean_draws, fill=TRUE)
  write.csv(all_percents, paste0(results_dir, '/table_', baseline_year, '/percent_tables/', diarrhea_measure, '_', gaul, '_percent2010.csv'))

  change.dt <- change.dt[, pops := log(pops_2015 / get(paste0('pops_', baseline_year))) / (2015 - baseline_year)]
  change.dt <- change.dt[, pops := exp(pops * 10) * pops_2015]
  change.dt <- change.dt[pops=="NaN", pops := 0]
  change.dt <- change.dt[, year := 2025]
  ## 2025 predictions: make national and admin2 population-weighted means for each draw
    natl_mean_draws <- change.dt[, lapply(.SD, weighted.mean, w=pops, na.rm=TRUE), by=c('year', 'admin0'), .SDcols=grep("^pred_", names(change.dt)) ]
    admin2_mean_draws <- change.dt[, lapply(.SD, weighted.mean, w=pops, na.rm=TRUE), by=c('year', 'admin2', 'admin1', 'admin0'), .SDcols=grep("^pred_", names(change.dt)) ]
    admin1_mean_draws <- change.dt[, lapply(.SD, weighted.mean, w=pops, na.rm=TRUE), by=c('year', 'admin1', 'admin0'), .SDcols=grep("^pred_", names(change.dt)) ]
    natl_pops <- change.dt[, lapply(.SD, sum), by=c('year', 'admin0'), .SDcols='pops']
    admin2_pops <- change.dt[, lapply(.SD, sum), by=c('year', 'admin2', 'admin1', 'admin0'), .SDcols='pops']
    admin1_pops <- change.dt[, lapply(.SD, sum), by=c('year', 'admin1', 'admin0'), .SDcols='pops']
    ## Make national and admin2 summaries across draws
    natl_mean_draws <- natl_mean_draws[, lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^pred_", names(natl_mean_draws))]
    natl_mean_draws <- natl_mean_draws[, mean := apply(.SD, 1, mean), .SDcols=grep("^pred_", names(natl_mean_draws))]
    natl_mean_draws <- natl_mean_draws[, upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^pred_", names(natl_mean_draws))]
    admin2_mean_draws <- admin2_mean_draws[, lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^pred_", names(admin2_mean_draws))]
    admin2_mean_draws <- admin2_mean_draws[, mean := apply(.SD, 1, mean), .SDcols=grep("^pred_", names(admin2_mean_draws))]
    admin2_mean_draws <- admin2_mean_draws[, upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^pred_", names(admin2_mean_draws))]
    admin1_mean_draws <- admin1_mean_draws[, lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^pred_", names(admin1_mean_draws))]
    admin1_mean_draws <- admin1_mean_draws[, mean := apply(.SD, 1, mean), .SDcols=grep("^pred_", names(admin1_mean_draws))]
    admin1_mean_draws <- admin1_mean_draws[, upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^pred_", names(admin1_mean_draws))]
    natl_mean_draws <- natl_mean_draws[, c('mean','upper','lower','year','admin0'), with=FALSE]
    admin2_mean_draws <- admin2_mean_draws[, c('mean','upper','lower','year','admin2', 'admin1', 'admin0'), with=FALSE]
    admin1_mean_draws <- admin1_mean_draws[, c('mean','upper','lower','year','admin1','admin0'), with=FALSE]

    natl_mean_draws <- merge(natl_mean_draws, natl_pops, by=c('year', 'admin0'))
    admin1_mean_draws <- merge(admin1_mean_draws, admin1_pops, by=c('year', 'admin0', 'admin1'))
    admin2_mean_draws <- merge(admin2_mean_draws, admin2_pops, by=c('year', 'admin0', 'admin1', 'admin2'))

    all_2025 <- rbind(natl_mean_draws, admin1_mean_draws, admin2_mean_draws, fill=TRUE)

    pixel_means_2025 <- copy(change.dt)
    pixel_means_2025 <- pixel_means_2025[, lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^pred_", names(pixel_means_2025))]
    pixel_means_2025 <- pixel_means_2025[, mean := apply(.SD, 1, mean), .SDcols=grep("^pred_", names(pixel_means_2025))]
    pixel_means_2025 <- pixel_means_2025[, upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^pred_", names(pixel_means_2025))]
    pixel_means_2025 <- pixel_means_2025[, c('mean','upper','lower','pops','year','admin0','admin1','admin2'), with=FALSE]

  ## Make national and admin2 population-weighted means for each draw
  natl_mean_draws <- geo.dt[, lapply(.SD, weighted.mean, w=pops, na.rm=TRUE), by=c('year', 'admin0'), .SDcols=grep("^V", names(geo.dt)) ]
  admin2_mean_draws <- geo.dt[, lapply(.SD, weighted.mean, w=pops, na.rm=TRUE), by=c('year', 'admin2', 'admin1', 'admin0'), .SDcols=grep("^V", names(geo.dt)) ]
  admin1_mean_draws <- geo.dt[, lapply(.SD, weighted.mean, w=pops, na.rm=TRUE), by=c('year', 'admin1', 'admin0'), .SDcols=grep("^V", names(geo.dt)) ]
  natl_pops <- geo.dt[, lapply(.SD, sum), by=c('year', 'admin0'), .SDcols='pops']
  admin2_pops <- geo.dt[, lapply(.SD, sum), by=c('year', 'admin2', 'admin1', 'admin0'), .SDcols='pops']
  admin1_pops <- geo.dt[, lapply(.SD, sum), by=c('year', 'admin1', 'admin0'), .SDcols='pops']

  ## Make probabilities of meeting goal threshold by aggregation level
  natl_probs <- copy(natl_mean_draws)
  if(target_type == 'less') natl_probs <- natl_probs[, lapply(.SD, function(x) {ifelse(x < goal_threshold, 1, 0)}), by=c('year', 'admin0'), .SDcols=grep("^V", names(natl_probs))]
  if(target_type == 'greater') natl_probs <- natl_probs[, lapply(.SD, function(x) {ifelse(x > goal_threshold, 1, 0)}), by=c('year', 'admin0'), .SDcols=grep("^V", names(natl_probs))]
  natl_probs <- natl_probs[, p_goal := apply(.SD, 1, mean), .SDcols=grep("^V", names(natl_probs))]
  natl_probs <- natl_probs[, c('p_goal','admin0','year'), with=FALSE]
  admin1_probs <- copy(admin1_mean_draws)
  if(target_type == 'less') admin1_probs <- admin1_probs[, lapply(.SD, function(x) {ifelse(x < goal_threshold, 1, 0)}), by=c('year', 'admin0', 'admin1'), .SDcols=grep("^V", names(admin1_probs))]
  if(target_type == 'greater') admin1_probs <- admin1_probs[, lapply(.SD, function(x) {ifelse(x > goal_threshold, 1, 0)}), by=c('year', 'admin0', 'admin1'), .SDcols=grep("^V", names(admin1_probs))]
  admin1_probs <- admin1_probs[, p_goal := apply(.SD, 1, mean), .SDcols=grep("^V", names(admin1_probs))]
  admin1_probs <- admin1_probs[, c('p_goal','admin0','admin1','year'), with=FALSE]
  admin2_probs <- copy(admin2_mean_draws)
  if(target_type == 'less') admin2_probs <- admin2_probs[, lapply(.SD, function(x) {if(!is.na(x)) ifelse(x < goal_threshold, 1, 0)}), by=c('year', 'admin0', 'admin1', 'admin2'), .SDcols=grep("^V", names(admin2_probs))]
  if(target_type == 'greater') admin2_probs <- admin2_probs[, lapply(.SD, function(x) {if(!is.na(x)) ifelse(x > goal_threshold, 1, 0)}), by=c('year', 'admin0', 'admin1', 'admin2'), .SDcols=grep("^V", names(admin2_probs))]
  admin2_probs <- admin2_probs[, p_goal := apply(.SD, 1, mean), .SDcols=grep("^V", names(admin2_probs))]
  admin2_probs <- admin2_probs[, c('p_goal','admin0','admin1','admin2','year'), with=FALSE]

  ## Make national and admin2 summaries across draws
  natl_mean_draws <- natl_mean_draws[, lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^V", names(natl_mean_draws))]
  natl_mean_draws <- natl_mean_draws[, mean := apply(.SD, 1, mean), .SDcols=grep("^V", names(natl_mean_draws))]
  natl_mean_draws <- natl_mean_draws[, upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^V", names(natl_mean_draws))]
  admin2_mean_draws <- admin2_mean_draws[, lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^V", names(admin2_mean_draws))]
  admin2_mean_draws <- admin2_mean_draws[, mean := apply(.SD, 1, mean), .SDcols=grep("^V", names(admin2_mean_draws))]
  admin2_mean_draws <- admin2_mean_draws[, upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^V", names(admin2_mean_draws))]
  admin1_mean_draws <- admin1_mean_draws[, lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^V", names(admin1_mean_draws))]
  admin1_mean_draws <- admin1_mean_draws[, mean := apply(.SD, 1, mean), .SDcols=grep("^V", names(admin1_mean_draws))]
  admin1_mean_draws <- admin1_mean_draws[, upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^V", names(admin1_mean_draws))]

  natl_mean_draws <- natl_mean_draws[, c('mean','upper','lower','year','admin0'), with=FALSE]
  admin2_mean_draws <- admin2_mean_draws[, c('mean','upper','lower','year','admin2', 'admin1', 'admin0'), with=FALSE]
  admin1_mean_draws <- admin1_mean_draws[, c('mean','upper','lower','year','admin1','admin0'), with=FALSE]

  natl_mean_draws <- merge(natl_mean_draws, natl_pops, by=c('year', 'admin0'))
  natl_mean_draws <- merge(natl_mean_draws, natl_probs, by=c('year', 'admin0'))
  admin1_mean_draws <- merge(admin1_mean_draws, admin1_pops, by=c('year', 'admin0', 'admin1'))
  admin1_mean_draws <- merge(admin1_mean_draws, admin1_probs, by=c('year', 'admin0', 'admin1'))
  admin2_mean_draws <- merge(admin2_mean_draws, admin2_pops, by=c('year', 'admin0', 'admin1', 'admin2'))
  admin2_mean_draws <- merge(admin2_mean_draws, admin2_probs, by=c('year', 'admin0', 'admin1', 'admin2'))

  all_results <- rbind(natl_mean_draws, admin1_mean_draws, admin2_mean_draws, all_2025, fill=TRUE)

  ## Save pixel tables for this country
  pixel_means <- copy(geo.dt)
  pixel_means <- pixel_means[year == 2015, ]
  ## Make probabilities of meeting goal threshold by pixel
  classify_pixel <- function(x) {
      if(target_type == 'less') {
        x[!is.na(x) & x > goal_threshold] <- 0
        x[!is.na(x) & x != 0 & x < goal_threshold] <- 1
      }
      if(target_type == 'greater') {
        x[!is.na(x) & x < goal_threshold] <- 0
        x[!is.na(x) & x != 0 & x > goal_threshold] <- 1
      }
    return(x)
  }
  pixel_probs <- pixel_means[, lapply(.SD, classify_pixel), .SDcols=grep("^V", names(pixel_means))]
  pixel_probs <- pixel_probs[, p_goal := apply(.SD, 1, mean), .SDcols=grep("^V", names(pixel_probs))]
  pixel_probs <- pixel_probs[, c('p_goal'), with=FALSE]
  write.csv(pixel_probs, paste0(results_dir, '/table_', baseline_year, '/pixels/', diarrhea_measure, '_', gaul, '_probs.csv'))
  dir.create(paste0(results_dir, '/table_', baseline_year, '/simple/'))
  writeRaster(
    master_list[[paste0('list_', gaul, '.simple_', gaul)]],
    file = paste0(results_dir, '/table_', baseline_year, '/simple/', gaul),
    format='GTiff',
    overwrite = TRUE
  )
  ## Make summaries
  pixel_means <- pixel_means[, lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^V", names(pixel_means))]
  pixel_means <- pixel_means[, mean := apply(.SD, 1, mean), .SDcols=grep("^V", names(pixel_means))]
  pixel_means <- pixel_means[, upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^V", names(pixel_means))]
  pixel_means <- pixel_means[, c('mean','upper','lower','pops','year','admin0','admin1','admin2'), with=FALSE]
  all_pixel_means <- rbind(pixel_means, pixel_means_2025)
  write.csv(all_pixel_means, paste0(results_dir, '/table_', baseline_year, '/pixels/', diarrhea_measure, '_', gaul, '.csv'))

  ## Return admin summaries
  return(all_results)

}

dir.create(paste0(results_dir, '/table_', baseline_year))
dir.create(paste0(results_dir, '/table_', baseline_year, '/pixels'))
full_table <- rbindlist(lapply(new_gaul_list, build_table,
                               indicator = indicator,
                               indicator_group = indicator_group,
                               run_date = run_date,
                               nperiod = 16,
                               shapefile_version = shapefile_version, 
                               master_list = master_list,
                               results_dir = results_dir,
                               baseline_year = baseline_year,
                               goal_threshold = goal_threshold,
                               diarrhea_measure = diarrhea_measure))
write.csv(full_table, paste0(results_dir, '/table_', baseline_year, '/', diarrhea_measure, '_', regions, '.csv'))

