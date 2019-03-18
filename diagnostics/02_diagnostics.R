##############################################################################
## MBG diagnostics functions and plots for ORT
## Written by Kirsten Wiens
## Created 2018/09/17
#source("/homes/jfrostad/_code/lbd/hap/diagnostics/02_diagnostics.R", echo=T)
##############################################################################


## Setup -------------------------------------------------------------------------

## clear environment
rm(list=ls())

#running interactively?
debug <- T
debug.args <- c('simulate',
                'command',
                'args',
                'jfrostad',
                "/homes/jfrostad/_code/lbd/hap",
                'cooking',
                'cooking_fuel_solid',
                'config_ort_best',
                'cooking/model/configs/',
                'covs_ort_standard',
                'cooking/model/configs/',
                '2019_03_04_15_42_42',
                FALSE,
                FALSE,
                FALSE,
                'total')

## Set repo location, indicator group, and some arguments
if (debug!=T) {
  
  ## Set repo location, indicator group, and some arguments
  user            <- commandArgs()[4]
  core_repo       <- commandArgs()[5]
  indicator_group <- commandArgs()[6]
  indicator       <- commandArgs()[7]
  config_par      <- commandArgs()[8]
  config_file     <- commandArgs()[9]
  cov_par         <- commandArgs()[10]
  cov_file        <- commandArgs()[11]

} else {
  
  user            <- debug.args[4]
  core_repo       <- debug.args[5]
  indicator_group <- debug.args[6]
  indicator       <- debug.args[7]
  config_par      <- debug.args[8]
  config_file     <- debug.args[9]
  cov_par         <- debug.args[10]
  cov_file        <- debug.args[11]
  
}

message(indicator)

## Load MBG packages
package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

## Throw a check for things that are going to be needed later
message('Looking for things in the config that will be needed for this script to run properly')

## Read config file and save all parameters in memory
config <- load_config(repo            = core_repo,
                      indicator_group = '',
                      indicator       = '',
                      config_name     = paste0(config_file, config_par),
                      covs_name       = paste0(cov_file, cov_par))

## Set run date(s)
run_date <- ifelse(!debug, commandArgs()[12], debug.args[12])
multi_run_dates <- ifelse(!debug, commandArgs()[13], debug.args[13])

## Create output folder with the run_date
outputdir      <- paste('/share/geospatial/mbg', indicator_group, indicator, 'output', run_date, '', sep='/')

## Create proper year list object
if (class(year_list) == 'character') year_list <- eval(parse(text=year_list))

## Ensure you have defined all necessary settings in your config
check_config()

## If running individual countries make sure all country FEs and REs off
individual_countries <- ifelse(!debug, commandArgs()[14], debug.args[14])
if (individual_countries) {
  use_child_country_fes <- FALSE
  use_inla_country_fes  <- FALSE
  use_inla_country_res  <- FALSE
}

## Get regions
makeholdouts <- ifelse(!debug, commandArgs()[15], debug.args[15])
holdouts <- ifelse(makeholdouts == TRUE, 1, 0)
Regions <- list.files(outputdir, pattern = 'unraked*_admin_draws')
Regions <- gsub(paste0(indicator, '_unraked_admin_draws_eb_bin0_'), '', Regions)
Regions <- gsub(paste0('_', holdouts, '.RData'), '', Regions)
Regions = Regions[!(Regions %in% paste0(holdouts, '.RData'))]

## Set measure
measure <- ifelse(!debug, commandArgs()[16], debug.args[16])

## Combine and summarize aggregated results --------------------------

# combine unraked results
message('Combining unraked aggregated results')
combine_aggregation(rd       = run_date,
                    indic    = indicator,
                    ig       = indicator_group,
                    ages     = 0,
                    regions  = Regions,
                    holdouts = holdouts,
                    raked    = F,
                    delete_region_files = F)

# summarize admins
summarize_admins(ad_levels = c(0,1,2), raked = F)

# combine raked results
# TODO currently irrelevant
# message('Combining raked aggregated results')
# if (indicator == 'had_diarrhea') {
#   combine_aggregation(rd       = run_date,
#                       indic    = indicator,
#                       ig       = indicator_group,
#                       ages     = 0,
#                       regions  = Regions,
#                       holdouts = holdouts,
#                       raked    = T,
#                       measure  = measure,
#                       delete_region_files = F)
#   
#   # summarize admins
#   summarize_admins(ad_levels = c(0,1,2), raked = T, measure = measure)
# }


## Plot stackers and covariates ------------------------------------------------------

message('Making time series plots for stackers by admin unit')
dir.create(paste0(outputdir, '/diagnostic_plots/'))

if (use_stacking_covs) {

  ## Stackers over time aggregated to admins
  #TODO add source / points columns to input dataset
  source(file.path(core_repo, 'diagnostics/03_stacker_admin_time_series.R'))
  plot_stackers_by_adm01(indicator, 
                         indicator_group, 
                         run_date, 
                         Regions,
                         measure = measure,
                         draws = T,
                         raked = ifelse(indicator == 'had_diarrhea', T, F),
                         credible_interval = 0.95,
                         N_breaks = c(0, 10, 50, 100, 500, 1000, 2000, 4000))
}

stop("this is all so far")

# Plot priors for spatial hyperparameters --------------------------------------------------

message('Plotting spatial hyperparameters prior and posteriors')

if (measure == 'prevalence') {
  
  # Plot a la HIV team
  source(paste0(core_repo, 'diagnostics/04_plot_hyperparameters.R'))
  plot_hyperparameters(indicator = indicator, 
                       indicator_group = indicator_group, 
                       run_date = run_date, 
                       age = 0, 
                       holdout = holdouts, 
                       save_file = NULL,
                       regs = Regions)
  
  # Plot a la Chris
  source(paste0(core_repo, 'diagnostics/05_plot_priors_chris.R'))
  for (region in Regions) {
    plot_spatial_priors(indicator,
                        indicator_group,
                        run_date,
                        region,
                        holdout = holdouts,
                        age = 0)
  }
  
}


# Global diagnostics -------------------------------

# if global model is finished
if (length(Regions) == 14) {
  
  
  # Make global results map --------------------------------------------------
  
  library('png')
  library('gridExtra')
  
  message('Plotting global maps')
  source(paste0(core_repo, 'diagnostics/06_plot_global_map.R'))
  
  # set arguments
  raked_map <- ifelse(indicator == 'had_diarrhea', T, F)
  raked_measure_map <- ifelse(indicator == 'had_diarrhea', measure, NULL)
  map_years <- c(2000, 2005, 2010, 2017)
  map_levels <- c('admin1', 'admin2')
  
  # map mean model results
  map_model_results(indicator,
                    indicator_group,
                    run_date,
                    type = 'mean',
                    raked = raked_map,
                    lvl_years = map_years,
                    lvl_colors = 'magma',
                    lvl_limits = c(0, 1),
                    include_diff = TRUE,
                    geo_levels = map_levels,
                    plot_by_year = TRUE,
                    plot_combined = FALSE,
                    file_type = 'png',
                    raked_measure = raked_measure_map)
  
  # map uppper model results
  map_model_results(indicator,
                    indicator_group,
                    run_date,
                    type = 'upper',
                    raked = raked_map,
                    lvl_years = map_years,
                    lvl_colors = 'magma',
                    lvl_limits = c(0, 1),
                    include_diff = TRUE,
                    geo_levels = map_levels,
                    plot_by_year = TRUE,
                    plot_combined = FALSE,
                    file_type = 'png',
                    raked_measure = raked_measure_map)
  
  # map lower model results
  map_model_results(indicator,
                    indicator_group,
                    run_date,
                    type = 'lower',
                    raked = raked_map,
                    lvl_years = map_years,
                    lvl_colors = 'magma',
                    lvl_limits = c(0, 1),
                    include_diff = TRUE,
                    geo_levels = map_levels,
                    plot_by_year = TRUE,
                    plot_combined = FALSE,
                    file_type = 'png',
                    raked_measure = raked_measure_map)
  
  # function to combine mean, upper, and lower into one pdf
  combine_plots <- function(ad, years) {
    # setup plot
    pdf(file = paste0(outputdir, 'results_maps/', indicator, if (raked_map) '_raked_' else '_unraked_', ad, '.pdf'), width = 8, height = 12)
    # loop over years
    for (yr in years) {
      # load rasters
      img1 <- readPNG(paste0(outputdir, 'results_maps/', indicator, '_upper', if (raked_map) '_raked_' else '_unraked_', ifelse(is.null(raked_measure), '', paste0(raked_measure, '_')), ad, '_', yr, '.png'))
      img2 <- readPNG(paste0(outputdir, 'results_maps/', indicator, '_mean', if (raked_map) '_raked_' else '_unraked_', ifelse(is.null(raked_measure), '', paste0(raked_measure, '_')), ad, '_', yr, '.png'))
      img3 <- readPNG(paste0(outputdir, 'results_maps/', indicator, '_lower', if (raked_map) '_raked_' else '_unraked_', ifelse(is.null(raked_measure), '', paste0(raked_measure, '_')), ad, '_', yr, '.png'))
      img_list <- list(img1, img2, img3)
      # arrange plot
      gl = lapply(img_list, rasterGrob)
      grid.arrange(grobs=gl)
    }
    # finish plot
    dev.off()
  } # end function
  
  # apply function to all admin levels
  for (lvl in map_levels) {
    combine_plots(ad = lvl, years = map_years)
  }
  
  
  # Make model fit statistics --------------------------------------------------
  
  # fit statistics for prevalence
  if (measure == 'prevalence') {
    
    # Combine csv files
    csvs <- list.files(outputdir, pattern = 'input_data_(.*).csv', full.names = T)
    csv_master <- rbindlist(lapply(csvs, fread))
    csv_master[, V1 := NULL]
    write.csv(csv_master, file=paste0(outputdir, '/input_data.csv'))
    
    # Get in and out of sample draws
    run_in_oos <- get_is_oos_draws(ind_gp        = indicator_group,
                                   ind           = indicator,
                                   rd            = run_date,
                                   ind_fm        = 'binomial',
                                   model_domain  = 'africa',
                                   age           = 0,
                                   nperiod       = length(year_list),
                                   yrs           = year_list,
                                   get.oos       = as.logical(makeholdouts),
                                   year_col      = 'year',
                                   write.to.file = TRUE,
                                   shapefile_version = modeling_shapefile_version)
    
    # Set out_dir
    dir.create(paste0(outputdir, '/summary_metrics/'), recursive = T, showWarnings = F)
    
    # Calculate and save PV summary statistics
    draws.df <- fread(paste0(outputdir, '/output_draws_data.csv'))
    pvtab <- rbindlist(lapply(list(c('oos'), c('oos', 'year'), c('oos', 'region')), function(results_by) {
      pv <- get_pv_table(d               = draws.df,
                         indicator_group = indicator_group,
                         rd              = run_date,
                         indicator       = indicator,
                         aggregate_on    = c('country', 'ad1', 'ad2'),
                         result_agg_over = results_by,
                         coverage_probs  = c(25, 50, 75, 90, 95),
                         plot_ci         = TRUE,
                         draws           = as.numeric(samples),
                         save_csv        = FALSE,
                         out.dir         = paste0(outputdir, '/summary_metrics/'))
      ldply(pv, .id = 'Level')
    }), fill = T)
    write.csv(pvtab, file = paste0(outputdir, '/summary_metrics/pv_metrics.csv'), row.names=F)
    
    # plot results
    source(paste0(core_repo, 'diagnostics/07_plot_pv_results.R'))
    plot_pv_results(indicator, 
                    indicator_group, 
                    run_dates = run_date,
                    save_file = paste0(outputdir, '/summary_metrics/pv_metrics.pdf'))
  }
  
}