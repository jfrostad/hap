##############################################################################
## MBG diagnostics functions and plots for ORT
## Written by Kirsten Wiens
## Created 2018/09/17
#source("/homes/jfrostad/_code/lbd/hap/diagnostics/02_diagnostics.R", echo=T)
##############################################################################


## Setup -------------------------------------------------------------------------

## clear environment
rm(list=ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j/"
  h_root <- file.path("/ihme/homes", Sys.info()["user"])
  
  package_lib    <- file.path(h_root, '_code/_lib/pkg')
  ## Load libraries and  MBG project functions.
  .libPaths(package_lib)
  
  # necessary to set this option in order to read in a non-english character shapefile on a linux system (cluster)
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
} else {
  j_root <- "J:"
  h_root <- "H:"
}

#load external packages
#TODO request adds to lbd singularity
pacman::p_load(magrittr, mgsub)

#running interactively?
debug <- T
debug.args <- c('simulate',
                'command',
                'args',
                'jfrostad',
                "/homes/jfrostad/_code/lbd/hap",
                'cooking',
                'cooking_fuel_solid',
                'config_hap_best',
                'cooking/model/configs/',
                'covs_cooking_dia_cssa',
                'cooking/model/configs/',
                '2019_07_17_09_37_38',
                'total')

#pull args from the job submission if !interactive
args <- ifelse(debug %>% rep(., length(debug.args)), debug.args, commandArgs()) 

## Set repo location, indicator group, and some arguments
user            <- args[4]
core_repo       <- args[5]
indicator_group <- args[6]
indicator       <- args[7]
config_par      <- args[8]
config_file     <- args[9]
cov_par         <- args[10]
cov_file        <- args[11]

message(indicator)

## Load MBG packages
package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

#use your own diacritics fx, due to inscrutable error
#note: requires mgsub pkg
#TODO submit PR
fix_diacritics <<- function(x) {
  
  #first define replacement patterns as a named list
  defs <-
    list('??'='S', '??'='s', '??'='Z', '??'='z', '??'='A', '??'='A', '??'='A', '??'='A', '??'='A', '??'='A', '??'='A', 
         '??'='C', '??'='E', '??'='E','??'='E', '??'='E', '??'='I', '??'='I', '??'='I', '??'='I', '??'='N', '??'='O', 
         '??'='O', '??'='O', '??'='O', '??'='O', '??'='O', '??'='U','??'='U', '??'='U', '??'='U', '??'='Y', '??'='B', 
         '??'='a', '??'='a', '??'='a', '??'='a', '??'='a', '??'='a', '??'='a', '??'='c','??'='e', '??'='e', '??'='e', 
         '??'='e', '??'='i', '??'='i', '??'='i', '??'='i', '??'='o', '??'='n', '??'='o', '??'='o', '??'='o', '??'='o',
         '??'='o', '??'='o', '??'='u', '??'='u', '??'='u', '??'='y', '??'='y', '??'='b', '??'='y', '??'='Ss')
  
  #then force conversion to UTF-8 and replace with non-diacritic character
  enc2utf8(x) %>% 
    mgsub(., pattern=enc2utf8(names(defs)), replacement = defs) %>% 
    return
  
}

## Throw a check for things that are going to be needed later
message('Looking for things in the config that will be needed for this script to run properly')

## Read config file and save all parameters in memory
config <- set_up_config(repo            = core_repo,
                        indicator_group = '',
                        indicator       = '',
                        config_name     = paste0(config_file, config_par),
                        covs_name       = paste0(cov_file, cov_par))

## Set run date(s)
run_date <- args[12]

## Set measure
measure <- args[13]

## Create output folder with the run_date
outputdir      <- paste('/share/geospatial/mbg', indicator_group, indicator, 'output', run_date, '', sep='/')

## Create proper year list object
if (class(year_list) == 'character') year_list <- eval(parse(text=year_list))

## Get regions that have successfully completed through aggregation step
Regions <- list.files(outputdir, pattern = paste0(ifelse(indicator == 'had_diarrhea', measure, 'unraked'),'*_admin_draws'))
Regions <- gsub('.*eb_bin0_', '', Regions)
for (r in 1:length(Regions)) Regions[[r]] <- substr(Regions[[r]], start = 1, stop = nchar(Regions)[[r]]-8)
Regions <- unique(Regions)
Regions <- Regions[Regions != '']
message(paste0(Regions, '\n'))

## Set holdout to 0 because for now we'll just run the cleaning and stacker line plots on the full model
holdouts <- 0

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
                    metrics = 'rates', #TODO do rates apply to HAP?
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