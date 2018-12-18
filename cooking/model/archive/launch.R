###############################################################################
###############################################################################
## Basic Launch script for Nov 2017 MBG intensive. RB
##
###############################################################################
###############################################################################


###############################################################################
## SETUP
###############################################################################

## clear environment
rm(list=ls())

## Set core_repo location and indicator group
user              <- Sys.info()['user']
code_dir         <- '/homes/jfrostad/_code/lbd'
core_repo         <- file.path(code_dir, '/lbd_core/')
my_repo         <- file.path(code_dir, '/housing/model')
remote            <- 'origin'
branch            <- 'master'
indicator_group   <- 'hap'
indicator         <- 'cooking_fuel'
pullgit           <- TRUE
keep_run_date     <- T
last_run_date     <- "2018_09_05_10_02_47"

## sort some directory stuff and pull newest code into share
if(pullgit) system(sprintf('cd %s\ngit pull %s %s', core_repo, remote, branch))

sharedir       <- paste('/share/geospatial/mbg', indicator_group, indicator, sep = '/')
commondir      <- paste(core_repo, 'mbg_central/share_scripts/common_inputs', sep = '/')
package_list <- c(t(read.csv(paste(commondir, 'package_list.csv', sep = '/'), header = FALSE)))

#this dir is necessary to save model images
file.path(sharedir, 'model_image_history') %>% dir.create(recursive = T)

# Load MBG packages and functions
message('Loading in required R packages and MBG functions')
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)
# In addition to mbg_central functions, functions in '/share/code/geospatial/lbd_core/training'
# can be loaded in as well by doing:
# mbg_setup(package_list = package_list, repos = c(core_repo, '/share/code/geospatial/lbd_core/training'))

# Load custom functions
source(paste0(my_repo, '/_lib/fx.R'))

#GBD functions for getting IDs
gbd.function.dir <- file.path("/home/j/temp/central_comp/libraries/current/r")
file.path(gbd.function.dir, "get_ids.R") %>% source
#risk_ids <- get_ids('rei')
gbd_id <- 'pollution_indoor_total_prev' #(all solid fuels)

## Read config file and save all parameters in memory
config <- load_config(repo            = my_repo,
                      indicator_group = indicator_group,
                      indicator       = indicator,
                      ## config_name     = 'config_training_gp',
                      ## config_name     = 'config_training_stack',
                      config_name     = 'config',
                      ## config_name     = 'config_training_raw',
                      ## config_name     = 'config_training_raw_oos',
                      ## config_name     = 'config_training_raw_gp',
                      ## config_name     = 'config_training_stack_gp_tmb',
                      covs_name       = 'covs')

## Ensure you have defined all necessary settings in your config
check_config()


## Create proper year list object
if (class(year_list) == "character") year_list <- eval(parse(text=year_list))

## Create run date in correct format
if(keep_run_date==T) { run_date <- last_run_date
} else run_date <- make_time_stamp(TRUE)

## Create output folder with the run_date
outputdir      <- paste('/share/geospatial/mbg', indicator_group, indicator, 'output', run_date, '', sep='/')
dir.create(outputdir)

## Create directory structure for this model run
create_dirs(indicator_group = indicator_group, indicator = indicator)

## Print the core_repo hash and check it
message("Printing git hash for 'core_repo' and checking against LBD Core Code master repo")
record_git_status(core_repo = core_repo, check_core_repo = TRUE)

## Create a few objects from the config file loaded above
if (class(Regions) == "character" & length(Regions) == 1) Regions <- eval(parse(text=Regions))
if (class(year_list) == "character") year_list <- eval(parse(text=year_list))
if (length(summstats) == 1 & grepl(",", summstats)) summstats <- eval(parse(text=summstats))

## Load gaul list
gaul_list <- get_gaul_codes(Regions)

## If running individual countries, get set up
if (individual_countries == TRUE) {
  # Convert all Regions to individual countries
  Regions <- get_individual_countries(gaul_list)

  # Turn off all FEs
  use_child_country_fes <- FALSE
  use_inla_country_fes  <- FALSE
  use_inla_country_res  <- FALSE
}

###############################################################################
## Make Holdouts
###############################################################################
if(makeholdouts){
  # load the full input data
  df <- load_input_data(indicator   = indicator,
                        simple      = NULL,
                        removeyemen = TRUE,
                        years       = yearload,
                        yl          = year_list,
                        withtag     = as.logical(withtag),
                        datatag     = datatag,
                        use_share   = as.logical(use_share))

  # add in location information
  df <- merge_with_ihme_loc(df)

  # make a list of dfs for each region, with 5 qt folds identified in each
  stratum_ho <- make_folds(data       = df,
                           n_folds    = as.numeric(n_ho_folds),
                           spat_strat = 'qt',
                           temp_strat = 'prop',
                           strat_cols = 'region',
                           ts         = as.numeric(ho_ts),
                           mb         = as.numeric(ho_mb))
}


###############################################################################
## Launch Parallel Script
###############################################################################

## Make loopvars aka strata grid (format = regions, ages, holdouts)
if(makeholdouts) loopvars <- expand.grid(Regions, 0, 0:n_ho_folds) else loopvars <- expand.grid(Regions, 0, 0)

## loop over them, save images and submit qsubs
for(i in 1:nrow(loopvars)){

  message(paste(loopvars[i,2],as.character(loopvars[i,1]),loopvars[i,3]))

  # make a qsub string
  #TODO this fx breaks if havent created model_image_history dir in sharedir, could happen recursively?
  qsub <- make_qsub_share(age           = loopvars[i,2],
                          reg           = as.character(loopvars[i,1]),
                          holdout       = loopvars[i,3],
                          test          = F,
                          indic         = indicator,
                          saveimage     = TRUE,
                          #TODO jn doesnt seem to be defined anywhere else in this file??
                          #TODO removed eval parse, jn is read in as string when placed in config
                          #addl_job_name = jn, ## from config
                          #code = "xxx" #use to run a custom parallel model from your indicator specific folder
                          memory        = 10,
                          cores         = 10,
                          singularity   = "default",
                          singularity_opts = list(SET_OMP_THREADS=1, SET_MKL_THREADS=10),
                          addl_job_name = 'hap_model',
                          code = 'parallel_mod_savecov',
                          geo_nodes     = F)

  system(qsub)
  
}


## check to make sure models are done before continuing
waitformodelstofinish(lv = cbind(as.character(loopvars[,1]),loopvars[,3]),sleeptime=60)

##############################################################################
## Summarize model results
##############################################################################

clean_model_results_table()

###############################################################################
## Post-Estimation
###############################################################################

## Save strata for Shiny to use in producing aggregated fit statistics
strata <- unique(as.character(loopvars[,1]))
dir.create(paste0(sharedir, '/fit_stats'))
save(strata, file = paste0(sharedir, '/fit_stats/strata.RData'))

## Load GBD Estimates for this indicator which will be used in raking
gbd <- load_gbd_data2(gbd_type     = "covariate",
                     gbd_name     = gbd_id,
                     gaul_list    = get_gaul_codes('per'),
                     measure_id   = 5,
                     age_group_id = 1,
                     metric_id    = 3,
                     year_ids     = year_list)

# Prepare for parallel post-estimation - save file with objs to re-load in child processes
prep_postest(indicator = indicator,
             indicator_group = indicator_group,
             run_date = run_date,
             save_objs = c("core_repo", "gbd", "year_list", "summstats",
                           "rake_transform", "pop_measure"))

## Parallelized post-estimation over region
postest_script <- "postest_script"

for (s in strata) {
  qsub <- make_qsub_postest(code = postest_script,
                            stratum = s,
                            log_location = 'sharedir',
                            memory        = 10,
                            singularity   = "default",
                            geo_nodes     = F,
                            cores         = 10)
  system(qsub)
}

## check to make sure post-est done before continuing
waitformodelstofinish(lv = cbind(strata, 0), sleeptime=60)

## Combine post est stuff across regions and save needed outputs
post_load_combine_save(summstats = summstats)

# Clean up / delete unnecessary files
clean_after_postest(indicator             = indicator,
                    indicator_group       = indicator_group,
                    run_date              = run_date,
                    strata                = strata,
                    delete_region_rasters = F)

###############################################################################
## Launch model diagnostics script for shiny tool
###############################################################################
# View results at https://shiny.ihme.washington.edu/connect/#/apps/119/

make_model_diagnostics(indic        = indicator,
                       ig           = indicator_group,
                       log_location = 'sharedir',
                       rd           = run_date,
                       geo_nodes    = F,
                       cores        = 10,
                       singularity  ='default')

###############################################################################
## Aggregate to admin2, admin1, and national levels
###############################################################################

submit_aggregation_script(indicator       = indicator,
                          indicator_group = indicator_group,
                          run_date        = run_date,
                          raked           = c(TRUE,FALSE),
                          pop_measure     = pop_measure,
                          overwrite       = T,
                          ages            = 0, # Note: can take vector of ages
                          holdouts        = 0,
                          regions         = strata,
                          corerepo        = core_repo,
                          log_dir         = paste0(sharedir, "/output/", run_date, "/"),
                          geo_nodes       = F,
                          singularity     = "default",
                          slots           = 8)

waitforaggregation(rd = run_date, indic = indicator, ig = indicator_group,
                   ages     = 0,
                   regions  = strata,
                   holdouts = 0,
                   raked    = c(T, F))

combine_aggregation(rd = run_date, indic = indicator, ig = indicator_group,
                    ages     = 0,
                    regions  = strata,
                    holdouts = 0,
                    raked    = c(T, F))

summarize_admins(summstats = c("mean", "upper", "lower", "cirange"),
                 ad_levels = c(0,1,2),
                 raked     = c(T,F))
message("After summarize_admins - about to save CSV")

# Combine csv files
csvs <- list.files(paste0(sharedir, '/output/', run_date, '/'),
                   pattern = "input_data(.*).csv",
                   full.names = T)

csv_master <- lapply(csvs, fread) %>%
  rbindlist %>%
  subset(., select = names(.) != "V1")
write.csv(csv_master, file=paste0(sharedir, '/output/', run_date, '/input_data.csv'))

###############################################################################
## Make time series plots
###############################################################################
message('Making time series plots by admin unit')

## Set run_date, indicator, indicator_group, out_dir per your preferences
in_dir <- paste0(outputdir, '/pred_derivatives/admin_summaries/')
in_file_ad0 <- paste0(in_dir, indicator, '_admin_0_unraked_summary.csv')
in_file_ad1 <- paste0(in_dir, indicator, '_admin_1_unraked_summary.csv')
in_file_ad2 <- paste0(in_dir, indicator, '_admin_2_unraked_summary.csv')

## Prepare input predictions
ad0_df <- fread(in_file_ad0)
ad1_df <- fread(in_file_ad1)
ad2_df <- fread(in_file_ad2)

## Prepare input data
input_data <- fread(paste0('/share/geospatial/mbg/input_data/', indicator, '.csv'))
setnames(input_data, 'survey_series', 'source')
input_data[, point := !polygon]
admin_data <- input_aggregate_admin2(indicator = indicator, 
                                    indicator_group = indicator_group, 
                                    regions = Regions, 
                                    run_date = run_date,
                                    input_data = input_data)
save(admin_data, file = paste0(outputdir, 'input_data_aggregated_admin012.RData'))
ad0_data <- admin_data$ad0
ad1_data <- admin_data$ad1
ad2_data <- admin_data$ad2

## Create output directory
dir.create(paste0(outputdir, '/time_series_plots/'))

## Run the plotting code
subnational_ts_plots2(ad0_df = ad0_df,
                     ad1_df = ad1_df,
                     ad2_df = ad2_df,
                     ind_title = indicator,
                     out_dir = paste0(outputdir, '/time_series_plots/'),
                     highisbad = F,
                     verbose = T,
                     plot_levels = ifelse(individual_countries, c('ad1', 'ad2'), c('ad0', 'ad1', 'ad2')),
                     plot_data = T,
                     ad0_data = ad0_data,
                     ad1_data = ad1_data,
                     ad2_data = ad2_data)


###############################################################################
## Plot covariates and stackers
###############################################################################

if (use_stacking_covs) {
  
  ## Create output directory
  dir.create(paste0(outputdir, '/diagnostic_plots/'))
  
  ## Covariate importance plots
  source(paste0(repo, "4_post_estimation/get_cov_weights.R"))
  get_cov_weights(indicator, indicator_group, run_date, Regions, outputdir)
  
  ## Stackers over time aggregated to admins
  source(paste0(repo, '4_post_estimation/plot_admin_time_series.R'))
  plot_stackers_by_adm01(repo, indicator, indicator_group, run_date, Regions, outputdir)
  
}


###############################################################################
## Make summart metrics and validation plots
###############################################################################

## Get in and out of sample draws
run_in_oos <- get_is_oos_draws(ind_gp        = indicator_group,
                               ind           = indicator,
                               rd            = run_date,
                               ind_fm        = 'binomial',
                               model_domain  = 'south_america', # <- ?
                               age           = 0,
                               nperiod       = length(year_list),
                               yrs           = year_list,
                               get.oos       = as.logical(makeholdouts),
                               year_col      = 'year',
                               write.to.file = TRUE)

## Set out_dir
dir.create(paste0(outputdir, '/summary_metrics/'), recursive = T, showWarnings = F)

## Calculate and save PV summary statistics
draws.df <- fread(paste0(outputdir, '/output_draws_data.csv'))
# country summary stats
pvtab_admin0 <- get_pv_table(d               = draws.df, # <- make everything below here into a function
                             indicator_group = indicator_group,
                             rd              = run_date,
                             indicator       = indicator,
                             result_agg_over = c('year', 'oos', 'region'),
                             aggregate_on    = 'country',
                             draws           = as.numeric(samples),
                             out.dir         = paste0(outputdir, '/summary_metrics/'))
# admin 1 summary stats
pvtab_admin1 <-  get_pv_table(d               = draws.df,
                              indicator_group = indicator_group,
                              rd              = run_date,
                              indicator       = indicator,
                              result_agg_over = c('year', 'oos', 'region'),
                              aggregate_on    = 'ad1',
                              draws           = as.numeric(samples),
                              out.dir         = paste0(outputdir, '/summary_metrics/'))
# admin 2 summary stats
pvtab_admin2 <-  get_pv_table(d               = draws.df,
                              indicator_group = indicator_group,
                              rd              = run_date,
                              indicator       = indicator,
                              result_agg_over = c('year', 'oos', 'region'),
                              aggregate_on    = 'ad2',
                              draws           = as.numeric(samples),
                              out.dir         = paste0(outputdir, '/summary_metrics/'))
# combine into one file
pvtab <- rbindlist(list(pvtab_admin0[[1]][, Level := names(pvtab_country)], 
                        pvtab_admin1[[1]][, Level := names(pvtab_admin1)], 
                        pvtab_admin2[[1]][, Level := names(pvtab_admin2)]), use.names = TRUE)
write.csv(pvtab, paste0(outputdir, '/summary_metrics/pv_metrics.csv'), row.names = FALSE)

## Validation results
source(paste0(indic_repo, '4_post_estimation/plot_pv_results.r'))
plot_pv_results(indicator, indicator_group, run_date, 
                save_file = paste0('/share/geospatial/mbg/ort/ors/output/', run_date, '/summary_metrics/pv_metrics.pdf'))