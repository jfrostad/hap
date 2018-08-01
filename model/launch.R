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
pullgit           <- FALSE

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

## Read config file and save all parameters in memory
config <- load_config(repo            = my_repo,
                      indicator_group = indicator_group,
                      indicator       = indicator,
                      ## config_name     = 'config_training_gp',
                      ## config_name     = 'config_training_stack',
                      config_name     = 'config_ors',
                      ## config_name     = 'config_training_raw',
                      ## config_name     = 'config_training_raw_oos',
                      ## config_name     = 'config_training_raw_gp',
                      ## config_name     = 'config_training_stack_gp_tmb',
                      covs_name       = 'covs_ors')

## Ensure you have defined all necessary settings in your config
check_config()

## Create run date in correct format
run_date <- make_time_stamp(TRUE)

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
                          memory        = 10,
                          cores         = 10,
                          singularity   = "default",
                          geo_nodes     = TRUE)

  system(qsub)
}


## check to make sure models are done before continuing
waitformodelstofinish(lv = cbind(as.character(loopvars[,1]),loopvars[,3]),sleeptime=60)

##############################################################################
## Summarize model results
##############################################################################

clean_model_results_table()

# Stop if individual countries (no need for post-est)
if(as.logical(individual_countries) == F) {

  ###############################################################################
  ## Post-Estimation
  ###############################################################################

  ## Save strata for Shiny to use in producing aggregated fit statistics
  strata <- unique(as.character(loopvars[,1]))
  dir.create(paste0(sharedir, '/fit_stats'))
  save(strata, file = paste0(sharedir, '/fit_stats/strata.RData'))

  ## Load GBD Estimates for this indicator which will be used in raking
  gbd <- load_gbd_data(gbd_type     = "output",
                       gbd_name     = 302,
                       gaul_list    = get_gaul_codes('africa'),
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
                              geo_nodes     = TRUE,
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
                         geo_nodes    = TRUE,
                         cores        = 10)

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
                            geo_nodes       = TRUE,
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
  ## Create AROC objects & do projections
  ###############################################################################

  make_aroc(ind_gp           = indicator_group,
            ind              = indicator,
            rd               = run_date,
            matrix_pred_name = NULL,
            type             = c("cell", "admin"),
            measure          = "prevalence",
            year_list        = c(2000:2015),
            uselogit         = FALSE,
            raked            = FALSE,
            weighting_res    = 'domain',
            weighting_type   = 'exponential',
            pow              = 1,
            input_data = read.csv(sprintf('/share/geospatial/mbg/%s/%s/output/%s/input_data.csv',
                                          indicator_group, indicator, run_date)),
            mult_emp_exp     = FALSE,
            extra_file_tag = "_exp_domain")

  make_proj(ind_gp     = indicator_group,
            ind        = indicator,
            rd         = run_date,
            type       = c("cell", "admin"),
            proj_years = c(2020, 2025, 2030),
            measure    = "prevalence",
            skip_cols  = NULL,
            year_list  = c(2000:2015),
            uselogit   = FALSE,
            extra_file_tag = "_exp_domain")

  ###############################################################################
  # Look at performance against goals
  ###############################################################################

  # Define goals: start by initializing goal object
  goals <- add_goal(target_year = 2030,
                    target = 0.03,
                    target_type = "less",
                    abs_rel = "absolute",
                    pred_type = c("cell", "admin"))

  # Add goals to existing goal object by specifying goal_obj
  goals <- add_goal(goal_obj = goals,
                    target_year = 2020,
                    target = 0.04,
                    target_type = "less",
                    abs_rel = "absolute",
                    pred_type = c("cell", "admin"))

  # Run comparisons
  compare_to_target(ind_gp = indicator_group,
                    ind = indicator,
                    rd = run_date,
                    goal_obj = goals,
                    measure = "prevalence",
                    year_list = c(2000:2015),
                    uselogit = FALSE)

  ###############################################################################
  # Make summary metrics
  ###############################################################################

  # Get in and out of sample draws
  run_in_oos <- get_is_oos_draws(ind_gp = indicator_group,
                                 ind = indicator,
                                 rd = run_date,
                                 ind_fm = 'binomial',
                                 model_domain = 'africa',
                                 age = 0,
                                 nperiod = 16,
                                 yrs = 2000:2015,
                                 get.oos = as.logical(makeholdouts),
                                 write.to.file = TRUE)

  ## set out_dir
  out_dir <- paste0(sharedir, "/output/", run_date, "/summary_metrics/")
  dir.create(out_dir, recursive = T, showWarnings = F)

  ## for admin0
  draws.df <- fread(sprintf("/share/geospatial/mbg/%s/%s/output/%s/output_draws_data.csv",
                            indicator_group, indicator, run_date))

  country.pvtable <- get_pv_table(d = draws.df,
                                  indicator_group = indicator_group,
                                  rd = run_date,
                                  indicator=indicator,
                                  aggregate_on='country',
                                  draws = as.numeric(samples),
                                  out.dir = out_dir)

  write.csv(country.pvtable,
            file = sprintf("/share/geospatial/mbg/%s/%s/output/%s/summary_metrics/country_metrics.csv",
                           indicator_group, indicator, run_date))

  ad1.pvtable <- get_pv_table(d = draws.df,
                              indicator_group = indicator_group,
                              rd = run_date,
                              indicator=indicator,
                              aggregate_on='ad1',
                              draws = as.numeric(samples),
                              out.dir = out_dir)
  write.csv(ad1.pvtable,
            file = sprintf("/share/geospatial/mbg/%s/%s/output/%s/summary_metrics/ad1_metrics.csv",
                           indicator_group, indicator, run_date))
  ad2.pvtable <- get_pv_table(d = draws.df,
                              indicator_group = indicator_group,
                              rd = run_date,
                              indicator=indicator,
                              aggregate_on='ad2',
                              draws = as.numeric(samples),
                              out.dir = out_dir)
  write.csv(ad2.pvtable,
            file = sprintf("/share/geospatial/mbg/%s/%s/output/%s/summary_metrics/ad2_metrics.csv",
                           indicator_group, indicator, run_date))

  ###############################################################################
  ## Launch Validation Report (deprecated)
  ###############################################################################

  # dir.create(paste0(sharedir, '/output/', run_date, '/val_logs/'))

  # ## Launch validation reports
  # lapply(strata, submit_validation_report,
  #                    indicator       = indicator,
  #                    indicator_group = indicator_group,
  #                    run_date        = run_date,
  #                    pop_measure     = pop_measure,
  #                    repo            = repo,
  #                    log_dir         = paste0(sharedir, '/output/', run_date, '/val_logs/'),
  #                    target_type     = target_type,
  #                    target
  #                            = as.numeric(st_targ),
  #                    geo_nodes       = TRUE)

} # Close loop for individual countries

## END OF FILE
###############################################################################
