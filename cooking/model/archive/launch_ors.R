################################################################################
################################################################################
## Launch script for Oral Rehydration Therapy (test) for Africa
##
##
## Last Updated: July 9, 2018
################################################################################
################################################################################


################################################################################
## SETUP
################################################################################

## clear environment
rm(list=ls())

## Set repo location and indicator group
user            <- Sys.info()['user']
core_repo       <- '/ihme/code/geospatial/lbd_core/'
indic_repo      <- paste0('/share/code/geospatial/', user, '/ort/')
remote          <- 'origin'
branch          <- 'develop'
indicator_group <- 'ort'
indicator       <- 'ors'
use_geos_notes  <- FALSE
slots <- 20

## drive locations
root           <- '/home/j/'
sharedir       <- sprintf('/share/geospatial/mbg/%s/%s',indicator_group,indicator)
commondir      <- sprintf('/share/geospatial/mbg/common_inputs')
package_list <- c(t(read.csv(sprintf('%s/package_list.csv',commondir),header=FALSE)))

## setup a working directory
setwd(core_repo)

## Load MBG packages and functions
message('Loading in required R packages and MBG functions')
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

## Read config file and save all parameters in memory
config <- load_config(repo            = indic_repo,
                      indicator_group = "",
                      indicator       = "",
                      config_name     = paste0('3_modeling/', indicator, '/config_', indicator),
                      covs_name       = paste0('3_modeling/', indicator, '/covs_', indicator))

## Create run date in correct format
run_date <- make_time_stamp(TRUE)

## Create output folder with the run_date
outputdir      <- paste('/share/geospatial/mbg', indicator_group, indicator, 'output', run_date, '', sep='/')
dir.create(outputdir)

## Ensure you have defined all necessary settings in your config
check_config()

# get summstats from config
if(length(summstats) == 1 & grepl(",", summstats)) summstats <- eval(parse(text=summstats))

## Create a few objects from the config file loaded above
if (class(Regions) == "character" & length(Regions) == 1) Regions <- eval(parse(text=Regions))
if (class(year_list) == "character") year_list <- eval(parse(text=year_list))

## Load gaul list
gaul_list <- 195 # just peru for now
                 # if you need to look it up:
                 # master_shape <- readRDS('/share/geospatial/rds_shapefiles/gbd_2016/master_shape_all.rds')


## If running individual countries, get set up
if (individual_countries == TRUE) {
  # Convert all Regions to individual countries
  Regions <- gaul_list
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
                        withtag     = as.logical(withtag),
                        datatag     = datatag,
                        use_share   = as.logical(use_share))

  # add in location information
  df <- merge_with_ihme_loc(df)

  # make a list of dfs for each region, with 5 qt folds identified in each
  stratum_ho <- make_folds(data       = df,
                           n_folds    = n_ho_folds,
                           spat_strat = 'qt',
                           temp_strat = 'prop',
                           strat_cols = 'region',
                           ts         = ho_ts,
                           mb         = ho_mb)
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
  qsub <- make_qsub_share(age           = loopvars[i,2],
                          reg           = as.character(loopvars[i,1]),
                          holdout       = loopvars[i,3],
                          test          = F,
                          indic         = indicator,
                          saveimage     = TRUE,
                          memory        = 10,
                          cores         = ifelse(use_geos_nodes, 5, as.numeric(slots)),
                          geo_nodes     = use_geos_nodes)

  system(qsub)

}

## Check to make sure models are done before continuing
waitformodelstofinish(lv = cbind(as.character(loopvars[, 1]), loopvars[, 3]), sleeptime = 60)

###############################################################################
## Post-Estimation
###############################################################################

# ~~~~~~~~~~~~~~~~~~~ Q ~~~~~~~~~~~~~~~~~~~ 
# What is the purpose of post-estimation?
# What if I'm not raking to GBD?
# ~~~~~~~~~~~~~~~~~~~ Q ~~~~~~~~~~~~~~~~~~~ 

## Save strata for Shiny to use in producing aggregated fit statistics
strata <- unique(as.character(loopvars[, 1]))
dir.create(paste0(outputdir, '/fit_stats'))
save(strata, file = paste0(outputdir, '/fit_stats/strata.RData'))

# Not using GBD raking for ORT because I have issues with the GBD data
# gbd <- load_gbd_data(gbd_type     = "output",
#                     gbd_name     = gbd_id,
#                     gaul_list    = get_gaul_codes('africa'),
#                     measure_id   = 5,
#                     age_group_id = 1,
#                     metric_id    = 3,
#                     year_ids     = year_list)

## Prepare for parallel post-estimation - save file with objs to re-load in child processes
prep_postest(indicator       = indicator,
             indicator_group = indicator_group,
             run_date        = run_date,
             save_objs       = c("core_repo", "indic_repo", "gbd", "year_list", "summstats", "rake_transform", "pop_measure"))

## Parallelized post-estimation over region
postest_script <- "postest_script"

for (s in strata) {
  qsub <- make_qsub_postest(code         = postest_script,
                            stratum      = s,
                            log_location = 'sharedir',
                            memory       = 10,
                            cores        = ifelse(use_geos_nodes, 6, 50),
                            geo_nodes    = use_geos_nodes)
  system(qsub)
}

## Check to make sure post-est done before continuing
waitformodelstofinish(lv = cbind(strata, 0), sleeptime=60)

## Combine post est stuff across regions and save needed outputs
# post_load_combine_save(summstats = summstats, raked = 'unraked', rf_table = FALSE)
post_load_combine_save(summstats = summstats)

## Clean up / delete unnecessary files
clean_after_postest(indicator             = indicator,
                    indicator_group       = indicator_group,
                    run_date              = run_date,
                    strata                = strata,
                    delete_region_rasters = F)

###############################################################################
## Launch model diagnostics script for shiny tool
###############################################################################
## View results at https://shiny.ihme.washington.edu/connect/#/apps/119/

make_model_diagnostics(indic     = indicator,
                       ig        = indicator_group,
                       rd        = run_date,
                       cores     = ifelse(use_geos_nodes, 2, 15),
                       geo_nodes = use_geos_nodes)

###############################################################################
## Aggregate to admin2, admin1, and national levels
###############################################################################

submit_aggregation_script(indicator       = indicator,
                          indicator_group = indicator_group,
                          run_date        = run_date,
                          raked           = c(TRUE, FALSE),
                          pop_measure     = pop_measure,
                          overwrite       = T,
                          ages            = 0, # Note: can take vector of ages
                          holdouts        = 0,
                          regions         = strata,
                          corerepo        = core_repo,
                          log_dir         = outputdir,
                          slots           = ifelse(use_geos_nodes, 5, 45),
                          geo_nodes       = use_geos_nodes)

waitforaggregation(rd       = run_date,
                   indic    = indicator,
                   ig       = indicator_group,
                   ages     = 0,
                   regions  = strata,
                   holdouts = 0,
                   raked    = c(T, F))

combine_aggregation(rd       = run_date,
                    indic    = indicator,
                    ig       = indicator_group,
                    ages     = 0,
                    regions  = strata,
                    holdouts = 0,
                    raked    = c(T, F))

summarize_admins(summstats = summstats,
                 ad_levels = c(0, 1, 2),
                 raked     = c(T, F))

if (use_stacking_covs) {
  aggregate_stackers_admin0(indicator       = indicator,
                            indicator_group = indicator_group,
                            run_date        = run_date,
                            age             = 0,
                            holdout         = 0,
                            regions         = Regions,
                            year_list       = year_list,
                            pop_measure     = pop_measure,
                            results_file    = paste0(outputdir, "/pred_derivatives/admin_summaries/", indicator, "_admin_0_stackers.csv"))
}

###############################################################################
## Make summary metrics
###############################################################################

## Combine csv files
csvs <- list.files(outputdir, pattern = "input_data_(.*).csv", full.names = T)
csv_master <- rbindlist(lapply(csvs, fread))
csv_master[, V1 := NULL]
write.csv(csv_master, file=paste0(outputdir, '/input_data.csv'))

## Get in and out of sample draws
run_in_oos <- get_is_oos_draws(ind_gp        = indicator_group,
                               ind           = indicator,
                               rd            = run_date,
                               ind_fm        = 'binomial',
                               model_domain  = 'africa',
                               age           = 0,
                               nperiod       = length(year_list),
                               yrs           = year_list,
                               get.oos       = as.logical(makeholdouts),
                               year_col      = "year",
                               write.to.file = TRUE)

## Set out_dir
dir.create(paste0(outputdir, "/summary_metrics/"), recursive = T, showWarnings = F)

## Calculate and save PV summary statistics
draws.df <- fread(paste0(outputdir, "/output_draws_data.csv"))
pvtab <- rbindlist(lapply(c("country", "ad1", "ad2"), function(lvl) {
  pv <- lapply(list(c("oos"), c("oos", "year"), c("oos", "region")), function(results_by) {
    get_pv_table(d               = draws.df,
                 indicator_group = indicator_group,
                 rd              = run_date,
                 indicator       = indicator,
                 aggregate_on    = lvl,
                 result_agg_over = results_by,
                 draws           = as.numeric(samples),
                 out.dir         = paste0(outputdir, "/summary_metrics/"))
  })
  cbind(Level = lvl, rbindlist(pv, fill = T))
}))
write.csv(pvtab, file = paste0(outputdir, "/summary_metrics/pv_metrics.csv"), row.names=F)

# Plot MBG vs. GBD estimates (interactive plot)
# This may not be helpful for ORT
# country_list <- fread(paste0(root, 'temp/', user, "/gaul_list.csv"))
# raking <- fread(paste0(outputdir, "ebf_rf.csv"))
# raking <- merge(raking, country_list[, c("iso3","name"), with = F], by = "name")
# raking[, "name" := lapply(.SD, function(x){as.factor(x)}), .SDcols="name"]

# library(plotly)
# mypath <- file.path(paste0(outputdir, "/summary_metrics/mbg_gbd.html"))
# p <- ggplot(data = raking, aes(x = rake_to_mean, y = geo_mean, color=iso3, text = paste("year:", year))) +
#   geom_point() + labs(x = "GBD mean", y = "MBG mean") + geom_abline(intercept = 0, slope = 1)
# p <- ggplotly(p)
# htmlwidgets::saveWidget(p, file = mypath)

###############################################################################
## Make maps and plots
###############################################################################

## Covariate importance plots
if (use_stacking_covs) {
  source(paste0(indic_repo, "post_estimation/get_cov_weights.R"))
  get_cov_weights(indicator, indicator_group, run_date, Regions, outputdir)
}

## National data-and-estimates plots
# source(paste0(indic_repo, "/post_estimation/admin0_data_and_estimates_plots.r"))
# admin0_data_and_estimates_plots(data_date, run_date)

## MBG Levels and differences maps
source(paste0(indic_repo, "post_estimation/map_model_results.r"))
map_model_results(indicator, indicator_group, run_date, TRUE)
map_model_results(indicator, indicator_group, run_date, FALSE)

## GBD National level maps
source(paste0(indic_repo, "post_estimation/map_gbd_model_results.r"))
map_gbd_model_results(indicator, indicator_group, run_date)

## Validation results
source(paste0(indic_repo, "post_estimation/plot_pv_results.r"))
plot_pv_results(indicator, indicator_group, run_date, save_file = paste0(outputdir, "/summary_metrics/pv_metrics.pdf"))