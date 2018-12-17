###############################################################################
## MBG launch script
##
## Modified for ORT by Kirsten Wiens starting on 2018/08/10
##
###############################################################################


## Setup -------------------------------------------------------------------------

## Clear environment
rm(list=ls())

## Set repo location, indicator group, and some arguments
user            <- commandArgs()[4]
repo            <- commandArgs()[5]
indicator_group <- commandArgs()[6]
indicator       <- commandArgs()[7]
config_par      <- commandArgs()[8]
cov_par         <- commandArgs()[9]
Regions         <- commandArgs()[10]
parallel_script <- commandArgs()[11]
core_repo       <- repo
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
                      config_name     = paste0('housing/model/config_', config_par),
                      covs_name       = paste0('housing/model/covs_', cov_par))

## Set xgboost hyperparameter filepath
## TODO investigate
hyperparameter_filepath <- paste0('/ihme/homes/kewiens/gbm_optim/xgb/best_pars_ors_', Regions, '.csv')

## Set some covariate options
plot_covariates <- commandArgs()[12]
covariate_plotting_only <- commandArgs()[13]

## Set to prod or geos nodes
proj_arg <- commandArgs()[14]
message(proj_arg)
use_geos_nodes <- commandArgs()[15]

## Set run date and whether running individual countries
run_date <- commandArgs()[16]
individual_countries <- commandArgs()[17]

## Create output folder with the run_date
outputdir      <- paste('/share/geospatial/mbg', indicator_group, indicator, 'output', run_date, '', sep='/')
dir.create(outputdir)

## Create directory structure for this model run
create_dirs(indicator_group = indicator_group, indicator = indicator)

## Create proper year list object
if (class(year_list) == 'character') year_list <- eval(parse(text=year_list))

## Ensure you have defined all necessary settings in your config
check_config()

## If running individual countries make sure all country FEs and REs off
individual_countries <- commandArgs()[17]
if (individual_countries) {
  use_child_country_fes <- FALSE
  use_inla_country_fes  <- FALSE
  use_inla_country_res  <- FALSE
}

## Set BRT parameters from optimizer sheet
## TODO investigate
# gbm_params <- read.csv(paste0(core_repo, '/housing/3_modeling/gbm_params_ors.csv'), stringsAsFactors = F)
# gbm_tc <- as.numeric(select(filter(gbm_params, indi == indicator, region == Regions), gbm_tc))
# gbm_lr <- as.numeric(select(filter(gbm_params, indi == indicator, region == Regions), gbm_lr))
# gbm_bf <- as.numeric(select(filter(gbm_params, indi == indicator, region == Regions), gbm_bf))
# gbm_nminobs <- as.numeric(select(filter(gbm_params, indi == indicator, region == Regions), gbm_nminobs))
# gbm_ntrees <- as.numeric(select(filter(gbm_params, indi == indicator, region == Regions), gbm_ntrees))

## Record model parameters in google sheet ORZ Model Tracker
## TODO investigate
# library('googlesheets')
# model_params <- c(indicator_group, indicator, run_date, outputdir, 
#                   covariate_plotting_only, individual_countries, Regions, 
#                   config_par, cov_par, fixed_effects, gbd_fixed_effects,
#                   stacked_fixed_effects, mesh_t_knots, rho_prior, makeholdouts, parallel_script)
# load(paste0(core_repo, '/housing/model/gs_authorization_token.RData'))
# gs_auth(token = ttt)
# orz_tracker <- gs_title('ORZ model tracker')
# gs_add_row(orz_tracker, ws = 'Sheet1', input = model_params)
# rm(orz_tracker)


## Make holdouts -------------------------------------------------------------------------

if(makeholdouts){
  message('Making holdouts')
  
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
                           n_folds    = as.numeric(n_ho_folds),
                           spat_strat = 'qt',
                           temp_strat = 'prop',
                           strat_cols = 'region',
                           ts         = as.numeric(ho_ts),
                           mb         = as.numeric(ho_mb))
}


## Launch parallel script -------------------------------------------------------------------------

## Make loopvars aka strata grid (format = regions, ages, holdouts)
if(makeholdouts) loopvars <- expand.grid(Regions, 0, 0:n_ho_folds) else loopvars <- expand.grid(Regions, 0, 0)

## loop over them, save images and submit qsubs
for(i in 1:nrow(loopvars)){
  
  message(paste(loopvars[i,2],as.character(loopvars[i,1]),loopvars[i,3]))
  
  # get region memory
  r <- as.character(loopvars[i,1])
  if (r == 'dia_central_asia' | r == 'dia_se_asia' | r == 'dia_malay' | r == 'dia_sssa' | r == 'dia_mcaca') region_memory = 25
  if (r == 'dia_mid_east' | r == 'dia_cssa' | r == 'dia_essa' | r == 'dia_south_asia' | r == 'dia_afr_horn' | r == 'dia_name' | r == 'dia_wssa') region_memory = 35
  if (r == 'dia_s_america' | r == 'dia_chn_mng') region_memory = 45
  
  # make a qsub string
  qsub <- make_qsub_share(age           = loopvars[i,2],
                          reg           = as.character(loopvars[i,1]),
                          holdout       = loopvars[i,3],
                          test          = F,
                          indic         = indicator,
                          saveimage     = TRUE,
                          memory        = region_memory,
                          cores         = ifelse(use_geos_nodes, 10, as.numeric(slots)),
                          proj          = proj_arg,
                          geo_nodes     = as.logical(use_geos_nodes),
                          corerepo      = core_repo,
                          code          = parallel_script,
                          addl_job_name = paste0(indicator, '_', as.character(loopvars[i,1]), '_parallel'),
                          singularity   = 'default')
  # submit job
  system(qsub)
  
}
