###############################################################################
###############################################################################
## Basic Launch script for July 2017 MBG intensive. RB
## Recommend that launch is typically
##
###############################################################################
###############################################################################


###############################################################################
## SETUP
###############################################################################

## clear environment
rm(list=ls())

##X11 workaround
fplot <- function(obj) {
  setwd('/homes/jfrostad/temp/plot_junk/')

  png('fplot.png', 1200, 1200)
  print(plot(obj))
  dev.off()
}

## Set repo location and indicator group
user            <- 'adesh'
repo            <- sprintf('/share/code/geospatial/%s/mbg/',user)
remote          <- 'origin'
branch          <- 'develop'
indicator_group <- 'diarrhea'
makeholdouts    <- FALSE
pullgit         <- FALSE
indicator       <- 'had_diarrhea'
run_date <- 'training_jf'
model_geo_nodes <- FALSE
config_par <- 'exp_1'
Regions <- 'NGA'
message(indicator)
## sort some directory stuff and pull newest code into share
if(pullgit) system(sprintf('cd %s\ngit pull %s %s', repo, remote, branch))

## drive locations
root           <- ifelse(Sys.info()[1]=='Windows', 'J:/', '/home/j/')
sharedir       <- sprintf('/share/geospatial/mbg/%s/%s',indicator_group,indicator)
commondir      <- sprintf('/share/geospatial/mbg/common_inputs')
package_list <- c(t(read.csv(sprintf('%s/package_list.csv',commondir),header=FALSE)))

# TBD: Remve all 'setwd()'
setwd(repo)
core_repo <- repo

# Load MBG packages and functions
message('Loading in required R packages and MBG functions')
source(paste0(repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = repo)

for (p in package_list) {
  try(library(p, character.only = T))
}

library(seegSDM, lib.loc = '/share/code/geospatial/adesh/r_packages_hf_sing/')
library(seegMBG, lib.loc = '/share/code/geospatial/adesh/r_packages_hf_sing/')
library(mgcv)

## Throw a check for things that are going to be needed later
message('Looking for things in the config that will be needed for this script to run properly')

## Read config file and save all parameters in memory
config <- load_config(repo            = repo,
                      indicator_group = indicator_group,
                      indicator       = indicator,
                      config_name     = config_par,
                      covs_name = 'covs_experiment')

## Create directory structure for this model run
  create_dirs(indicator_group = indicator_group,
              indicator = indicator)


# Set BRT parameters from optimizer sheet
gbm_params <- read.csv(paste0(repo, '/', indicator_group, '/gbm_params.csv'), stringsAsFactors = F)
gbm_tc <- as.numeric(gbm_params$gbm_tc[1])
gbm_lr <- as.numeric(gbm_params$gbm_lr[1])
gbm_bf <- as.numeric(gbm_params$gbm_bf[1])
gbm_nminobs <- as.numeric(gbm_params$gbm_nminobs[1])
gbm_ntrees <- as.numeric(gbm_params$gbm_ntrees[1])

## Ensure you have defined all necessary settings in your config
check_config()

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
                          cores         = as.numeric(slots),
                          geo_nodes     = TRUE,
                          code = 'parallel_model_stg2',
                          singularity      = 'default',
                          singularity_opts = list(SET_OMP_THREADS=1, SET_MKL_THREADS=10),
                          addl_job_name = config_par)

  system(qsub)

}
