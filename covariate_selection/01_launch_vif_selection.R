##############################################################################
## Variance inflation factor (VIF) launcher script
## Written by Kirsten Wiens
## Created 2018/11/06
##############################################################################


## Setup -------------------------------------------------------------------------

# clear environment
rm(list = ls())

# set general arguments
user            <- Sys.info()['user']
repo            <- "/homes/jfrostad/_code/lbd/hap/"
indicator_group <- 'cooking'

# Load MBG packages and functions
message('Loading in required R packages and MBG functions')
package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
source(paste0(repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = repo)

# set cluster arguments
use_geos_nodes  <- T
proj_arg        <- 'proj_geo_nodes'
proj            <- ifelse(use_geos_nodes, paste0(' -P ', proj_arg, ' -l gn=TRUE '), paste0(' -P ', proj_arg, ' '))

# set config and covariate files
config_par   <- 'config_ort_best'
config_file <- file.path(indicator_group, 'model/configs/')
cov_par      <- 'covs_ort_standard'
cov_file <- config_file

# indicate whether to use old run date
use_old_run_date <- FALSE
old_run_date_input <- ''

# set run date
if (use_old_run_date == FALSE) {
  run_date <- make_time_stamp(TRUE)
} else {
  run_date <- old_run_date_input
}

# set whether running individual countries
individual_countries <- FALSE

# indicate threshold parameters
threshold_min <- 2
threshold_max <- 5
threshold_step <- 1

# indicate whether or not to crop covariates (must give .RData file with cropped covariate file if crop_covs = FALSE)
crop_covs <- TRUE
cropped_covs_file <- ''

# list indicators
indics <- c('cooking_fuel_solid')

# list either all regions or one region/country (argument cannot be a vector/list of regions)
regions <- 'all'


## Run vif selection script -------------------------------------------------------------------------

for (i in indics) {
    
  # set specific arguments
  indicator       <- i
  jname           <- paste0(indicator, '_vif_selection')
  mycores         <- ifelse(use_geos_nodes, 15, 40)
  
  # set up qsub
  sys.sub <- paste0('qsub -e /share/geospatial/mbg/cooking/logs/errors -o /share/geospatial/mbg/cooking/logs/output ', 
                    '-pe multi_slot ', mycores, proj, 
                    '-v sing_image=default -v SET_OMP_THREADS=1 -v SET_MKL_THREADS=1 -N ', jname, ' ')
  r_shell <- paste0('/share/code/geospatial/kewiens/ort/mbg_central/share_scripts/shell_sing.sh')
  script <- paste0(repo, '/covariate_selection/02_vif_selection_script.R')
  args <- paste(user, repo, indicator_group, indicator, config_par, config_file, cov_par, cov_file, 
                run_date, regions, threshold_min, threshold_max, threshold_step, individual_countries,
                crop_covs, cropped_covs_file)
  
  # run launch script
  system(paste(sys.sub, r_shell, script, args))
  
}
