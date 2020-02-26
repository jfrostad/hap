##############################################################################
## Variance inflation factor (VIF) launcher script
## Written by Kirsten Wiens
## Created 2018/11/06
##############################################################################
#source('/homes/jfrostad/_code/lbd/hap/covariate_selection/01_launch_vif_selection.R') 

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

# set node preference
use_geos_nodes <- TRUE
if (use_geos_nodes) {
  proj <- 'proj_geo_nodes'
  r_shell <- 'shell_geos.sh'
} else {
  proj <- 'proj_geospatial'
  r_shell <- 'shell_prod.sh'
}

# set config and covariate files
config_par   <- 'config_hap_standard'
config_file <- file.path(indicator_group, 'model/configs/')
cov_par      <- 'covs_hap_standard'
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
  mymem <- '300G'
  sys.sub <- paste0('qsub -e /share/geospatial/mbg/cooking/logs/errors -o /share/geospatial/mbg/cooking/logs/output ',
                    '-l m_mem_free=', mymem, ' -P ', proj, ifelse(use_geos_nodes, ' -q geospatial.q ', ' '),
                    '-l fthread=1 -l h_rt=00:12:00:00 -v sing_image=default -N ', jname, ' ')
  r_shell <- paste0('/share/code/geospatial/kewiens/ort/mbg_central/share_scripts/shell_sing.sh')
  script <- paste0(repo, '/covariate_selection/02_vif_selection_script.R')
  args <- paste(user, repo, indicator_group, indicator, config_par, config_file, cov_par, cov_file, 
                run_date, regions, threshold_min, threshold_max, threshold_step, individual_countries,
                crop_covs, cropped_covs_file)
  
  # run launch script
  paste(sys.sub, r_shell, script, args) %>% system
  
}
#***********************************************************************************************************************