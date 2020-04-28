##############################################################################
## Launch script to crop covariates and data together for BRT optimization
## Written by Kirsten Wiens
##############################################################################


## Setup -------------------------------------------------------------------------

# clear environment
rm(list = ls())

# set general arguments
user            <- Sys.info()['user']
repo            <- file.path('/homes', user, '_code/lbd/hap/')
core_repo       <- repo
indicator_group <- 'cooking'
indicator       <- 'cooking_fuel_solid'

# set cluster arguments
use_geos_nodes  <- TRUE
proj_arg        <- 'proj_geo_nodes'

# set config and covariate files
config_par   <- 'config_hap_best'
config_file  <- 'cooking/model/configs/'
covar_par      <- 'region_specific'
cov_file     <- config_file

# set cov version for diarrhea (doesn't matter for ORT)
# options: 'standard' or 'standard_inc_mbg'
# cov_version <- 'standard_inc_mbg'
file_addin <- FALSE

# set whether running for individual countries
individual_countries <- FALSE

# list all regions or countries
if (indicator == 'cooking') {
  regions <- c('dia_afr_horn', 'dia_cssa', 'dia_wssa', 'dia_name', 'dia_sssa', 
               'dia_mcaca', 'dia_s_america', 'dia_central_asia', 'dia_chn_mng', 
               'dia_se_asia', 'dia_malay', 'dia_south_asia', 'dia_mid_east', 'dia_essa')
} else {
  regions <- c('dia_afr_horn', 'dia_name', 'dia_sssa', 
               'dia_mcaca', 'dia_s_america', 'dia_central_asia', 'dia_chn_mng', 
               'dia_se_asia', 'dia_malay', 'dia_mid_east',
               'ZWE', 'KEN', 'NGA', 'COD', 'IND', 'PAK',
               'dia_essa-zwe-ken', 'dia_wssa-nga', 'dia_cssa-cod', 'dia_south_asia-ind-pak')
}

regions <- c('dia_wssa', 'dia_south_asia')


## Run covariate data scripts -------------------------------------------------------------------------

for (r in regions) {

  # set region specific covariates, if desired
  if (covar_par == 'region_specific') cov_par <- paste0('covs_cooking_', r)
  else cov_par <- covar_par

  # set specific arguments
  region          <- r
  jname           <- paste0(r, '_brt_data')
  mymem           <- '20G'
  
  # set up qsub
  sys.sub <- paste0('qsub -e /share/temp/sgeoutput/', user,'/errors -o /share/temp/sgeoutput/', user, '/output ', 
                    '-l m_mem_free=', mymem, ' -P ', proj_arg, ifelse(use_geos_nodes, ' -q geospatial.q ', ' '),
                    '-l fthread=1 -l h_rt=00:04:00:00 -v sing_image=default -N ', jname, ' ')
  r_shell <- file.path(repo, 'mbg_central/share_scripts/shell_sing.sh')
  script <- file.path(repo, 'gbm_optim/save_cov_data.R')
  args <- paste(region, user, repo, indicator_group, indicator, config_par, config_file, 
                cov_par, cov_file, individual_countries, file_addin)
  
  # run launch script
  system(paste(sys.sub, r_shell, script, args))
  
}
