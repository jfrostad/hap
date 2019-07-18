##############################################################################
## MBG launch, aggregate results, and diagnostics launcher script for ORT
## Indicators: ors, rhf, ors_or_rhf, zinc
## Written by Kirsten Wiens
## Created 2018/02/23
##############################################################################
#source('/homes/jfrostad/_code/lbd/hap/cooking/model/01_launcher.R') 

## Setup -------------------------------------------------------------------------

# clear environment
rm(list = ls())

# set general arguments
user            <- Sys.info()['user']
repo            <- file.path('/homes', user, '_code/lbd/hap/')
indicator_group <- 'cooking'
parallel_script <- file.path(indicator_group, 'model/parallel_hap')

# Load MBG packages and functions
message('Loading in required R packages and MBG functions')
package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
source(paste0(repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = repo)

# set cluster arguments
use_geos_nodes  <- T
proj_arg        <- ifelse(use_geos_nodes, 'proj_geo_nodes', 'proj_geospatial_dia')
proj            <- ifelse(use_geos_nodes, paste0(' -P ', proj_arg, ' -l gn=TRUE '), paste0(' -P ', proj_arg, ' '))

# set covariate arguments
plot_covariates <- TRUE
covariate_plotting_only <- FALSE

# indicate whether to use old run date
use_old_run_date <- FALSE
old_run_date_input <- ''

# set run date
if (use_old_run_date == FALSE) {
  run_date <- make_time_stamp(TRUE)
} else {
  run_date <- old_run_date_input
}

# set config and covariate files
config_par   <- 'hap_best'
covar_par      <- 'region_specific'
#covar_par      <- 'ort_standard' #use to select single covariate set for all regions

# set whether running for individual countries
individual_countries <- FALSE


# indicate holdout (also need to do so in config)
holdout <- TRUE # only matters if running aggregation, set to TRUE for holdouts

# list all regions or countries
regions <- c('dia_afr_horn', 'dia_cssa', 'dia_wssa', 'dia_name', 'dia_sssa', 
             'dia_mcaca', 'dia_s_america', 'dia_central_asia', 'dia_chn_mng', 
             'dia_se_asia', 'dia_malay', 'dia_south_asia', 'dia_mid_east', 'dia_essa')
#regions <- c('dia_essa', 'dia_wssa', 'dia_cssa', 'dia_sssa')
regions <- c('dia_wssa')  
  
# list indicators
indics <- 'cooking_fuel_solid'

## Run launch scripts -------------------------------------------------------------------------

for (i in indics) {
  
  # make sure that only selecting a previous run_date intentionally
  if (use_old_run_date == TRUE) {
    prev <- readline('Are you sure you want to use a previous run date? Y or N: ')
    if (prev != 'Y') stop('Set use_old_run_date to FALSE.')
  }
  
  for (r in regions) {
    
    # set specific arguments
    Regions         <- r
    indicator       <- i
    jname           <- paste(indicator, Regions, sep = '_')
    mymem           <- '20G'
    
    # set region specific covariates, if desired
    if (covar_par == 'region_specific') {
      cov_par <- paste0('cooking_', Regions)
    } else {
      cov_par <- covar_par
    }
    
    # some quick checks for the arguments
    if(use_old_run_date == TRUE & old_run_date_input == '') stop('You indicated using an old run date; please provide an old run date')
    
    # set up qsub
    sys.sub <- paste0('qsub -e /share/temp/sgeoutput/', user,'/errors -o /share/temp/sgeoutput/', user, '/output ', 
                      '-l m_mem_free=', mymem, ' -P ', proj_arg, ifelse(use_geos_nodes, ' -q geospatial.q ', ' -q all.q '),
                      '-l fthread=1 -l h_rt=00:02:00:00 -v sing_image=default -N ', jname, ' -l archive=TRUE ')
    r_shell <- file.path(repo, 'mbg_central/share_scripts/shell_sing.sh')
    script <- file.path('/homes', user, '_code/lbd/hap', indicator_group, 'model/01_launch.R')
    args <- paste(user, repo, indicator_group, indicator, config_par, cov_par, Regions, parallel_script,
                  plot_covariates, covariate_plotting_only, proj_arg, use_geos_nodes, run_date, holdout)

    # run launch script
    paste(sys.sub, r_shell, script, args) %>% 
      system
    
  }
  
}