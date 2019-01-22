##############################################################################
## MBG launch, aggregate results, and diagnostics launcher script for ORT
## Indicators: ors, rhf, ors_or_rhf, zinc
## Written by Kirsten Wiens
## Created 2018/02/23
##############################################################################


## Setup -------------------------------------------------------------------------

# clear environment
rm(list = ls())

# set general arguments
user            <- Sys.info()['user']
repo            <- file.path('/homes', user, '_code/lbd/hap')
indicator_group <- 'cooking'
parallel_script <- '/model/parallel_hap'

# Load MBG packages and functions
message('Loading in required R packages and MBG functions')
package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
source(paste0(repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = repo)

# set cluster arguments
use_geos_nodes  <- TRUE
proj_arg        <- 'proj_geo_nodes_dia'
proj            <- ifelse(use_geos_nodes, paste0(' -P ', proj_arg, ' -l gn=TRUE '), paste0(' -P ', proj_arg, ' '))

# set covariate arguments
plot_covariates <- FALSE
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
config_par   <- 'ort_best'
covar_par      <- 'region_specific'
covar_par      <- 'ort_standard'

# set whether running for individual countries
individual_countries <- FALSE

# list all regions or countries
regions <- c('dia_afr_horn', 'dia_cssa', 'dia_wssa', 'dia_name', 'dia_sssa', 
             'dia_mcaca', 'dia_s_america', 'dia_central_asia', 'dia_chn_mng', 
             'dia_se_asia', 'dia_malay', 'dia_south_asia', 'dia_mid_east', 'dia_essa')
regions <- c('dia_s_america', 'dia_cssa', 'dia_essa', 'dia_wssa', 'dia_sssa', 'dia_se_asia', 'dia_south_asia')
regions <- c('dia_mcaca', 'dia_afr_horn', 'dia_cssa')
  
# list indicators
indics <- 'cooking_clean'

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
    mycores         <- 1
    
    # set region specific covariates, if desired
    if (covar_par == 'region_specific') cov_par <- paste0('hap_', Regions)
    else cov_par <- covar_par
    
    # some quick checks for the arguments
    if(use_old_run_date == TRUE & old_run_date_input == '') stop('You indicated using an old run date; please provide an old run date')
    if(individual_countries == TRUE & (!is.character(r) | nchar(r) != 3)) stop('You indicated running for individual countries; please specify region by iso3 code')
    if(individual_countries == FALSE & nchar(r) == 3) stop('It looks like you indicated an iso3 code for region; please set individual_countries to TRUE')
    
    # set up qsub
    sys.sub <- paste0('qsub -e /share/temp/sgeoutput/', user,'/errors -o /share/temp/sgeoutput/', user, '/output ', 
                      '-pe multi_slot ', mycores, proj, 
                      '-v sing_image=default -v SET_OMP_THREADS=1 -v SET_MKL_THREADS=1 -N ', jname, ' ')
    r_shell <- file.path(repo, 'mbg_central/share_scripts/shell_sing.sh')
    script <- file.path('/homes', user, '_code/lbd/hap', indicator_group, 'model/01_launch.R')
    args <- paste(user, repo, indicator_group, indicator, config_par, cov_par, Regions, parallel_script,
                  plot_covariates, covariate_plotting_only, proj_arg, use_geos_nodes, run_date, individual_countries)
    
    # run launch script
    paste(sys.sub, r_shell, script, args) %>% system
    
  }
  
}