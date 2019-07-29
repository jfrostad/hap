# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: Written by Ani Deshpande
# Date: 07/25/2019
# Purpose: Launch script to run BRT optimization
# source("/homes/jfrostad/_code/lbd/hap/gbm_optim/04_optim_launcher.R", echo=T)
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

# define user
user <- Sys.info()['user']

# set repo
repo  <- file.path('/homes', user, '_code/lbd/hap')

# set node preference
use_geos_nodes <- TRUE
if (use_geos_nodes) {
  proj <- 'proj_geo_nodes'
  r_shell <- 'shell_geos.sh'
} else {
  proj <- 'proj_geospatial'
  r_shell <- 'shell_prod.sh'
}

# Use hf_inla
hf_inla_shell <- '/share/singularity-images/health_fin/forecasting/shells/health_fin_forecasting_shell_mkl_singularity.sh'
r_shell <- hf_inla_shell
mkl <- 1

# indicators to be launched
indis <- c('cooking_fuel_solid')

# regions to be launched
if (indis[[1]] == 'had_diarrhea') {
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

regions <- c('dia_sssa')


# set cov version for diarrhea (doesn't matter for ORT)
# options: 'standard' or 'standard_inc_mbg'
# cov_version <- 'standard_inc_mbg'
file_addin <- FALSE

# bounds versions to test
bounds_versions <- c(5,6,7)

# train and bag fractions to test
cv_fold <- 3
bag_fraction <- 0.5


## BRT optimizer script -------------------------------------------------------------------------

for (bv in bounds_versions) {

  for (ind in indis) {
    
    for (r in regions) {
      
      # get arguments
      jname <- paste0(ind, '_', r, '_optim')
      mymem <- '200G'
      sys.sub <- paste0('qsub -e /share/temp/sgeoutput/', user,'/errors -o /share/temp/sgeoutput/', user, '/output ', 
                        '-l m_mem_free=', mymem, ' -P ', proj, ifelse(use_geos_nodes, ' -q geospatial.q ', ' '),
                        '-l fthread=1 -l h_rt=00:09:00:00 -v sing_image=default -N ', jname, ' ')
      # launch script name to qsub
      script <- file.path(repo, 'gbm_optim/runGBM.R')
      jobnum <- paste0(ind, '_', r)
      opt_type <- 'gp'
      lrnr_type <- 'brt'
      bounds_version <- bv
      experiment_version <- paste0('test', bounds_version, '_', Sys.Date(), ifelse(file_addin == FALSE, '', paste0('_', file_addin)))
      args <- paste0(jobnum, ' ', opt_type, ' ', lrnr_type, ' ', bounds_version, ' ', 
                     experiment_version, ' ', ind, ' ', cv_fold, ' ', bag_fraction, ' ', file_addin)
      system(paste(sys.sub, r_shell, mkl, script, args))
  
    }
    
    Sys.sleep(1)
  }

}