  ##############################################################################
  ## MBG launch, aggregate results, and diagnostics launcher script for ORT
  ## Indicators: ors, rhf, ors_or_rhf, zinc
  ## Written by Kirsten Wiens
  ## Created 2018/02/23
  ##############################################################################
  #source('/homes/jfrostad/_code/lbd/hap/cooking/post/1_launcher.R') 
  
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
  
  # list all regions or countries
  # standard regions
  regions <- c('dia_afr_horn', 'dia_cssa', 'dia_wssa', 'dia_name-ESH', 'dia_name', 'dia_sssa', 
               'dia_mcaca', 'dia_s_america-GUF', 'dia_s_america', 'dia_central_asia', 'dia_chn_mng', 
               'dia_se_asia', 'dia_malay', 'dia_south_asia', 'dia_mid_east', 'dia_essa')

  regions <- c('dia_essa-ERI-DJI-YEM', "ERI+DJI+YEM",
               #'dia_afr_horn-ERI-DJI-YEM',
               'sssa-ZAF', 'ZAF',
               'cssa-AGO-GNQ', 'AGO',
               'wssa-CPV-NGA', 'NGA',
               'noaf-ESH',
               #'dia_mcaca', 
               'caca-CUB',
               #'dia_s_america-BRA-GUF', 'BRA',
               'ansa-VEN', 'trsa-GUF',
               #'dia_central_asia', 
               'stan-TKM',
               'CHN', 'MNG',
               'ocea-MYS',
               'seas-VNM-THA-MYS', 'VNM', 'THA',
               'mide+TKM', 'soas')
  
  #regions <- c('AGO', 'THA', 'VNM', 'ZAF')
  
  ## Set repo location, indicator group, and some arguments
  user <- 'jfrostad'
  repo <- "/homes/jfrostad/_code/lbd/hap"
  indicator_group <- 'cooking'
  indicator <- 'cooking_fuel_solid'
  config_par   <- 'hap_standard'
  holdout <- 0
  age <- 0
  run_date <- '2020_04_02_23_45_45'
  measure <- 'prev'


  
for (region in regions) {
 
      # set memory based on region
      if (region %in% c('dia_chn_mng', 'dia_s_america-GUY', 'dia_s_america-BRA')) { mymem <- '900G'
      } else if (region %in% c('dia_wssa', 'dia_s_america-BRA')) { mymem <- '500G'
      } else mymem <- '350G'
      
      #name job
      jname           <- paste('eDL', region, indicator, sep = '_')
      
      #setup covars file
      cov_par <- paste(indicator_group, region, sep='_')
      
      # set up qsub
      sys.sub <- paste0('qsub -e /share/temp/sgeoutput/', user,'/errors -o /share/temp/sgeoutput/', user, '/output ', 
                        '-l m_mem_free=', mymem, ' -P ', proj_arg, ifelse(use_geos_nodes, ' -q geospatial.q ', ' -q all.q '),
                        '-l fthread=1 -l h_rt=16:00:00:00 -v sing_image=default -N ', jname, ' -l archive=TRUE ')
      r_shell <- file.path(repo, 'mbg_central/share_scripts/shell_sing.sh')
      script <- file.path(repo, indicator_group, 'post/2_entry.R')
      args <- paste(user, repo, indicator_group, indicator, config_par, cov_par, region, run_date, measure, holdout)
      
      # run launch script
      paste(sys.sub, r_shell, script, args) %>% 
        system

}