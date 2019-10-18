## Script to run raking and aggregating for diarrhea after cell preds have been generated
## Author: Kirsten Wiens
## Date: 2010-01-04
#source('/homes/jfrostad/_code/lbd/hap/cooking/model/launch_agg.R') 

# Setup ------------------------------------------------------------------------------------------------------------------------------------------

#detect if running in rstudio IDE
debug <- T
interactive <- ifelse(debug, T, !(is.na(Sys.getenv("RSTUDIO", unset = NA))))

if (interactive) {
  
  # set arguments
  indicator_group          <- 'cooking'
  indicator                <- 'cooking_fuel_solid'
  regions                  <- c('dia_afr_horn', 'dia_cssa', 'dia_wssa', 'dia_name', 'dia_sssa', 
                                'dia_mcaca', 'dia_s_america', 'dia_central_asia', 'dia_chn_mng', 
                                'dia_se_asia', 'dia_malay', 'dia_south_asia', 'dia_mid_east', 'dia_essa')
  #regions <- 'dia_chn_mng'
  run_date                 <- '2019_09_16_23_24_14'
  makeholdout              <- F
  
} else {
  
  # Load arguments from qsub
  indicator_group          <- commandArgs()[6]
  indicator                <- commandArgs()[7]
  reg                      <- commandArgs()[10]
  run_date                 <- commandArgs()[16]
  makeholdout              <- commandArgs()[17]
}

# set holdouts
if (as.logical(makeholdout)) {
  holdouts <- c(0,1,2,3,4,5) 
} else {
  holdouts <- 0
}



# loop over holdouts
for (ho in holdouts) {
  for (reg in regions) {
    
    # Set filepaths
    age <- 0
    test <- 0
    pathaddin <- paste0('_bin',age,'_',reg,'_',ho)
    outputdir <- file.path('/share/geospatial/mbg', indicator_group, indicator, 'output', run_date, '/')
    
    # Load files
    load(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/model_image_history/pre_run_tempimage_', run_date, pathaddin,'.RData'))

    # make sure we have repo argument
    repo <- core_repo
    
    # # Overwrite project for this re-launch 
    # proj_arg                 <- commandArgs()[14]
    # use_geos_nodes           <- commandArgs()[15]
    # indicator_group          <- commandArgs()[6]
    # indicator                <- commandArgs()[7]
    # reg                      <- commandArgs()[10]
    # run_date                 <- commandArgs()[16]
    # makeholdout              <- commandArgs()[17]
    # 
    
    ## Aggregate ----------------------------------------------------------------------------------------------------------
    
    # set specific arguments
    measure         <- 'prevalence'
    jname           <- paste('agg', reg, indicator, sep = '_')
    
    # set memory by region
    individual_countries <- ifelse(nchar(reg) == 3, TRUE, FALSE)
    mymem <- 200
    if (as.logical(individual_countries) & reg != 'IND') mymem <- 100
    if(r %in% c('dia_malay', 'dia_name')) mymem <- 250
    if(r %in% c('dia_wssa', 'dia_south_asia')) mymem <- 320
    if(r %in% c('dia_chn_mng', 'dia_s_america')) mymem <- 400
    
    # set up qsub
    sys.sub <- paste0('qsub -e ', outputdir, '/errors -o ', outputdir, '/output ', 
                      '-l m_mem_free=', mymem, 'G -P ', proj_arg, ifelse(use_geos_nodes, ' -q geospatial.q ', ' -q all.q '),
                      '-l fthread=2 -l h_rt=00:12:00:00 -v sing_image=default -N ', jname, ' -l archive=TRUE ')
    r_shell <- paste0(repo, 'mbg_central/share_scripts/shell_sing.sh')
    script <- file.path(repo, 'mbg_central/share_scripts/frax_script_hap.R')
    args <- paste(user, repo, indicator_group, indicator, config_par, cov_par, reg, proj_arg, 
                  use_geos_nodes, run_date, measure, ho)
    
    # submit qsub
    system(paste(sys.sub, r_shell, script, args))
    
  }
}