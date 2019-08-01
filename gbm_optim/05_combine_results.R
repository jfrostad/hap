##############################################################################
## Combine results from BRT optimization
## Written by Kirsten Wiens
## Originally written to take in results of bag/train fraction testing
# source("/homes/jfrostad/_code/lbd/hap/gbm_optim/05_combine_results.R", echo=T)
##############################################################################


## Setup -------------------------------------------------------------------------

# clear workspace
rm(list=ls())
pacman::p_load(data.table, magrittr)

# indicator tested
indicators <- c('cooking_fuel_solid')

# space bounds version
lrnr_type <- 'brt'
experiment_version <- 'test6_2019-07-30'

# set directories
repo  <- file.path('/homes', Sys.info()['user'], '_code/lbd/hap')
code_dir <- file.path(repo, 'gbm_optim')
in_dir <- '/share/scratch/tmp/jfrostad/output/gbm_optim/'
dir_train <- file.path(in_dir, lrnr_type, experiment_version, '/stats_train_output/')
dir_test <- file.path(in_dir, lrnr_type, experiment_version, '/stats_test_output/')
dir_model <- file.path(in_dir, lrnr_type, experiment_version, '/stats_output/')
dir_params <- file.path(in_dir, lrnr_type, experiment_version, '/')

for (indicator in indicators) {
  message(indicator)
  
  # get completed regions
  regions <- list.files(dir_params, pattern = paste0('best_pars_', indicator)) %>% 
    gsub(paste0('best_pars_', indicator, '_'), '', .) %>% 
    gsub('_exp1.csv', '', .) %>% 
    #if (indicator == 'ors') gsub('or_rhf_', '', .) %>% 
    unique %T>%
    print
  
  ## Combine results from BRT optimization runs -------------------------------------------------------------------------
  
  # function to read in, clean, and save model outputs
  combine_outputs <- function(dir, save = T, regs = regions, ind = indicator) {
    
    # initiate a list
    all_results <- as.data.table(lapply(paste0(dir, list.files(dir, pattern = paste0(ind, '_', regs[[1]]))), fread))
    all_results[, region := NA]
    all_results <- all_results[-1, ]
    
    for (r in regs) {
      message(r)
    
      # read files into a list
      results <- as.data.table(lapply(paste0(dir, list.files(dir, pattern = paste0(ind, '_', r, '_exp'))), fread))
      results[, region := r]
      
      # rbind into one file and order by experiment
      all_results <- rbind(all_results, results, fill = T)
    }
      
    # save, if desired
    if (save) write.csv(all_results, paste0(dir, 'summary_', indicator, '.csv'))
  }
  
  # combine parameter files
  combine_outputs(dir_params)
  
  # combine in-sample stats
  combine_outputs(dir_train)
  
  # combine out-of-sample stats
  combine_outputs(dir_test)
  
  # combine overall model stats
  combine_outputs(dir_model)

}
#***********************************************************************************************************************