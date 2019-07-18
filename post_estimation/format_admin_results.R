# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 06/24/2019
# Purpose: Build keyed data.tables at the admin_0/1/2 result for a variety of input models
#source('/homes/jfrostad/_code/lbd/hap/post_estimation/format_admin_results.R') 
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory

# clear memory
rm(list=ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j/"
  h_root <- file.path("/ihme/homes", Sys.info()["user"])
  
  package_lib    <- file.path(h_root, '_code/_lib/pkg')
  ## Load libraries and  MBG project functions.
  .libPaths(package_lib)
  
  # necessary to set this option in order to read in a non-english character shapefile on a linux system (cluster)
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
} else {
  j_root <- "J:"
  h_root <- "H:"
}

#load external packages
#TODO request adds to lbd singularity
pacman::p_load(ccaPP, fst, mgsub)

#options
options(scipen=999) #not a fan
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
# load MBG packages
core_repo <- file.path(h_root, '_code/lbd/ort/')
package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

#use your own diacritics fx, due to inscrutable error
#note: requires mgsub pkg
#TODO submit PR
fix_diacritics <<- function(x) {
  
  #first define replacement patterns as a named list
  defs <-
    list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 
         'Ç'='C', 'È'='E', 'É'='E','Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 
         'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U','Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 
         'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c','è'='e', 'é'='e', 'ê'='e', 
         'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
         'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y', 'ß'='Ss')
  
  #then force conversion to UTF-8 and replace with non-diacritic character
  enc2utf8(x) %>% 
    mgsub(., pattern=enc2utf8(names(defs)), replacement = defs) %>% 
    return
  
}

#format aggregated results files at admin 0/1/2 lvls
format_admin_results <- function(ind_gp,
                                 ind,
                                 rd,
                                 measure,
                                 suffix,
                                 var_names = ind, # name using ind by default, but can pass custom name
                                 rk) {
  
  #helper function to load a list of admin results and format them into a single DT
  load_admins <- function(i) { 
    
    message('~>loading results for: ', ind[[i]])

    # def directory, then read in all the admin objects from a single RData file
    combined_file <- file.path('/share/geospatial/mbg', ind_gp[[i]], ind[[i]], 'output', rd[[i]]) %>% 
      paste0(., '/', ind[[i]], 
             ifelse(rk[[i]], '_raked', '_unraked'), 
             measure[[i]], '_admin_draws', suffix[[i]], '.RData')
    
    #helper function to create the combined file if it is not present
    create_combined_results <- function(file, sfx='_0.RData') {
      
      message('combined file not present...building')

      #find all relevant files
      files <- list.files(gsub(basename(file), '', file), pattern = 'admin_draws', full.names = T) %>% 
        .[grep(sfx, .)]
      
      load_specific_obj <- function(file, obj) {
        
        message('loading ->', obj, ' from: ', file)
        
        #loads an RData file, and returns the requested object
        load(file)
        ls()[ls() == obj] %>% 
          get %>% 
          return
        
      }
      
      #load each of the required objects from all files
      admin_0 <- lapply(files, load_specific_obj, obj='admin_0') %>% rbindlist
      admin_1 <- lapply(files, load_specific_obj, obj='admin_1') %>% rbindlist
      admin_2 <- lapply(files, load_specific_obj, obj='admin_2') %>% rbindlist
      sp_hierarchy_list <- lapply(files, load_specific_obj, obj='sp_hierarchy_list') %>% rbindlist
      
      #return objects in a named list (we can assign this to the parent env using list2env)
      list('admin_0'=admin_0, 'admin_1'=admin_1, 'admin_2'=admin_2, 'sp_hierarchy_list'=sp_hierarchy_list) %>% 
        return
      
    }
    
    #load the combined file (create from all files if has not already been created)
    if (combined_file %>% file.exists) load(combined_file, verbose=T)
    else { 
      l <- create_combined_results(combined_file)
      list2env(l, envir = environment()) #TODO cannot pipe using list2env?
      rm(l) #cleanup
    }
    
    if (i==4) browser()

    # harmonize the hierarchy lists (some are using factor variables which don't merge well)
    factor_vars <- names(sp_hierarchy_list) %>% .[vapply(., is.factor, c(is.factor=FALSE))]
    
    if (length(factor_vars) > 0) {
      
      message('formatting spatial hierarchy table to num/chr - input table uses factor variables')
      
      #build helper functions
      format_hierarchy <- function(dt) {
        
        out <- copy(dt)
        
        #helper fx to convert factors without information loss
        #https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
        facToNum <- function(f) as.numeric(as.character(f))
        
        #we want to convert the codes to num and the names to chr
        cols_to_num <- factor_vars[factor_vars %like% 'CODE']
        cols_to_chr <- factor_vars[factor_vars %like% 'NAME']
        cols <- list(cols_to_num, cols_to_chr)
        funs <- rep(c(facToNum, as.character), lengths(cols))
        
        # convert class based on column name
        out <- out[, unlist(cols) := Map(function(f, x) f(x), funs, .SD), .SDcols = unlist(cols)] %>% 
          return
        
      }
      
      #convert vars
      sp_hierarchy_list <- format_hierarchy(sp_hierarchy_list)
      
      #reassess and test
      #TODO add tests to ensure no information loss?
      if(vapply(sp_hierarchy_list, is.factor, c(is.factor=FALSE)) %>% any) stop('Failed to convert sp hierarchy!')
      
    }
    
    # format and append
    bind_results <- function(input_dt, info) {
      
      dt <- copy(input_dt)
      
      #pull out the level of aggregation, rename the variable to harmonize and record the value for later
      lvl_str <- names(dt) %>% .[. %like% 'CODE']
      lvl <- substr(lvl_str, start=1, stop=4) #extract level value
      setnames(dt, lvl_str, 'code') # rename
      dt[, agg_level := lvl] #record the level
      dt[, c('pop', 'region') := NULL] #remove unecessary vars
      
      #merge on location names while simultaneously formatting them
      out <- merge(dt,
                   info[, .(code=get(lvl_str), 
                            name=paste0(lvl, '_NAME') %>% get)] %>% unique, 
                   by='code',
                   all.x=T)
      
      #melt and output
      melt(out,
           measure = patterns("V"),
           variable.name = "draw",
           value.name = var_names[[i]]) %>% 
        return
      
    }
    
    dt <- list(admin_0, admin_1, admin_2) %>% 
      lapply(., bind_results, info=sp_hierarchy_list) %>% 
      rbindlist %>% 
      return
    
  }
  
  #load/format all the admin results and then merge them together
  dt <- lapply(1:length(ind), load_admins) %>% 
    Reduce(function(...) merge(..., all = TRUE), .) %>% 
    return
  
}

#***********************************************************************************************************************

# ---OPTIONS------------------------------------------------------------------------------------------------------------
## indicate whether running interactively
interactive <- TRUE

## if running interactively, set arguments
if (interactive) {

  ## set arguments
  indicator_groups         <- list('ort', 'ort', 'ort',
                                   'wash', 'wash', 'wash',
                                   'wash', 'wash', 'wash',
                                   'child_growth_failure', 'child_growth_failure', 'child_growth_failure',
                                   'child_growth_failure', 'child_growth_failure', 'child_growth_failure')
  
  indicators               <- list('rhf', 'ors', 'had_diarrhea', 
                                   'w_piped', 'w_imp_cr', 'w_unimp_cr',
                                   's_piped', 's_imp_cr', 's_unimp_cr',
                                   'wasting_mil_c', 'wasting_mod_c', 'wasting_sev_c',
                                   'stunting_mil_c', 'stunting_mod_c', 'stunting_sev_c')
  
  run_dates                <- list('2019_05_15_full_OOS', '2019_05_15_full_OOS', '2019_05_15_full_OOS', 
                                   '2019_05_20_00_00_02', '2019_05_20_00_00_02', '2019_05_20_00_00_02',
                                   '2019_05_20_00_00_02', '2019_05_20_00_00_02', '2019_05_20_00_00_02',
                                   '2019_04_26_13_44_30', '2019_04_26_13_44_30', '2019_04_26_13_44_30',
                                   '2019_04_26_13_44_30', '2019_04_26_13_44_30', '2019_04_26_13_44_30')
  
  #use to denote if a specific measure is required (creates the proper filepath)
  measures                 <- list('', '', '_deaths', 
                                   '', '', '',
                                   '', '', '',
                                   '', '', '',
                                   '', '', '')
  
  suffixes                 <- list('_eb_bin0_0', '_eb_bin0_0', '_eb_bin0_0', 
                                   '_eb_bin0_0', '_eb_bin0_0', '_eb_bin0_0', 
                                   '_eb_bin0_0', '_eb_bin0_0', '_eb_bin0_0', 
                                   '_eb_bin0_0', '_eb_bin0_0', '_eb_bin0_0',
                                   '_eb_bin0_0', '_eb_bin0_0', '_eb_bin0_0')
  
  #use to denote if you want raked or unraked results
  rks                      <- list(F, F, T, 
                                   F, F, F,
                                   F, F, F,
                                   T, T, T, 
                                   T, T, T)


} else {
  
  ## otherwise, grab arguments from qsub
  ## note this requires a shell script with "<$1 --no-save $@", because its starting at 4
  #TODO

} 

# print out session info so we have it on record
sessionInfo()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~ Prep MBG inputs/Load Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PID <- Sys.getpid()
tic("Entire script") # Start master timer

## Set seed for reproducibility
message('Setting seed 98118 for reproducibility')
set.seed(98118)

# Make the admin results tables ------------------------------------------------------------
tic('Make table')
dt <- format_admin_results(ind_gp = indicator_groups,
                           ind = indicators,
                           #var_names = list('a', 'b', 'c'), #by default use ind name, but can use to simplify formula
                           rd = run_dates,
                           measure = measures,
                           suffix = suffixes,
                           rk = rks)

#reset key 
#TODO made need to redefine depending on desired calculations
setkey(dt, code, year, agg_level, draw)
toc(log = TRUE)

#calculations?
#made up calculation
tic('Calculations')
dt[, d := a/c * b^2]
dt[, `:=` (a = NULL, b = NULL, c = NULL)] #no longer needed
toc(log = TRUE)

#return uncertainty
tic('Summarize CI')
setkey(dt, code, year, agg_level)
summary <- dt %>% 
  copy %>% #do not modify in place for the purposes of this demo
  .[, mean := lapply(.SD, mean, na.rm=T), by=key(.), .SDcols='d'] %>% 
  .[, lower := lapply(.SD, quantile, 0.025, na.rm=T), by=key(.), .SDcols='d'] %>% 
  .[, upper := lapply(.SD, quantile, 0.975, na.rm=T), by=key(.), .SDcols='d'] %>% 
  .[, `:=` (d = NULL, draw = NULL)] %>%  #no longer needed
  unique(., by=key(.))
toc(log=TRUE)

#reshape wide
tic('Reshaping back to wide')
out <- dcast(dt, ... ~ draw, value.var= 'd')
toc(log = TRUE)

# finish up and save
#TODO
tic('Saving')
share_dir <- '/home/j/temp/jfrostad/output/admin_draws'
write.fst(dt, path = file.path(share_dir, 'ort_wash_cgf_admin_draws.fst'))
write.csv(dt, file = file.path(share_dir, 'ort_wash_cgf_admin_draws.csv'), row.names=F)
toc(log = TRUE)


 
#*********************************************************************************************************************** 
