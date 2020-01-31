###############################################################################
## Parallel script for post-estimation and aggregation
##
## Modified for ORT and diarrhea by Kirsten Wiens on 2019/06/03
##
###############################################################################
# source('/homes/jfrostad/_code/lbd/hap/mbg_central/share_scripts/frax_script_hap.R') 

## Setup ---------------------------------------------------------------------------------------------------
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
pacman::p_load(data.table, dplyr, mgsub, raster, sf, fasterize, fst)

#detect if running in rstudio IDE
debug <- F
interactive <- ifelse(debug, T, !(is.na(Sys.getenv("RSTUDIO", unset = NA))))

if (interactive) {
  
  ## Set repo location, indicator group, and some arguments
  user <- 'jfrostad'
  core_repo <- "/homes/jfrostad/_code/lbd/hap"
  indicator_group <- 'cooking'
  indicator <- 'cooking_fuel_solid'
  config_par   <- 'hap_best'
  holdout <- 0
  age <- 0
  run_date <- '2020_01_15_18_33_30'
  measure <- 'prevalence'
  reg <- 'BRA'
  cov_par <- paste(indicator_group, reg, sep='_')
  
} else {
  
  ## Set repo location, indicator group, and some arguments
  user            <- commandArgs()[4]
  core_repo       <- commandArgs()[5]
  indicator_group <- commandArgs()[6]
  indicator       <- commandArgs()[7]
  config_par      <- commandArgs()[8]
  cov_par         <- commandArgs()[9]
  reg             <- commandArgs()[10]
  run_date        <- commandArgs()[13]
  measure         <- commandArgs()[14]
  holdout         <- as.numeric(commandArgs()[15])
  age             <- 0

}

## Load MBG packages
package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

#use your own diacritics fx, due to inscrutable error
#note: requires mgsub pkg
#TODO submit PR
fix_diacritics <<- function(x) {
  
  require(mgsub)
  
  #first define replacement patterns as a named list
  defs <-
    list('??'='S', '??'='s', '??'='Z', '??'='z', '??'='A', '??'='A', '??'='A', '??'='A', '??'='A', '??'='A', '??'='A', 
         '??'='C', '??'='E', '??'='E','??'='E', '??'='E', '??'='I', '??'='I', '??'='I', '??'='I', '??'='N', '??'='O', 
         '??'='O', '??'='O', '??'='O', '??'='O', '??'='O', '??'='U','??'='U', '??'='U', '??'='U', '??'='Y', '??'='B', 
         '??'='a', '??'='a', '??'='a', '??'='a', '??'='a', '??'='a', '??'='a', '??'='c','??'='e', '??'='e', '??'='e', 
         '??'='e', '??'='i', '??'='i', '??'='i', '??'='i', '??'='o', '??'='n', '??'='o', '??'='o', '??'='o', '??'='o',
         '??'='o', '??'='o', '??'='u', '??'='u', '??'='u', '??'='y', '??'='y', '??'='b', '??'='y', '??'='Ss')
  
  #then force conversion to UTF-8 and replace with non-diacritic character
  enc2utf8(x) %>% 
    mgsub(., pattern=enc2utf8(names(defs)), replacement = defs) %>% 
    return
  
}

## Load custom post-estimation functions
file.path(core_repo, 'post_estimation/_lib', 'aggregate_inputs.R') %>% source

## Read config file and save all parameters in memory
config_filepath <- 'cooking/model/configs/'
config <- set_up_config(repo            = core_repo,
                        indicator_group = indicator_group,
                        indicator       = indicator,
                        config_name     = paste0('/model/configs/config_', config_par),
                        covs_name       = paste0('/model/configs/covs_', cov_par))

# Get the necessary variables out from the config object into global env
rake_countries <- eval(parse(text = config[V1 == 'rake_countries', V2]))
rake_subnational <- eval(parse(text = config[V1 == 'subnational_raking', V2]))
modeling_shapefile_version <- config[V1 == 'modeling_shapefile_version', V2]
raking_shapefile_version <- config[V1 == 'raking_shapefile_version', V2]
countries_not_to_rake <- config[V1 == 'countries_not_to_rake', V2]
countries_not_to_subnat_rake <- config[V1 == 'countries_not_to_subnat_rake', V2]
year_list <- eval(parse(text = config[V1 == 'year_list', V2]))
metric_space <- config[V1 == 'metric_space', V2]

## Set filepath and pathaddin
sharedir <- sprintf("/share/geospatial/mbg/%s/%s", indicator_group, indicator)
outputdir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date, '/')
pathaddin <- paste0('_bin0_', reg, '_', holdout)

# Print some settings to console
message(indicator)
message(indicator_group)
message(run_date)
message(reg)
message(pop_measure)
message(holdout)


## Define raking parameters ---------------------------------------------------------------------------

## If this is a HAP indicator model run set all countries to not be raked
if (indicator %like% 'cooking') {

  # Create function to pull isos for diarrhea custom regs
  get_region_isos <- function(modeling_region) {
    
    # define regions
    region_list <- list(
      'dia_afr_horn' = 'dji+eri+eth+sdn+som+ssd+yem',
      'dia_cssa-cod' = 'ago+caf+cog+gab+gnq+stp',
      'dia_wssa-nga' = 'ben+bfa+civ+cmr+cpv+gha+gin+gmb+gnb+lbr+mli+mrt+ner+sen+sle+tcd+tgo',
      'dia_name' = 'dza+egy+esh+lby+mar+tun',
      'dia_sssa' = 'bwa+nam+zaf',
      'dia_mcaca' = 'blz+cri+cub+dma+dom+grd+gtm+hnd+hti+jam+lca+mex+nic+pan+slv+vct',
      'dia_s_america-bra' = 'bol+col+ecu+guf+guy+per+pry+sur+tto+ven',
      'dia_central_asia' = 'kgz+tjk+tkm+uzb',
      'dia_se_asia' = 'khm+lao+mmr+mys+tha+vnm',
      'dia_malay' = 'idn+phl+png+tls',
      'dia_south_asia-ind-pak' = 'bgd+btn+lka+npl',
      'dia_mid_east' = 'afg+irn+irq+jor+pse+syr',
      'dia_essa-zwe-ken' = 'bdi+com+lso+mdg+moz+mwi+rwa+swz+syc+tza+uga+zmb',
      'PAK' = 'pak',
      'KEN' = 'ken',
      'NGA' = 'nga',
      'COD' = 'cod',
      'IND' = 'ind',
      'ZWE' = 'zwe',
      'MNG' = 'mng',
      'dia_cssa' = 'ago+caf+cod+cog+gab+gnq+stp',
      'dia_wssa' = 'ben+bfa+civ+cmr+cpv+gha+gin+gmb+gnb+lbr+mli+mrt+ner+nga+sen+sle+tcd+tgo',
      'dia_s_america' = 'bol+bra+col+ecu+guf+guy+per+pry+sur+tto+ven',
      'dia_chn_mng' = 'chn+mng',
      'dia_south_asia' = 'bgd+btn+ind+lka+npl+pak',
      'dia_south_asia-ind' = 'bgd+btn+lka+npl+pak',
      'dia_essa' = 'bdi+com+ken+lso+mdg+moz+mwi+rwa+swz+syc+tza+uga+zmb+zwe'
    )
    # return region list
    return(toupper(region_list[[modeling_region]]))
  }
    
  # apply function
  countries_not_to_rake <- get_region_isos(reg)
  countries_not_to_subnat_rake <- get_region_isos(reg)
  subnational_raking <- FALSE
  
} else {
  
  subnational_raking <- TRUE
  countries_not_to_subnat_rake <- NULL
  
}

# Assume subnational raking unless otherwise speciified
rake_subnational <- ifelse(subnational_raking, T, F)

# Determine if a crosswalk is needed
crosswalk <- ifelse(modeling_shapefile_version != raking_shapefile_version, T, F)

# Force linear raking to avoid issues with logit raking
rake_method <- 'linear'

# Print raking info
message('Metric Space                       : ', metric_space)
message('Subnational raking                 : ', rake_subnational)
message('Countries not to rake at all       : ', countries_not_to_rake)
message('Countries not to rake subnationally: ', countries_not_to_subnat_rake)


## Load GBD estimates -------------------------------------------------------------------------------------
#TODO is this piece necessary? doesnt seem that GBD does anything if not raking
rake_to_path <- '/share/geospatial/kewiens/diarrhea_raking/'
gbd <- as.data.table(read.csv(paste0(rake_to_path, 'gbd_',  measure, '.csv'), stringsAsFactors = FALSE))
setnames(gbd, c('location_id', 'year_id', 'val'), c('name', 'year', 'mean'))

## Load cell pred and populations -----------------------------------------------------------------

# Get the simple and new_simple rasters prepped up for us
message('Loading simple and prepped rasters')
raster_outputs <- prep_shapes_for_raking(
  reg = reg,
  modeling_shapefile_version = modeling_shapefile_version,
  raking_shapefile_version = raking_shapefile_version,
  field = 'loc_id'
)

## Take out the objects from the list that actually matters to us:
simple_raster <- raster_outputs[['simple_raster']]
new_simple_raster <- raster_outputs[['new_simple_raster']]

simple_polygon <- raster_outputs[['simple_polygon']]
new_simple_polygon <- raster_outputs[['new_simple_polygon']]

pixel_id <- raster_outputs[['pixel_id']]

# Load cell draws
message('Loading cell pred')
load(paste0(outputdir, indicator, '_cell_draws_eb_bin0_', reg, '_', holdout, '.RData'))

# Load populations
message('Loading populations')
gbd_pops <- prep_gbd_pops_for_fraxrake(pop_measure = pop_measure, reg = reg, year_list = year_list, gbd_round_id = 5)

## Rake and aggregate -----------------------------------------------------------------

print('Using the rates raking and aggregation functions:')

# if we're raking to etiologies, then loop over them
if (grepl('eti', measure)) {
  
  for (i in unique(gbd$rei_name)) {
    
    # subset data to eti
    gbd <- gbd[rei_name == i]
    
    ## First, create all the fractional rake factors
    fractional_rake_rates(
      cell_pred = cell_pred,
      simple_raster = simple_raster,
      simple_polygon = simple_polygon,
      pixel_id = pixel_id,
      shapefile_version = raking_shapefile_version,
      reg = reg,
      pop_measure = pop_measure,
      year_list = year_list,
      interval_mo = interval_mo,
      rake_subnational = rake_subnational,
      age_group = age_group,
      sex_id = sex_id,
      sharedir = sharedir,
      run_date = run_date,
      indicator = indicator,
      gbd = gbd,
      rake_method = rake_method,
      gbd_pops = gbd_pops,
      countries_not_to_rake = countries_not_to_rake,
      countries_not_to_subnat_rake = countries_not_to_subnat_rake,
      hold = holdout,
      meas = paste0(measure, '_', i)
    )
    
    ## Now, create the raked cell pred files!
    outputs <- fractional_agg_rates(
      cell_pred = cell_pred,
      simple_raster = simple_raster,
      simple_polygon = simple_polygon,
      pixel_id = pixel_id,
      shapefile_version = raking_shapefile_version,
      reg = reg,
      pop_measure = pop_measure,
      year_list = year_list,
      interval_mo = interval_mo,
      rake_subnational = rake_subnational,
      sharedir = sharedir,
      run_date = run_date,
      indicator = indicator,
      main_dir = outputdir,
      rake_method = rake_method,
      age = age,
      holdout = holdout,
      countries_not_to_subnat_rake = countries_not_to_subnat_rake,
      return_objects = TRUE,
      meas = paste0(measure, '_', i)
    )
    
    ## Save mean
    ras  <- insertRaster(new_simple_raster, matrix(rowMeans(outputs[['raked_cell_pred']]), ncol = 18))
    writeRaster(
      ras,
      file = (paste0(outputdir, '/', indicator, '_prediction_', measure, '_', i, '_eb',pathaddin)),
      overwrite = TRUE
    )
    rm(ras)
    
    ## Save lower
    ras  <- insertRaster(new_simple_raster, matrix(apply(outputs[['raked_cell_pred']], 1, lower), ncol = 18))
    writeRaster(
      ras,
      file = paste0(outputdir, '/', indicator,'_lower_', measure, '_', i, '_eb', pathaddin),
      overwrite = TRUE
    )
    rm(ras)
    
    ## Save upper
    ras  <- insertRaster(new_simple_raster, matrix(apply(outputs[['raked_cell_pred']], 1, upper), ncol = 18))
    writeRaster(
      ras,
      file = paste0(outputdir, '/', indicator,'_upper_', measure, '_', i, '_eb', pathaddin),
      overwrite = TRUE
    )
    rm(ras)
    
  }
  
} else { # if we're not raking etiologies, no need to loop   
  
  ## First, create all the fractional rake factors
  fractional_rake_rates(
    cell_pred = cell_pred,
    simple_raster = simple_raster,
    simple_polygon = simple_polygon,
    pixel_id = pixel_id,
    shapefile_version = raking_shapefile_version,
    reg = reg,
    pop_measure = pop_measure,
    year_list = year_list,
    interval_mo = interval_mo,
    rake_subnational = rake_subnational,
    age_group = age_group,
    sex_id = sex_id,
    sharedir = sharedir,
    run_date = run_date,
    indicator = indicator,
    gbd = gbd,
    rake_method = rake_method,
    gbd_pops = gbd_pops,
    countries_not_to_rake = countries_not_to_rake,
    countries_not_to_subnat_rake = countries_not_to_subnat_rake,
    hold = holdout,
    meas = measure
  )
  
  ## Now, create the raked cell pred files!
  outputs <- fractional_agg_rates(
    cell_pred = cell_pred,
    simple_raster = simple_raster,
    simple_polygon = simple_polygon,
    pixel_id = pixel_id,
    shapefile_version = raking_shapefile_version,
    reg = reg,
    pop_measure = pop_measure,
    year_list = year_list,
    interval_mo = interval_mo,
    rake_subnational = rake_subnational,
    sharedir = sharedir,
    run_date = run_date,
    indicator = indicator,
    main_dir = outputdir,
    rake_method = rake_method,
    age = age,
    holdout = holdout,
    countries_not_to_subnat_rake = countries_not_to_subnat_rake,
    return_objects = TRUE,
    meas = measure
  )
  
  ## Save raked summaries if we're not modeling HAP indicators
  if (!(indicator %like% 'cooking')) {
    
    ## Save mean
    ras  <- insertRaster(new_simple_raster, matrix(rowMeans(outputs[['raked_cell_pred']]), ncol = 18))
    writeRaster(
      ras,
      file = (paste0(outputdir, '/', indicator, '_prediction_', measure, '_eb',pathaddin)),
      overwrite = TRUE
    )
    rm(ras)
    
    ## Save lower
    ras  <- insertRaster(new_simple_raster, matrix(apply(outputs[['raked_cell_pred']], 1, lower), ncol = 18))
    writeRaster(
      ras,
      file = paste0(outputdir, '/', indicator,'_lower_', measure, '_eb', pathaddin),
      overwrite = TRUE
    )
    rm(ras)
    
    ## Save upper
    ras  <- insertRaster(new_simple_raster, matrix(apply(outputs[['raked_cell_pred']], 1, upper), ncol = 18))
    writeRaster(
      ras,
      file = paste0(outputdir, '/', indicator,'_upper_', measure, '_eb', pathaddin),
      overwrite = TRUE
    )
    rm(ras)
    
  } else {
    
    ## Delete unnecessary raked files
    to_del <- list.files(outputdir, pattern = paste0('_raked_', measure, '_eb_bin0_', reg, '_', holdout))
    to_del <- c(to_del, list.files(outputdir, pattern = paste0('_raked_', measure, '_admin_draws_eb_bin0_', reg, '_', holdout)))
    to_del <- c(to_del, list.files(outputdir, pattern = paste0('_raked_', measure, '_c_admin_draws_eb_bin0_', reg, '_', holdout)))
    if (length(to_del) > 0) lapply(paste0(outputdir, to_del), file.remove)
  }
  
}

## Aggregate data and stackers ------------------------------------------------------
message('Aggregating data and stackers for lineplots')
# Aggregate data to admin 0 and 1
dat <- aggregate_input_data(reg,
                            indicator, 
                            indicator_group, 
                            run_date,
                            modeling_shapefile_version,
                            build=T)

# Aggregate stackers to admin 0 and 1
stack <- aggregate_child_stackers(reg,
                                  indicator, 
                                  indicator_group, 
                                  run_date, 
                                  modeling_shapefile_version,
                                  pop_measure='total',
                                  build=T)
## Finish up -----------------------------------------------------------------

## Write a file to mark done
write(NULL, file = paste0(outputdir, '/fin_', pathaddin))

## All done
message('Done with post-estimation and aggregation for ', reg)