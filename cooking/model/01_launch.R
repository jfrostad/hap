###############################################################################
## MBG launch script
##
## Modified for ORT by Kirsten Wiens starting on 2018/08/10
##
###############################################################################


## Setup -------------------------------------------------------------------------

## Clear environment
rm(list=ls())

## Set repo location, indicator group, and some arguments
user            <- commandArgs()[4]
core_repo       <- commandArgs()[5]
indicator_group <- commandArgs()[6]
indicator       <- commandArgs()[7]
config_par      <- commandArgs()[8]
cov_par         <- commandArgs()[9]
Regions         <- commandArgs()[10]
parallel_script <- commandArgs()[11]
repo       <- core_repo
message(indicator)

## Singularity version
sing_dir <- '/share/singularity-images/lbd/testing_INLA_builds/'
which_sing <- file.path(sing_dir, 'lbd_rpkgs3.6.0gcc9mklrstudioserver1.2.1511_v3.simg')
which_sing <- file.path(sing_dir, 'lbd_rpkgs3.6.0gcc9mklrstudioserver1.2.1511_v4.simg')

## Load MBG packages
package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

## Throw a check for things that are going to be needed later
message('Looking for things in the config that will be needed for this script to run properly')

## Read config file and save all parameters in memory
config <- load_config(repo            = core_repo,
                      indicator_group = indicator_group,
                      indicator       = indicator,
                      config_name     = paste0('/model/configs/config_', config_par),
                      covs_name       = paste0('/model/configs/covs_', cov_par))

## Set some covariate options
plot_covariates <- commandArgs()[12]
covariate_plotting_only <- commandArgs()[13]

## Set to prod or geos nodes
proj_arg <- commandArgs()[14]
message(proj_arg)
use_geos_nodes <- commandArgs()[15]

## Set run date and whether running individual countries
run_date <- commandArgs()[16]
individual_countries <- commandArgs()[17]

## Create output folder with the run_date
outputdir      <- paste('/share/geospatial/mbg', indicator_group, indicator, 'output', run_date, '', sep='/')
dir.create(outputdir, recursive=T)

## Create directory structure for this model run
create_dirs(indicator_group = indicator_group, indicator = indicator)

## Create proper year list object
if (class(year_list) == 'character') year_list <- eval(parse(text=year_list))

## Ensure you have defined all necessary settings in your config
check_config()

## If running individual countries make sure all country FEs and REs off
individual_countries <- commandArgs()[17]
if (individual_countries) {
  use_child_country_fes <- FALSE
  use_inla_country_fes  <- FALSE
  use_inla_country_res  <- FALSE
}

## If not running a model with India make sure all subnational REs off
if (Regions != 'dia_south_asia' & Regions != 'IND') {
  use_subnat_res <- FALSE
}

## Set BRT parameters from optimizer sheet
if (stacked_fixed_effects %like% 'gbm') {
  
  gbm_params <- paste0(core_repo, indicator_group, '/model/configs/gbm_params.csv') %>% 
    fread(stringsAsFactors=F)
  gbm_tc <- gbm_params[indi == indicator & region == Regions, gbm_tc]
  gbm_lr <- gbm_params[indi == indicator & region == Regions, gbm_lr]
  gbm_bf <- gbm_params[indi == indicator & region == Regions, gbm_bf]
  gbm_nminobs <- gbm_params[indi == indicator & region == Regions, gbm_nminobs]
  gbm_ntrees <- gbm_params[indi == indicator & region == Regions, gbm_ntrees]
  gbm_cv <- gbm_params[indi == indicator & region == Regions, gbm_cv]
  
} else {
  
  gbm_cv <- NA
  gbm_tc <- NA
  gbm_bf <- NA
  
}

# ## Set xgboost options
# if (any(grepl('xgboost', stacked_fixed_effects))) {
#   
#   xg_grid_search <- T #set TRUE to search the default grid, FALSE for adaptive random search
#   xg_ensemble <- T #set TRUE in order to build/return an xgboost ensemble using caretensemble
#   xg_second_best <- F #set TRUE if you want to return/use the top 2 xgb models instead of the ensemble
#   
# }

# ## Record model parameters in google sheet ORZ Model Tracker
# library('googlesheets')
# model_params <- c(indicator_group, indicator, run_date, outputdir,
#                   covariate_plotting_only, individual_countries, Regions,
#                   config_par, cov_par, fixed_effects, gbd_fixed_effects,
#                   stacked_fixed_effects, use_inla_country_fes, use_nid_res, gam_knots, gbm_cv, gbm_bf, gbm_tc,
#                   mesh_t_knots, rho_prior, theta_prior, nugget_prior, ctry_re_prior, ctry_re_sum0,
#                   spde_integrate0, makeholdouts, use_s2_mesh, modeling_shapefile_version, parallel_script)
# load(paste0(core_repo, '/ors/3_modeling/gs_authorization_token.RData'))
# gs_auth(token = ttt)
# orz_tracker <- gs_title('ORZ model tracker')
# gs_add_row(orz_tracker, ws = 'Sheet1', input = model_params)
# rm(orz_tracker)


## Make holdouts -------------------------------------------------------------------------

if(as.logical(makeholdouts) & !as.logical(skiptoinla)){
  message('Making holdouts')
  
  set.seed(98112)
  
  # load the full input data
  df <- load_input_data(indicator   = indicator,
                        simple      = NULL,
                        removeyemen = FALSE,
                        years       = yearload,
                        yl          = year_list,
                        withtag     = as.logical(withtag),
                        datatag     = datatag,
                        use_share   = as.logical(use_share))
  
  # add in location information
  df <- merge_with_ihme_loc(df, re = Regions, shapefile_version = modeling_shapefile_version)
  
  # remove data for countries outside of region
  df <- df[!is.na(region)]
  
  # make holdouts based on nids, if selected
  if (holdout_strategy == 'nids') {
    
    # make sure we have enough nids to do holdouts
    if (length(unique(df$nid)) < 5) {
      n_ho_folds <- length(unique(df$nid))
    }
    
    # make a list of dfs for each region, with 5 nid folds identified in each
    stratum_ho <-   make_folds(data       = df,
                               n_folds    = as.numeric(n_ho_folds),
                               spte_strat = 'nids',
                               strat_cols = 'region',
                               ss_col     = ss_col,
                               yr_col     = yr_col,
                               seed       = 98112,
                               save.file = paste0('/share/geospatial/mbg/',
                                                  indicator_group, '/',
                                                  indicator, '/output/',
                                                  run_date, '/stratum_', Regions, '.rds'))
  } 
  
  # make holdouts based on admin 1, if selected
  if (holdout_strategy == 'admin') {
    
    # Load simple polygon template by region
    gaul_list           <- get_adm0_codes(Regions, shapefile_version = modeling_shapefile_version)
    simple_polygon_list <- load_simple_polygon(gaul_list = gaul_list, buffer = 1, tolerance = 0.4, shapefile_version = modeling_shapefile_version)
    subset_shape        <- simple_polygon_list[[1]]
    
    # Load simple raster by region
    raster_list        <- build_simple_raster_pop(subset_shape)
    simple_raster      <- raster_list[['simple_raster']]
    
    # load admin 2 shapefile and crop by region
    shapefile_admin <- shapefile(get_admin_shapefile(admin_level = 2, version = modeling_shapefile_version))
    shapefile_admin <- gBuffer(shapefile_admin, byid = TRUE, width = 0)
    shapefile_admin <- crop(shapefile_admin, extent(subset_shape))
    
    # load mask raster and crop by region
    raster_mask <- raster('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/global_files/global_mask_master.grd')
    raster_mask <- crop(raster_mask, extent(simple_raster))
    
    # make admin 2 holdouts
    stratum_ho <- make_folds(data = df,
                             n_folds = as.numeric(n_ho_folds),
                             spat_strat = 'poly',
                             temp_strat = 'prop',
                             strat_cols = 'region',
                             ts = as.numeric(ho_ts),
                             mb = as.numeric(ho_mb),
                             admin_shps = shapefile_admin,
                             admin_raster = simple_raster,
                             mask_shape = subset_shape,
                             mask_raster = raster_mask,
                             shape_ident = 'ADM2_CODE',
                             lat_col = lat_col,
                             long_col = long_col,
                             ss_col = ss_col,
                             yr_col = yr_col,
                             seed = 98112,
                             save.file = paste0('/share/geospatial/mbg/',
                                                indicator_group, '/',
                                                indicator, '/output/',
                                                run_date, '/stratum_', Regions, '.rds'))
  } 
  
}

## Launch parallel script -------------------------------------------------------------------------

## Make loopvars aka strata grid (format = regions, ages, holdouts)
if(as.logical(makeholdouts)) loopvars <- expand.grid(Regions, 0, 0:as.numeric(n_ho_folds)) else loopvars <- expand.grid(Regions, 0, 0)

## loop over them, save images and submit qsubs
for(i in 1:nrow(loopvars)){
  
  message(paste(loopvars[i,2],as.character(loopvars[i,1]),loopvars[i,3]))
  
  # check that holdouts exist
  if (!as.logical(skiptoinla)) {
    if (loopvars[i, 3] > 0) {
      count <- sum(stratum_ho[[paste0('region__', loopvars[i, 1])]]$fold %in% loopvars[i, 3])
      if (count == 0) next
    }
  }
  
  # set cores by region
  r <- as.character(loopvars[i,1])
  region_cores <- 8
  if(r == 'NGA' | r == 'PAK' | r == 'KEN' | r == 'ZWE' | r == 'dia_central_asia') region_cores <- 4
  if(r == 'dia_se_asia' | r == 'dia_sssa' | r == 'MNG' | r == 'COD') region_cores <- 6
  if(r == 'dia_malay' | r == 'dia_name' | r == 'dia_wssa-nga' | r == 'dia_afr_horn') region_cores <- 10
  if(r == 'dia_chn_mng' | r == 'dia_wssa' | r =='dia_south_asia' | r=='dia_s_america-BRA') region_cores <- 12
  if(r == 'dia_s_america' | r == 'dia_chn_mng' | r == 'dia_wssa') region_cores <- 25
  if(loopvars[i, 3] > 0) region_cores <- round(region_cores*0.8)
  
  # convert cores approximately to memory and run time
  region_rt <- '06:00:00:00'
  if (region_cores < 9) region_rt <- '04:00:00:00'
  if (region_cores < 6) region_rt <- '03:12:00:00'
  if (region_cores > 9) region_rt <- '16:00:00:00'
  region_mem <- region_cores*35 #TODO qpid dawg
  
  # set thread options
  threads <- ifelse(region_cores>10, 2, 6)
  
  # make a qsub string
  qsub <- make_qsub_share(age           = loopvars[i,2],
                          reg           = as.character(loopvars[i,1]),
                          holdout       = loopvars[i,3],
                          test          = F,
                          indic         = indicator,
                          saveimage     = TRUE,
                          memory        = region_mem,
                          cores         = threads,
                          run_time      = region_rt,
                          proj          = proj_arg,
                          geo_nodes     = as.logical(use_geos_nodes),
                          corerepo      = core_repo,
                          code          = parallel_script,
                          addl_job_name = paste0(indicator, '_', as.character(loopvars[i,1]), '_parallel'),
                          singularity   = which_sing,
                          singularity_opts = list(SET_OMP_THREADS=threads, SET_MKL_THREADS=threads))
  
  message(qsub) #for posterity
  
  # submit job
  system(qsub)
  
}
