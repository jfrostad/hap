##############################################################################
## MBG aggregate results launcher script for ORT
## Written by Kirsten Wiens
## Created 2018/09/17
##############################################################################


## Setup -------------------------------------------------------------------------

## clear environment
rm(list=ls())

#running interactively?
debug <- T
if (debug) warning('debug is set to TRUE - if you did not mean to run interactively then kill job and turn off debug')
debug.args <- c('simulate',
                'command',
                'args',
                'jfrostad',
                "/homes/jfrostad/_code/lbd/hap",
                'cooking',
                'cooking_fuel_solid',
                'hap_best',
                'covs_cooking_dia_sssa',
                'dia_essa',
                'proj_geo_nodes',
                TRUE,
                '2019_07_15_12_05_42',
                'total',
                0)
#pull args from the job submission if !interactive
args <- ifelse(rep(debug, length(debug.args)), 
               debug.args, 
               commandArgs()) 

## Set repo location, indicator group, and some arguments
user            <- args[4]
core_repo       <- args[5]
indicator_group <- args[6]
indicator       <- args[7]
config_par      <- args[8]
cov_par         <- args[9]
Regions         <- args[10]
holdout         <- args[15]

## Load MBG packages
package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

## Throw a check for things that are going to be needed later
message('Looking for things in the config that will be needed for this script to run properly')

## Read config file and save all parameters in memory
config <- set_up_config(repo            = core_repo,
                        indicator_group = '',
                        indicator       = '',
                        config_name     = paste0('cooking/model/configs/config_', config_par),
                        covs_name       = paste0('cooking/model/configs/', cov_par))

## Set to prod or geos nodes
proj <- args[11]
use_geos_nodes <- args[12]

## Set run date
run_date <- args[13]

## Create output folder with the run_date
outputdir      <- paste('/share/geospatial/mbg', indicator_group, indicator, 'output', run_date, '', sep='/')

## Create proper year list object
if (class(year_list) == 'character') year_list <- eval(parse(text=year_list))

## Get measure to aggregate
measure <- args[14]

# set cores by region
individual_countries <- ifelse(nchar(Regions) == 3, TRUE, FALSE)
r <- Regions
region_cores <- 6
if(r == 'dia_malay' | r == 'dia_name') region_cores <- 8
if(r == 'dia_chn_mng' | r == 'dia_wssa' | r =='dia_south_asia') region_cores <- 10
if(r == 'dia_s_america') region_cores <- 12
if(makeholdouts) region_cores <- round(region_cores*0.8)

# convert cores approximately to memory and run time
region_rt <- '01:00:00:00'
region_mem <- region_cores*15


## Aggregate to admin2, admin1 and admin0 -------------------------------------------------------------------------

message('Submitting aggregation script')


submit_aggregation_script(indicator       = indicator, 
                          indicator_group = indicator_group,
                          run_date        = run_date,
                          raked           = FALSE,
                          pop_measure     = pop_measure,
                          overwrite       = T,
                          ages            = 0,
                          holdouts        = holdout,
                          regions         = Regions,
                          corerepo        = core_repo,
                          log_dir         = outputdir,
                          cores           = 4,
                          memory          = region_mem,
                          run_time        = region_rt,
                          proj            = proj,
                          geo_nodes       = as.logical(use_geos_nodes),
                          singularity     = 'default',
                          measure         = measure,
                          modeling_shapefile_version = modeling_shapefile_version,
                          raking_shapefile_version = raking_shapefile_version)
#***********************************************************************************************************************
