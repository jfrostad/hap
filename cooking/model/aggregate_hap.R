##############################################################################
## MBG aggregate results launcher script for ORT
## Written by Kirsten Wiens
## Created 2018/09/17
##############################################################################


## Setup -------------------------------------------------------------------------

## clear environment
rm(list=ls())

#running interactively?
debug <- F
debug.args <- c('simulate',
                'command',
                'args',
                'jfrostad',
                "/homes/jfrostad/_code/lbd/hap",
                'cooking',
                'cooking_clean',
                'ort_best',
                'ort_standard',
                'dia_sssa',
                'proj_geo_nodes_dia',
                TRUE,
                '2019_02_05_14_21_43',
                'total')

## Set repo location, indicator group, and some arguments
if (debug!=T) {
  
  user            <- commandArgs()[4]
  repo            <- commandArgs()[5]
  indicator_group <- commandArgs()[6]
  indicator       <- commandArgs()[7]
  config_par      <- commandArgs()[8]
  cov_par         <- commandArgs()[9]
  Regions         <- commandArgs()[10]
  core_repo       <- repo
  
} else{
  
  user            <- debug.args[4]
  repo            <- debug.args[5]
  indicator_group <- debug.args[6]
  indicator       <- debug.args[7]
  config_par      <- debug.args[8]
  cov_par         <- debug.args[9]
  Regions         <- debug.args[10]
  core_repo       <- repo

}

message(indicator)

## drive locations
root           <- ifelse(Sys.info()[1]=='Windows', 'J:/', '/home/j/')
sharedir       <- sprintf('/share/geospatial/mbg/%s/%s',indicator_group,indicator)
commondir      <- sprintf('/share/geospatial/mbg/common_inputs')
package_list <- c(t(read.csv(sprintf('%s/package_list.csv',commondir),header=FALSE)))

# Load MBG packages and functions
message('Loading in required R packages and MBG functions')
source(paste0(repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = repo)

## Throw a check for things that are going to be needed later
message('Looking for things in the config that will be needed for this script to run properly')

## Read config file and save all parameters in memory
config <- load_config(repo            = core_repo,
                      indicator_group = indicator_group,
                      indicator       = indicator,
                      config_name     = paste0('/model/configs/config_', config_par),
                      covs_name       = paste0('/model/configs/covs_', cov_par))

## Set to prod or geos nodes
proj <- ifelse(!debug, commandArgs()[11], debug.args[11])
use_geos_nodes <- ifelse(!debug, commandArgs()[12], debug.args[12])

## Set run date
run_date <- ifelse(!debug, commandArgs()[13], debug.args[13])

## Create output folder with the run_date
outputdir      <- paste('/share/geospatial/mbg', indicator_group, indicator, 'output', run_date, '', sep='/')

## Create proper year list object
if (class(year_list) == 'character') year_list <- eval(parse(text=year_list))

## Ensure you have defined all necessary settings in your config
check_config()

## Get measure to aggregate
pop_measure <- ifelse(!debug, commandArgs()[14], debug.args[14])


## Aggregate to admin2, admin1 and admin0 -------------------------------------------------------------------------

message('Submitting aggregation script')

submit_aggregation_script(indicator       = indicator, 
                          indicator_group = indicator_group,
                          run_date        = run_date,
                          raked           = FALSE,
                          pop_measure     = pop_measure,
                          overwrite       = T,
                          ages            = 0,
                          holdouts        = ifelse(makeholdouts, 1, 0),
                          regions         = Regions,
                          corerepo        = repo,
                          log_dir         = outputdir,
                          slots           = ifelse(use_geos_nodes, 10, 20),
                          proj            = proj,
                          geo_nodes       = as.logical(use_geos_nodes),
                          singularity     = 'default',
                          modeling_shapefile_version = modeling_shapefile_version,
                          raking_shapefile_version = raking_shapefile_version)