
series       = as.character(commandArgs()[3])

core_repo <- '/share/code/geospatial/lbd_core/'

## Load libraries and miscellaneous MBG project functions.
        setwd(core_repo)
        root <- "/home/j/"
        package_lib <- paste0(root,'/temp/geospatial/packages') # Library for all MBG versioned packages. Ensures that none of this code is
        #    dependent on the machine where the user runs the code.
        .libPaths(package_lib)                                  # Ensures packages look for dependencies here when called with library(). 
        #    Necessary for seeg libraries.
        source('mbg_central/mbg_functions.R')                   # Functions to run MBG model.
        source('mbg_central/prep_functions.R')                  # Functions to setup MBG run
        source('mbg_central/covariate_functions.R')             # Functions to prep and transform 5*5 covariates
        source('mbg_central/misc_functions.R')                  # Other computational MBG-related functions.
        source('mbg_central/post_estimation_functions.R')
        source('mbg_central/gbd_functions.R')
        source('mbg_central/shiny_functions.R')
        source('mbg_central/holdout_functions.R')
        source('education/pop_splitting_functions.R')
        source('mbg_central/seegMBG_transform_functions.R')
        source('mbg_central/validation_report_functions.R')
        package_list <- c('gridGraphics' ,'grid', 'gridExtra', 'gstat', 'magrittr', 'ggplot2', 'doParallel', 'SDMTools', 'foreign', 'rgeos', 'data.table','raster','rgdal','INLA','seegSDM','seegMBG','plyr','dplyr','leaflet')
        for(package in package_list) {
          library(package, lib.loc = package_lib, character.only=TRUE)
        }
  

# Load DHS geography codebook
    geo_file <- paste("/snfs1/WORK/11_geospatial/05_survey shapefile library/codebooks/", series, ".csv", sep = "")
    geo_map <- fread(geo_file, verbose = FALSE)
    setnames(geo_map, 'geospatial_id', 'cluster_number')
    geo_map <- geo_map[, cluster_number := as.numeric(cluster_number)]
    geo_poly_map <- geo_map[geo_map$point!=1]
        dhs_poly_map <- unique(geo_poly_map[, c('iso3','start_year','cluster_number','location_code','admin_level', 'shapefile'), with = FALSE])
        setnames(dhs_poly_map, 'start_year', 'year')

  source('mbg_central/graph_data_coverage.R')
  # Make a table of shapefiles & associated location codes
  df_shape_loc <- unique(dhs_poly_map[, c("shapefile", "location_code")])
  
  # Pull all polygons in parallel
  message("Pulling polys in parallel. This may take a while ...")
  poly_list <- pull_polys_in_parallel(shape_loc_list = df_shape_loc,
                                      shapefile_col = "shapefile",
                                      location_code_col = "location_code",
                                      cores = 20)
  poly_shapes_all <- poly_list[["poly_shapes_all"]]
  message("Done pulling polys")
  
  
  save_dir <- '/share/geospatial/mbg/survey_data_coverage'
  
  save(list='poly_shapes_all', file = paste0(save_dir, '/', series, '_polygons.RData'))
  
  ## Save points
  geo_point_map <- geo_map[geo_map$point==1]
  dhs_point_map <- unique(geo_point_map[, c('iso3','start_year','cluster_number','lat','long'), with = FALSE])
  setnames(dhs_point_map, 'start_year', 'year')
  
  save(list='dhs_point_map', file = paste0(save_dir, '/', series, '_points.RData'))
  
