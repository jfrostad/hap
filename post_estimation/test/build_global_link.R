# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 04/29/2019
# Purpose: Run correlation calculations vs various covariates for TIF files that bobby is using
# source('/homes/jfrostad/_code/lbd/hap/post_estimation/test/build_global_link.R') 
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
pacman::p_load(ccaPP, fasterize, fst, mgsub, wCorr)
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
# load MBG packages
core_repo <- file.path(h_root, '_code/lbd/hap/')
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

build_link_table <- function(shapefile_version,
                             cores,
                             region = "stage1+stage2",
                             custom_shapefile_path = NULL,
                             custom_shapefile_field = NULL) {
  
  #load in packages not included in singularity image
  new_pkg_lib   <- '/share/code/geospatial/jdv6/r_packages'
  .libPaths(c(.libPaths(), paste0("/share/geospatial/non_lbd_rstudio_pkgs/", paste0(R.Version()[c("major", "minor")], collapse = ".")), new_pkg_lib))
  test_pkg_list <- c('sf', 'lwgeom', 'RhpcBLASctl')
  for (pkg in test_pkg_list) {
    if (!pkg %in% as.vector(installed.packages(lib.loc = .libPaths())[, "Package"])) {
      install.packages(pkg, lib = new_pkg_lib)
    }
  }
  lapply(test_pkg_list, library,
         character.only = TRUE)
  
  if(is.null(custom_shapefile_path)){
    #read the admin 2 shapefile
    polys = st_read(paste0('/home/j/WORK/11_geospatial/admin_shapefiles/', shapefile_version,'/lbd_standard_admin_2.shp'), stringsAsFactors = F)
    
    #subset admin 2 shapefile to desired countries
    s1 = get_adm0_codes(region, shapefile_version = shapefile_version)
    polys = polys[polys$ADM0_CODE %in% c(s1),]
    simple_polygon_list <- load_simple_polygon(gaul_list = get_adm0_codes(region, shapefile_version = shapefile_version), buffer = 0.4, subset_only = FALSE)
    subset_shape   <- simple_polygon_list[[1]]
    simple_polygon = simple_polygon_list[[2]]
    message("Building simple raster from subset_shape")
    raster_list    <- build_simple_raster_pop(subset_shape)
    
  } else {
    region = NULL
    polys = st_read(custom_shapefile_path, stringsAsFactors = F, quiet = T)
    simple_polygon_list <- load_simple_polygon(gaul_list = NULL, buffer = 0.4, subset_only = FALSE, custom_shapefile_path = custom_shapefile_path)
    subset_shape   <- simple_polygon_list[[1]]
    simple_polygon = simple_polygon_list[[2]]
    message("Building simple raster from subset_shape")
    raster_list    <- build_simple_raster_pop(subset_shape, field = custom_shapefile_field, link_table = NULL)
  }
  
  #landsea mask
  landsea = raster('/home/j/WORK/11_geospatial/01_covariates/02_Oxford/01_Global_Masks/Land_Sea_Masks/CoastGlobal_5k_float.tif')
  #create a raster version of the pixel_id
  
  simple_raster  <- raster_list[['simple_raster']]
  
  #create pixel_id raster
  px_ras = simple_raster
  px_ras[] = 1:length(px_ras)
  
  #build link polygon, where there is a polygon for each pixel identified by pixel_id
  pixels = build_link_polygon(region, simple_raster = simple_raster)
  
  #fix some names
  setnames(pixels, 'layer', 'pixel_id')
  
  #assign crs
  st_crs(pixels) = 4326
  
  get_intersect = function(poly, x = -1){
    print(x)
    start = Sys.time()
    #make sure the polygon has valid geometry. If not, get the computer to fix it magically
    if (!st_is_valid(poly)) {
      poly = st_make_valid(poly)
    }
    
    #crop the baseline raster
    poly_bb = st_bbox(poly)
    poly_extent = extent(poly_bb[1],poly_bb[3],poly_bb[2],poly_bb[4])
    px_crop = crop(px_ras, poly_extent, snap = 'out')
    px_crop = px_crop[]
    
    #subset possible pixels
    target = pixels[pixels$pixel_id %in% px_crop,]
    
    #get a list of target polys (e.g. zip code) that intersect with the poly (e.g. critical habitat)
    inters = st_intersects(poly, target)
    
    #calculate start area of the poly
    target = target[inters[[1]],]
    target$start_area = st_area(target)
    
    #calculate the intersection
    aaa = st_intersection(poly, target)
    
    #if there is an intersection, calculate the end area, remove the geometry to save space
    
    if (nrow(aaa) > 0) {
      
      aaa$end_area = st_area(aaa)
      
      st_geometry(aaa) = NULL
      
      setDT(aaa)
      ret_obj = aaa
      ret_obj[, area_fraction := end_area/start_area]
    }else{
      st_geometry(poly) = NULL
      ret_obj = poly
      setDT(ret_obj)
    }
    end = Sys.time()
    
    ret_obj$start = start
    ret_obj$end = end
    
    return(ret_obj)
  }
  
  RhpcBLASctl::blas_set_num_threads(1)
  
  #go through id polygons in parallel and calculate intersections with ADM2 polygon
  link_fxed_gaul = parallel::mclapply(1:nrow(polys),
                                      function(x) get_intersect(polys[x,], x = x),
                                      mc.preschedule = F, mc.cores = cores)
  
  link_fxed_gaul = rbindlist(link_fxed_gaul)
  #backwards compatibility
  link_fxed_gaul[, ID := pixel_id]
  #converting from "unit" to numeric
  link_fxed_gaul$area_fraction <- as.numeric(link_fxed_gaul$area_fraction)
  #fixes area_fractions for pixels that are partially in water
  link_fxed_gaul <- fix_link(link_fxed_gaul)
  
  return(list("link_table" = link_fxed_gaul, "id_raster" = px_ras, "id_poly" = pixels))
}
#***********************************************************************************************************************

# ---OPTIONS------------------------------------------------------------------------------------------------------------
## indicate whether running interactively
interactive <- TRUE

## if running interactively, set arguments
if (interactive) {
  warning('interactive is set to TRUE - if you did not mean to run MBG interactively then kill the model and set interactive to FALSE in parallel script')
  
  ## set arguments
  shapefile                   <- '2019_05_06'
  modeling_shapefile_version <- shapefile
  covs                        <- c('access2', 
                                   'diarrhea_prev',
                                   'edu_mean_raked',
                                   'u5m')
  cov_measures                <- c('mean',
                                   'mean',
                                   'median',
                                   'mean')


} else {
  
  ## otherwise, grab arguments from qsub
  ## note this requires a shell script with "<$1 --no-save $@", because its starting at 4
  #TODO

} 

# print out session info so we have it on record
sessionInfo()

## Set seed for reproducibility
message('Setting seed 98118 for reproducibility')
set.seed(98118)
#***********************************************************************************************************************

# ---IN/OUT-------------------------------------------------------------------------------------------------------------
#define dirs
tmp_dir <- '/home/j/temp/jfrostad/'
data_dir <- file.path(tmp_dir, 'data', 'bobby_tifs')
out_dir <- file.path(tmp_dir, 'output', 'bobby_tifs')

#global link info
global_link_dir <- '/share/scratch/tmp/fwlt/' #TODO make official
#read in link_table
global_link_table <- file.path(global_link_dir, "link_table_full_world.rds") %>% readRDS %>%  as.data.table
#read in id_raster
global_id_raster <- file.path(global_link_dir, 'id_raster_full_world.rds') %>% readRDS
#***********************************************************************************************************************

# ---CALCULATE----------------------------------------------------------------------------------------------------------
tic("Entire script") # Start master time

tmp <- build_link_table('current', 30, region='stage1+stage2+stage3')

toc() # End master timer
 
#*********************************************************************************************************************** 