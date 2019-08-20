# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 04/29/2019
# Purpose: Run correlation calculations vs various covariates for TIF files that bobby is using
# source('/homes/jfrostad/_code/lbd/hap/post_estimation/test/test_link.R') 
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
pacman::p_load(ccaPP, fst, mgsub, wCorr)
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

#use the custom link table that Tim Whitson built for stage3
get_link_table <<- function(simple_raster, shapefile_version, global_ids, global_link, debug=F) {
  
  require(raster)
  
  #extract the pixel IDs using the cropped and masked global ID raster
  pixel_ids <- 
    crop(global_ids, simple_raster) %>% #crop id_raster to simple_raster
    mask(., simple_raster) %>% #mask id_raster with simple_raster
    raster::extract(., extent(.), na.rm = T) %>% #extract id_raster
    na.omit
  
  #subset link table by pixel ids from extract
  link_table <- global_link[ID %in% pixel_ids,]
  
  if (debug) {
    #TODO remove when working properly
    #offical link
    old_global_link <- data.table(readRDS(sprintf("/snfs1/WORK/11_geospatial/admin_shapefiles/%s/lbd_standard_link.rds", shapefile_version)))
    #read in id_raster
    old_global_raster <- readRDS(sprintf("/snfs1/WORK/11_geospatial/admin_shapefiles/%s/lbd_standard_id_raster.rds", shapefile_version))
    browser()
    
    #extract the pixel IDs using the cropped and masked global ID raster
    old_pixel_ids <- 
      crop(old_global_raster, simple_raster) %>%  #crop id_raster to simple_raster
      mask(., simple_raster) %>%  #mask id_raster with simple_raster
      raster::extract(., extent(.), na.rm = T) %>% #extract id_raster
      na.omit
    
    #subset link table by pixel ids from extract
    old_link_table <- old_global_link[ID %in% old_pixel_ids,]
  }
  
  #make sure list of pixel ids is same number of nonna cells in simple raster
  if (length(pixel_ids) != length(which(!is.na(getValues(simple_raster))))) {
    stop("The number of pixel_ids does not match the number of non-na values in simple raster. 
         \nPart of the simple raster may be outside the extent of the global id raster.")
  }
  
  if(debug) {
    #return subsetted link table and list of pixel ids
    list("link_table" = link_table, "pixel_ids" = pixel_ids,
         "old_link" = old_link_table, "old_pixel_ids" = old_pixel_ids) %>% 
      return
  } else list("link_table" = link_table, "pixel_ids" = pixel_ids) %>% return
  
  }

build_simple_raster_pop <<- function(subset_shape, u5m=FALSE, field=NULL, raking=F, link_table=modeling_shapefile_version) {
  
  if (is.null(field)) {
    if ('GAUL_CODE' %in% names(subset_shape@data)) field <- 'GAUL_CODE'
    if ('ADM0_CODE' %in% names(subset_shape@data)) field <- 'ADM0_CODE'
  }
  
  if(raking){
    field <- 'loc_id'
    # no 'loc_id' field in the link table, so we can't use it
    link_table <- NULL
  }
  
  if(u5m==FALSE){
    master_pop <- brick('/snfs1/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif') #WorldPop_allStages_stack.tif')
  } else {
    master_pop <- brick(raster('/snfs1/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/archive/replaced_20170623/a0004t/5y/worldpop_a0004t_5y_2000_00_00.tif'),
                        raster('/snfs1/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/archive/replaced_20170623/a0004t/5y/worldpop_a0004t_5y_2005_00_00.tif'),
                        raster('/snfs1/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/archive/replaced_20170623/a0004t/5y/worldpop_a0004t_5y_2010_00_00.tif'),
                        raster('/snfs1/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/archive/replaced_20170623/a0004t/5y/worldpop_a0004t_5y_2015_00_00.tif'))
  }
  
  cropped_pop <- crop(master_pop, extent(subset_shape), snap="out")
  
  ## Fix rasterize
  initial_raster <- rasterize_check_coverage(subset_shape, cropped_pop, field = field, link_table = link_table, global=T)
  if(length(subset(subset_shape, !(get(field) %in% unique(initial_raster))))!=0) {
    rasterized_shape <- 
      raster::merge(
        rasterize_check_coverage(subset(subset_shape, !(get(field) %in% unique(initial_raster))),
                                 cropped_pop,
                                 field = field,
                                 link_table = link_table),
        initial_raster)
  }
  if(length(subset(subset_shape, !(get(field) %in% unique(initial_raster))))==0) {
    rasterized_shape <- initial_raster
  }
  masked_pop <- raster::mask(x=cropped_pop, mask=rasterized_shape)
  
  raster_list <- list()
  raster_list[['simple_raster']] <- rasterized_shape
  raster_list[['pop_raster']] <- masked_pop
  
  return(raster_list)
  
}

rasterize_check_coverage <<- function(shapes, template_raster, field, ..., link_table = modeling_shapefile_version, global=F) {
  # backwards-compatible behavior - just call rasterize()
  if (is.null(link_table)) return(raster::rasterize(shapes, template_raster, field = field, ...))
  
  # Validate arguments
  is_admin_link_table <- FALSE
  if (is.data.table(link_table)) {
    is_admin_link_table <- TRUE
    # nothing to do - already a link table loaded in memory
  } else if (R.utils::isAbsolutePath(link_table)) {
    link_table <- readRDS(link_table)
  } else if (is_admin_shapefile_string(link_table)) {
    is_admin_link_table <- TRUE
    # load link table with pre-computed ownership percentages for each pixel cell
    link_table_file <- paste0(get_admin_shape_dir(link_table), "lbd_standard_link.rds")
    link_table <- readRDS(link_table_file)
  } else {
    stop("link_table argument was neither a data.table, an admin shapefile string, or an absolute path to a RDS file.")
  }
  
  if (! field %in% names(link_table)) {
    msg <- paste("WARNING: rasterize_check_coverage called with field", field,
                 "which is not present in link_table. Defaulting to raster::rasterize()")
    message(msg)
    return(raster::rasterize(shapes, template_raster, field = field, ...))
  }
  
  # aggregate link table generically for admin 0/1/2
  # Note: we need `with=FALSE` because `field` is passed as a parameter (not a hard-coded string)
  table <- link_table[,c("pixel_id", field, "area_fraction"), with=FALSE]
  if (is_admin_link_table && field != "ADM2_CODE") {
    # sum rows; area_fraction now represents the total area coverage by ADM0/1_CODE instead of ADM2_CODE
    table <- table[, .(area_fraction = sum(area_fraction)), by = c("pixel_id", field)]
  }
  # subset table so that we have 1 entry per pixel_id - the value of `field` with the maximum
  # area_fraction value for that pixel_id
  # https://stackoverflow.com/a/24558696
  pixel_owner <- table[table[, .I[which.max(area_fraction)], by=pixel_id]$V1]
  pixel_owner <- pixel_owner[order(pixel_id)]
  
  # generate world raster with pixel values for `field`
  world_pixel_owner <- suppressWarnings(empty_world_raster(whole_world = global))
  # subset to only those pixels owned by a shape we're interested in
  owned_pixels <- pixel_owner[pixel_owner[[field]] %in% shapes[[field]]]
  world_pixel_owner[owned_pixels$pixel_id] <- owned_pixels[[field]]
  
  result <- raster::crop(world_pixel_owner, template_raster, snap="near")
  if (raster::ncell(result) != raster::ncell(template_raster)) {
    message <- paste("Error in creating result raster. Should have created a raster of shape",
                     paste(dim(result), collapse=","),
                     "but instead created a raster of shape",
                     paste(dim(template_raster), collapse=","))
    stop(message)
  }
  return(result)
}

#update the simple polygon fx to remove unnecessary pieces and use SF to speedup process
load_simple_polygon <<- function(gaul_list, buffer, tolerance = 0.2,
                                 subset_only = F, makeplots = F, 
                                 raking = F, shapefile_version = 'current', testing=F) {
  
  require(sf)
  
  # Make a new simple_poly
  # count the vertices of a SpatialPolygons object, with one feature
  # TODO can be supplanted by mapview::npts() for sf
  vertices <- function(x) sum(sapply(x@polygons[[1]]@Polygons, function(y) nrow(y@coords)))
  
  if (testing) {
    message("Opening master shapefile...")
    master_shape_old <- readOGR(get_admin_shapefile(
      admin_level = 0, raking = raking,
      version = shapefile_version
    ))
    master_shape_old@data$ADM0_CODE <- as.numeric(as.character(master_shape_old@data$ADM0_CODE))
    subset_shape_old <- master_shape_old[master_shape_old@data$ADM0_CODE %in% gaul_list, ]
  }
  
  # ~~~~~~~~~~~~~~~~~
  # load data
  message("Opening master shapefile...")
  master_shape <- get_admin_shapefile(admin_level = 0, raking = raking, version = shapefile_version) %>% read_sf
  
  #TODO learn how to shift entire fx to sf paradigm
  subset_shape <- master_shape[master_shape$ADM0_CODE %in% gaul_list,]
  subset_shape <- subset_shape %>% as_Spatial #for now, just convert it back into SPDF
  
  if (subset_only) {
    
    list(subset_shape=subset_shape,spoly_spdf=NULL) %>% return
    
  } else {
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    message('Making a super-low-complexity map outline for INLA mesh creation')
    
    
    message('Full polygon vertices: ', vertices(subset_shape))
    #message('Full polygon vertices: ', npts(subset_shape))
    
    if (testing) vertices(subset_shape_old) == npts(subset_shape)
    
    # Merge everything together (where intersecting)
    af <- gUnaryUnion(subset_shape)
    #af <- st_union(subset_shape)
    
    # Initial simplification
    af_simple <- gSimplify(af, tol = tolerance, topologyPreserve = TRUE)
    #af_simple <- st_simplify(af, dTolerance = tolerance, preserveTopology = TRUE)
    
    # Remove tiny features
    # Get all sub polygons and their areas
    polys <- af_simple@polygons[[1]]@Polygons
    areas <- sapply(polys, function(x) x@area)
    
    # If there is more than one sub polygon, remove the ditzels (many single-country subsets are a single polygon,
    #   like Uganda, which would break these few lines)
    if(length(areas)>1) {
      # find top 5% by area
      big_idx <- which(areas > quantile(areas, 0.95))
      
      # convert back into a spatialPolygons object
      spoly <- SpatialPolygons(list(Polygons(polys[big_idx], ID = 1)))
    }
    if(length(areas)==1) {
      spoly <- af_simple
      big_idx <- 1
    }
    
    # Buffer slightly
    spoly <- gBuffer(spoly, width = buffer)
    
    # simplify again to reduce vertex count
    spoly <- gSimplify(spoly, tol = tolerance, topologyPreserve = TRUE)
    
    # Get list of original polygons
    polys2 <- af@polygons[[1]]@Polygons
    
    # Check if all are within the simple polygon
    check_if_in_spoly <- function(the_poly, compare_to, the_proj = projection(master_shape)) {
      the_poly <- SpatialPolygons(list(Polygons(list(the_poly), ID = 1)))
      projection(the_poly) <- the_proj
      projection(compare_to) <- the_proj
      
      if(suppressWarnings(gIsValid(the_poly)) == F) return(TRUE) #Ignore invalid polygons
      
      poly_intersect <- rgeos::gIntersection(the_poly, compare_to)
      
      if(is.null(poly_intersect)) {
        return(FALSE)
      } else {
        return(ifelse((raster::area(poly_intersect) == raster::area(the_poly)), TRUE, FALSE))
      }
    }
    
    over_list <- sapply(polys2, function(x) check_if_in_spoly(x, compare_to = spoly))
    
    if (all(over_list) == FALSE) {
      # Add back in polygons if missed by above procedure (e.g. islands dropped)
      big_idx <- unique(c(big_idx, which(over_list == F)))
      spoly <- SpatialPolygons(list(Polygons(polys[big_idx], ID = 1)))
      spoly <- gBuffer(spoly, width = buffer)
      spoly <- gSimplify(spoly, tol = tolerance, topologyPreserve = TRUE)
    }
    
    # Now check again with new spoly
    over_list <- sapply(polys2, function(x) check_if_in_spoly(x, compare_to = spoly))
    
    # If still not all enclosed, tolerance probably too high. Return warning
    if (all(over_list) == FALSE) {
      number_false = length(over_list[over_list == F])
      number_total = length(over_list)
      warning(paste0(number_false, " of ", number_total, " polygons are NOT enclosed within your simple polygon. \n",
                     "Adjust your buffer and tolerance values."))
    }
    
    # Return results
    #message('Simplified vertices: ', npts(spoly))
    message('Simplified vertices: ', vertices(spoly))
    
    # plot to check it encloses all of the important bits
    if(makeplots) plot(spoly)
    if(makeplots) plot(af, add = TRUE, border = grey(0.5))
    
    # turn into an SPDF
    spoly_spdf <- SpatialPolygonsDataFrame(spoly,
                                           data = data.frame(ID = 1),
                                           match.ID = FALSE)
    
    # add projection information
    projection(spoly_spdf) <- projection(master_shape)
    #projection(spoly_spdf) <- st_crs(master_shape)
    
    return(list(subset_shape=subset_shape,spoly_spdf=spoly_spdf))
  }
}
#***********************************************************************************************************************

# ---OPTIONS------------------------------------------------------------------------------------------------------------
## set arguments
shapefile_version           <- 'current'
modeling_shapefile_version  <- shapefile
reg                         <- 'BRA'

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
global_link_dir <- file.path('/home/j/WORK/11_geospatial/admin_shapefiles', shapefile_version) #TODO make official
tmp_link_dir <- '/share/scratch/tmp/fwlt'
#read in link_table
global_link_table <- file.path(global_link_dir, "lbd_full_link.rds") %>% readRDS %>%  as.data.table
global_link_table <- file.path(tmp_link_dir, "link_table_full_world.rds") %>% readRDS %>%  as.data.table
#read in id_raster
global_id_raster <- file.path(global_link_dir, 'lbd_full_id_raster.rds') %>% readRDS
global_id_raster <- file.path(tmp_link_dir, 'lbd_full_id_raster.rds') %>% readRDS
#***********************************************************************************************************************

# ---CALCULATE----------------------------------------------------------------------------------------------------------
reg <- 'CAN'
coastal_fix <- T
message('loading simple raster & populations')

## Load simple polygon template to model over
gaul_list <- get_adm0_codes(reg, shapefile_version = shapefile_version)
simple_polygon_list <- load_simple_polygon(
  gaul_list = gaul_list, buffer = 1, tolerance = 0.4,
  shapefile_version = shapefile_version
)
subset_shape <- simple_polygon_list[[1]]
simple_polygon <- simple_polygon_list[[2]]

## Load list of raster inputs (pop and simple)
if (coastal_fix) { 
  raster_list <- build_simple_raster_pop(subset_shape, link_table=global_link_table) #uses new simple_raster 
} else raster_list <- build_simple_raster_pop(subset_shape, link_table = NULL) #uses old rasterize

simple_raster <- raster_list[["simple_raster"]]
pop_raster <- raster_list[["pop_raster"]]
pixel_id <- seegSDM:::notMissingIdx(simple_raster)

message('loading links')
#####################################################################
# load the cell id to admin units link
link_table <- get_link_table(simple_raster, shapefile_version = shapefile_version, 
                             global_link=global_link_table, global_ids=global_id_raster)
link_table$link_table[, table(NAME_0)]
#*********************************************************************************************************************** 