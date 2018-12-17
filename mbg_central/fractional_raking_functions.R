#################### FRACTIONAL RAKING #############################
#
# In using 5x5 pixels, some pixels are split between several admin
# units. Fractional raking is a way of raking counts which takes
# this into consideration. For any pixel that is split between
# admin units, the fractional area of that admin unit within the
# pixel is multiplied by the count so that counts are properly
# attributed to their respective admin units.
#
# The key to fractional raking is a [link table] which has a row
# for each pixel-admin unit, and includes the gaul codes for admins
# 0-2, as well as the fractional area of the admin unit in that pixel
# (a pixel split between two admin 2 units would have two rows in the
# link table). There is a global link table generated from the stage 1
# and stage 2 simple raster, where every pixel gets a unique id.
# Practically, when using a link table, the global link table and raster
# are subsetted based on the simple raster of the region being raked
# using get_link_table()
#
# Build_link_polygon() and build_link_table() are functions used to
# create the link table, which will only need to be used when
# there is a change in the shapefile used to generate simple rasters.
#
# Many of the functions require the parameter overs, which is a vector
# of the column names of the draws, typically named V1 to Vndraws
# when the cell pred is converted to a data.frame or data.table.
#
# The general workflow of fractional raking goes like this -
# #get link table and pixel ids
# link_table_output <- get_link_table(simple_raster)
# link <- link_table_output[["link_table"]]
# pixel_id <- link_table_output[["pixel_ids"]]
#
# #bind on pixel ids to cell pred
# pixel_id <- rep(pixel_id, times=length(year_list))
# cell_pred <- cbind(cell_pred, pixel_id)
#
# #get overs parameter
# ndraws <- ncol(cell_pred)
# overs <- paste0('V',1:ndraws)
#
# #calculate raking factors (see function documentation for gbd parameter)
# fractional_rf <- get_fractional_rf(deaths_cell, link, gbd, "deaths", reg, overs, shapefile_version)
#
# #apply raking factors to cell pred
# raked_linked_cell_pred <- apply_fractional_rf(cell_pred, link, fractional_rf, overs)
#
# #deduplicate linked cell pred
# deduped_cell_pred <- dedupe_linked_cell_pred(raked_linked_cell_pred, overs)
#
# The raked_linked_cell_pred and deduped_cell_pred are the two main objects
# at the end of fractional raking. The raked_linked_cell_pred has raked
# draws that are linked to admin 0-2, and can be aggregated as is using the
# admin codes. The deduped_cell_pred is a matrix with the same dimensions
# as the original cell pred and can be used to make rasters of the data.
# See below for example of aggregation and rasterization.
#
# #make mean, upper and lower raster bricks
# raster_bricks_list <- list()
# for(func in c("mean", "upper", "lower")) {
#   raster_bricks_list[[func]] <- cell_pred_to_raster_brick(deduped_cell_pred, simple_raster, year_list, func)
# }
#
# #aggregate deaths to Adm 0-2
# aggregate_death_list <- aggregate_deaths(raked_linked_cell_pred, overs)
#
# For additional questions ask Michael Collison
#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fractional Raking Functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Calculate Fractional Raking Factors
#'
#' @Description: Calculate raking factors considering cases where a pixel is in
#' multiple admin units for deaths or cases
#'
#' @param cell_pred a cell pred with deaths/cases in data.table format with a pixel_id column from `get_link_table()`
#' @param link link table gotten from `get_link_table()`
#' @param gbd data.table with a columns - `gaul`, `year_id`, and `gbd_field (see below)`
#' with estimates for the indicator being raked to for each year-admin0
#' @param gbd_field the column name in the gbd data.table with the estimates for the indicator
#' @param region the region used to make the simple raster
#' @param overs a vector of the column names in the cell pred corresponding to draws
#' (typically V1:Vndraws)
#' @param shapefile_version string indicating which shapefile version to match ADM0 codes against
#' 
#' @return a data.table with raking factor by adm0 gaul code and year
#'
#' @import data.table
#' @import dplyr
#' 
get_fractional_rf <- function(cell_pred, link, gbd, gbd_field, region, overs, shapefile_version) {
  #merge link table and cell pred on pixel id
  linked_cell_pred <- merge(link, cell_pred, by.x = "ID", by.y = "pixel_id", all.y = T, allow.cartesian=T)

  #multiply death draws by area fractions and aggregate draws to ADM0 and year
  linked_cell_pred <- linked_cell_pred[, lapply(overs, function(x) sum(get(x) * area_fraction, na.rm = T)), by = c('year','ADM0_CODE')]

  #get mean of draws
  linked_cell_pred[,mbg_rf := rowMeans(linked_cell_pred[,overs, with=F])]

  #merge with gbd and calculate population raking factor
  rf_df <- merge(linked_cell_pred, gbd, by.x = c("ADM0_CODE", "year"), by.y = c("gaul", "year_id"), all.x=T)
  rf_df <- rf_df[, rf := get(gbd_field) / mbg_rf]

  #drop unecessary columns
  rf <- rf_df[,c("ADM0_CODE", "year", "rf")]

  #set raking factors for countries not in region to 1 to avoid overraking in countries
  #partially included in the simple raster. Not doing this causes the values of border
  #pixels to be much higher than they should be.
  rf[!(ADM0_CODE %in% get_adm0_codes(region, shapefile_version = shapefile_version)), rf := 1]
  return(rf)
}


#' Apply Fractional Raking Factors
#'
#' @Description: Apply raking factors considering cases where a pixel is in
#' multiple admin units for deaths or cases
#'
#' @param cell_pred a cell pred with deaths/cases
#' @param link link table generated by `build_link_table()`
#' @param fractional_rf a data.table generated by `get_fractional_rf`
#' @param overs a vector of the column names in the cell pred corresponding to draws
#' (typically V1:Vndraws)
#'
#' @return a linked cell pred with the rf applied
#'
#' @import data.table
#'
apply_fractional_rf <- function(cell_pred, link, fractional_rf, overs) {
  #generate id for rows in cell pred (merges below mess up ordering)
  cell_pred[, cell_pred_id := .I]
  #merge cell pred and link file
  linked_cell_pred <- merge(link, cell_pred, by.x = "ID", by.y = "pixel_id", all.y = T, allow.cartesian=T)
  #merge linked cell pred with raking factors
  linked_cell_pred <- merge(linked_cell_pred, fractional_rf, by.x = c("ADM0_CODE", "year"), by.y = c("ADM0_CODE", "year"), all.x =T)
  #converting from "units" type to double, truncates decimal otherwise
  linked_cell_pred$area_fraction <- as.double(linked_cell_pred$area_fraction)
  #calculate deaths on all draws
  raked_cell_pred <- linked_cell_pred[, (overs) := lapply(overs, function(x) get(x)* rf * area_fraction)]

  #sort on cell_pred_id
  setorder(raked_cell_pred, cell_pred_id)

  return(raked_cell_pred)
}


#' Deduplicate linked cell preds
#'
#' @Description: Collapse rows of cell preds with multiple admin units in one pixel back into
#' single rows
#'
#' @param linked_cell_pred a cell pred merged to a link table generated by `get_link_table()`
#' @param overs a vector of the column names in the cell pred corresponding to draws
#' (typically V1:Vndraws)
#'
#' @return a cell pred object in original matrix form which corresponds to simple raster
#'
#' @import data.table
#'
dedupe_linked_cell_pred <- function(linked_cell_pred, overs) {

  #sum deaths for each duplicated pixel
  dedup_dt <- linked_cell_pred[, lapply(.SD, sum), by=cell_pred_id, .SDcols=overs]

  #reorder on cell_pred_id to match original order
  dedup_dt$cell_pred_id <- as.numeric(as.character(dedup_dt$cell_pred_id))
  setorder(dedup_dt, cell_pred_id)

  #drop columns and convert back into matrix
  deduped_linked_cell_pred <- dedup_dt[,overs, with = FALSE]
  deduped_linked_cell_pred <- as.matrix(deduped_linked_cell_pred)
  colnames(deduped_linked_cell_pred) <- NULL
  return(deduped_linked_cell_pred)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Post-Raking Processing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Aggregate raked linked cell pred
#'
#' @Description: rake linked cell pred to admins0-2, maintaining draws
#'
#' @param raked_linked_cell_pred a raked and linked cell pred object
#' @param year_list list of years in cell_pred
#'
#' @return a list with 3 data.tables for admins0-2 with draws intact
#'
aggregate_counts <- function(raked_linked_cell_pred, overs) {

  admin_2 <- raked_linked_cell_pred[, lapply(overs, function(x) sum(get(x), na.rm = T)), by = c('year', 'ADM2_CODE', 'ADM0_CODE')]
  admin_1 = raked_linked_cell_pred[, lapply(overs, function(x) sum(get(x), na.rm = T)), by = c('year','ADM1_CODE', 'ADM0_CODE')]
  admin_0 = raked_linked_cell_pred[, lapply(overs, function(x) sum(get(x), na.rm = T)), by = c('year','ADM0_CODE')]
  return(list("admin_0" = admin_0, "admin_1" = admin_1,"admin_2" = admin_2))
}


#' Convert cell pred into raster brick
#'
#' @Description: Insert cell pred object into a series of raster layers for each year
#' then combine into rasterBrick
#'
#' @param cell_pred a cell pred object in matrix format
#' @param simple_raster the simple raster corresponding to the cell_pred_object
#' @param year_list list of years in cell_pred
#' @param func function to summarize cell pred. `mean`, `upper`, and `lower` are acceptable
#'
#' @return a rasterBrick with a layer for each year in the cell_pred
#'
#' @import matrixStats
#'
cell_pred_to_raster_brick <- function(cell_pred, simple_raster, year_list, func) {
  require(matrixStats)

  raster_list <- vector("list", length = length(year_list))

  #apply summary function
  if(func == "mean") {
    message("summarizing draws to mean")
    death_vec <- rowMeans(cell_pred)
  } else if (func == "upper") {
    message("summarizing draws to upper quantile - this can be slow for large cell_preds")
    death_vec <- matrixStats::rowQuantiles(cell_pred, probs = 97.5 / 100)
  } else if (func == "lower") {
    message("summarizing draws to lower quantile - this can be slow for large cell_preds")
    death_vec <- matrixStats::rowQuantiles(cell_pred, probs = 2.5 / 100)
  } else {
    stop("Not a valid function, please choose 'mean', 'upper', or 'lower'.")
  }

  #split years into separate vectors in a list
  death_vec_list <- split(death_vec, cut(seq_along(death_vec), length(year_list), labels = FALSE))
  #get raster pixels with data
  pixel_id <- which(!is.na(getValues(simple_raster)))
  message("Building raster brick \n")
  #loop through year vectors and insert into rasters
  for(i in 1:length(death_vec_list)){
    death_ras <- simple_raster
    raster_list[[i]] <- insertRaster(death_ras, cbind(death_vec_list[[i]]))
  }
  #set years as raster layer names
  names(raster_list) <- year_list

  #convert to rasterBrick
  death_raster <- brick(raster_list)
  return(death_raster)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Link Table Functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Subset global link table to regional table
#'
#' @Description: Reads in the global link table and id raster and subsets using
#' a regional simple raster.
#'
#' @param simple_raster Simple raster used for modelling region
#'
#' @return list of 2 objects:
#'    - a link table
#'    - a vector of pixel ids used to link one year in a cell pred to the link table.
#'
#' @import data.table
#' @import raster
#'
get_link_table <- function(simple_raster, shapefile_version) {
  #read in link_table
  global_link <- data.table(readRDS(sprintf("/snfs1/WORK/11_geospatial/admin_shapefiles/%s/lbd_standard_link.rds", shapefile_version)))
  #read in id_raster
  global_raster <- readRDS(sprintf("/snfs1/WORK/11_geospatial/admin_shapefiles/%s/lbd_standard_id_raster.rds", shapefile_version))
  #crop id_raster to simple_raster
  cropped_raster <- crop(global_raster, simple_raster)
  #mask id_raster with simple_raster
  masked_raster <- mask(cropped_raster, simple_raster)
  #extract id_raster
  pixel_ids <- raster::extract(masked_raster, extent(masked_raster), na.rm = T)
  pixel_ids <- pixel_ids[!is.na(pixel_ids)]
  #subset link table by pixel ids from extract
  link_table <- global_link[ID %in% pixel_ids,]
  #make sure list of pixel ids is same number of nonna cells in simple raster
  if(length(pixel_ids) != length(which(!is.na(getValues(simple_raster))))) {
    stop("The number of pixel_ids does not match the number of non-na values in simple raster. \nPart of the simple raster may be outside the extent of the global id raster.")
  }
  #return subsetted link table and list of pixel ids
  return(list("link_table" = link_table, "pixel_ids" = pixel_ids))
}


#' Build Link Polygon
#'
#' @Description: Builds a polygon where there is a layer for each pixel in a raster,
#' with an ID field referring to the id of the pixel in the raster
#'
#' @param region character string of region to be pulled from `get_adm0_codes()`
#' @param simple_raster default `NULL`. provides option to pass in simple raster rather
#' than making it using a region name to save time.
#' @param shapefile_version string to identify version of shapefile to build simple_raster
#' 
#' @return returns an sf polygon
#'
#' @example idpoly <- build_link_polygon(region)
#'
#' @import raster
#' @import sp
#' @import rgeos
#' @import spex
#' @import sf
#' 
build_link_polygon <- function(region, simple_raster = NULL, shapefile_version = 'current') {
  require(raster)
  require(sp)
  require(rgeos)
  require(spex)
  require(sf)

  #load simple raster (pass in as parameter for speed up)
  if(is.null(simple_raster)) {
    simple_polygon <- load_simple_polygon(gaul_list = get_adm0_codes(region, shapefile_version = shapefile_version),
                                          buffer = 0.4,
                                          shapefile_version = shapefile_version)
    subset_shape   <- simple_polygon[['subset_shape']]
    simple_polygon <- simple_polygon[['spoly_spdf']]

    message('Loading simple raster')
    raster_list    <- build_simple_raster_pop(subset_shape) #,u5m=TRUE)
    simple_raster  <- raster_list[['simple_raster']]
    pop_raster     <- raster_list[['pop_raster']]
  }

  #make a copy of simple raster and set all values to 0
  new_ras <- simple_raster
  values(new_ras) <- 0
  #give each pixel a unique id
  new_ras[1:length(simple_raster)] <- 1:length(simple_raster)

  #convert raster to sf polygon and give it an ID field
  message("converting ID raster to polygon")
  idpoly <- spex::polygonize(new_ras)
  idpoly$ID <- 1:nrow(idpoly)

  return(idpoly)
}


#' Build Link Table
#'
#' @Description: Builds a link table where each row represents an admin2-pixel, and includes
#' the admin0-2 levels, the id of the pixel (based on simple raster), and the percent of the
#' pixel covered by a country. Used for fractional raking where a pixel is in multiple
#' administrative units.
#'
#' @param idpoly an sf polygon generated from `build_link_polygon()``
#' @param region the region used to generate the simple raster
#' @param cores number of cores to run mclapply over
#' @param shapefile_version version of admin file to base link table on
#' 
#' @return returns the link data.table described above
#'
#' @example link <- build_link_table(idpoly, region, cores)
#'
#' @import lwgeom
#' @import parallel
#' @import data.table
#' @import dplyr
#' @import sf
#'
build_link_table <- function(idpoly, region, cores, shapefile_version) {
  require(lwgeom)
  require(parallel)
  require(data.table)
  gauls <- get_adm0_codes(region, shapefile_version = shapefile_version)
  
  admin_level <- 2
  #read in admin2 shapefile
  ad <- sf::st_read(get_admin_shapefile(admin_level = admin_level, version = shapefile_version))
  if(shapefile_version == 'gaul'){
    ##assign admin 2s properly for africa
    ## TODO: can we remove these corrections?
    ad[[paste0('ADM', admin_level,'_CODE')]][ad[[paste0('ADM', admin_level,'_CODE')]]==61013]=133
    ad[[paste0('ADM', admin_level,'_CODE')]][ad[[paste0('ADM', admin_level,'_CODE')]]==40760]=40765
    ad[[paste0('ADM', admin_level,'_CODE')]][ad[[paste0('ADM', admin_level,'_CODE')]]==40762]=145
    ad[[paste0('ADM', admin_level,'_CODE')]][ad[[paste0('ADM', admin_level,'_CODE')]]==102  ]=6
    message("Generating link table - recommend running this overnight if possible")
  }

  #get list of pixels in each polygon - suppress messages because it generates a lot of them
  # Set multithreading to serial for `mclapply()`:
  set_serial_threads()
  admin_cells <- suppressMessages(mclapply(1:nrow(ad), function(x)
    get_cells(ad[x,],
              idpoly), mc.cores = cores, mc.preschedule = F))
  # Return to multithreading (if any):
  set_original_threads()
  admin_cells_dat <- rbindlist(admin_cells, fill = T)
  #calculate fractional area
  admin_cells_dat[, area_fraction := obs_area/start_area]
  #merge on additional metadata
  admin_cells_dat <- merge(admin_cells_dat, data.frame(ad), by = 'ADM2_CODE')
  admin_cells_dat[, geometry:=NULL]

  #drop extra rows where there are no pixels in that polygon
  admin_cells_dat <- admin_cells_dat[!is.na(ID),]

  #apply area_fraction fix
  link <- fix_link(admin_cells_dat)

  return(link)
}


#' Get List of Pixels in each admin unit
#'
#' @Description: Used internally in `build_link_table()`.
#'
#' @param admin_unit GAUL admin 2 shapefile subset to a single admin unit
#' @param cell_map an sf polygon generated by `build_link_polygon()`
#'
#' @return a list with each entry referring to a data.table listing the ids of the pixels
#' within an admin unit
#'
#' @example   admin_cells <- suppressMessages(mclapply(1:nrow(ad), function(x)
#' get_cells(ad[x,], idpoly), mc.cores = cores, mc.preschedule = F))
#'
get_cells = function(admin_unit, cell_map){
  admin_level <- 2
  isect <- sf::st_intersection(admin_unit, cell_map)

  if(nrow(isect)>0){

    isect <- isect[,c(paste0('ADM',admin_level,'_CODE'), 'ID')]
    isect$obs_area <- sf::st_area(isect)
    isect$start_area <- sf::st_area(cell_map[cell_map$ID %in% isect$ID,])

    #get rid of the geometry
    isect$geometry = NULL

    setDT(isect)
  }else{

    isect <- as.data.frame(admin_unit[,paste0('ADM',admin_level,'_CODE')])[1]
    isect <- setDT(isect)
  }

  return(isect)
}


#' Fix area fractions in link table
#'
#' @Description: Scales area fractions for pixels with water. Pixels with 1 admin unit and
#' water have the area fraction set to 1, as all values will occur on the land. Pixels
#' containing multiple admin units and water are scaled proportionally. Internal function
#' used in `build_link_table()`
#'
#' @param link link table
#'
#' @return a link table with adjusted area fractions
#'
fix_link <- function(link) {
  #figure out which pixels have multiple rows
  link_fix <- link %>%
    group_by(ID) %>%
    summarise(total_area = sum(area_fraction),
              n = n()) %>%
    data.table()

  #merge link, link_fix and cast some variables to numeric
  link_fix2 <- merge(link, link_fix, by = "ID")
  link_fix2[, area_fraction := as.numeric(area_fraction)]
  link_fix2[, total_area := as.numeric(total_area)]
  #set area to 1 for all pixels in a single admin unit and water
  link_fix2[n == 1 & area_fraction < 1, area_fraction := 1]
  #recalculate area for pixels in multiple admin units and water proportionally
  link_fix2[n > 1 & total_area < 1, area_fraction := area_fraction * (1 / (1 - (1 - total_area)))]
  return(link_fix2)
}
