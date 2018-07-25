# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 07/19/2018
# Purpose: Define helper functions for the HAP collapse script
# source("/homes/jfrostad/_code/hap/extract/functions/resample_fx.R", echo=T)
#***********************************************************************************************************************

# ----Get Pop Raster----------------------------------------------------------------------------------------------------
#helper function to pull the pop raster for a given year
getPop <- function(this.year, 
                   path='/snfs1/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif') {
  
  if (this.year <= 2000) {
    band.num <- 1
  } else if (this.year > 2000 & this.year <= 2005) {
    band.num <- 2
  } else if (this.year > 2005 & this.year <= 2010) {
    band.num <- 3
  } else band.num <- 4
  
  raster(path, band=band.num) %>% 
    return
  
}
#***********************************************************************************************************************

# ----Resampling--------------------------------------------------------------------------------------------------------
#function to resample polygons into points based on population

polydat <- cooking[polygon==T]
shp <- unique(polydat$shapefile)[2]
subset <- polydat[shapefile==shp]

shape_master <- read_sf(paste0('/home/j//WORK/11_geospatial/05_survey shapefile library/Shapefile directory/',shp,'.shp'))
shape_master <- shapefile(paste0('/home/j//WORK/11_geospatial/05_survey shapefile library/Shapefile directory/',shp,'.shp'))

levels <- c('bin_cooking_fuel_mapped', 'ord_cooking_fuel_mapped')
pid <- 'bin_cooking_fuel_mapped'

#TODO simplify
subset_loc <- subset[,c(setdiff(names(subset),setdiff(levels,pid))), with=F]

loc <- unique(subset$location_code)[2]

shape <- shape_master[shape_master$GAUL_CODE==loc,]
subset_loc2 <- subset_loc[location_code==loc]

q <- nrow(subset_loc2)[1]

subset_loc3 <- subset_loc2[q]

  pop_raster <- getPop(subset_loc3$year_start) %>% #pull the appropriate raster based on year
    crop(x=., y=shape) %>% #crop and mask it to working area
    mask(., shape)

  if (length(unique(pop_raster)) < 1) { #TODO ask ani why this check is necessary, very slow
    samp_pts <- gCentroid(shape)@coords  %>% as.data.table
    samp_pts[, weight := 1]
  } else {
    samp_pts <- getPoints(shape = shape, raster = pop_raster, n = 0.01, perpixel = T) %>% as.data.table
  }
  
  setnames(samp_pts, 
           c('x', 'y', 'weights'), 
           c("long", "lat", "weight"))
      
  samp_pts[, shapefile := shp]
  
  
  setkey(samp_pts, shapefile)
      subset_loc3 <- left_join(samp_pts, subset_loc3, by = 'shapefile')
      subset_loc3$point <- 0
      
      generated_pts[[length(generated_pts) + 1]] <- subset_loc3

 
