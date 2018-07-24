# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 07/19/2018
# Purpose: Define helper functions for the HAP collapse script
# source("/homes/jfrostad/_code/hap/extract/functions/resample_fx.R", echo=T)
#***********************************************************************************************************************

# ----Resampling--------------------------------------------------------------------------------------------------------
#function to resample polygons into points based on population
if (indic == 'water') {
  levels <- c('piped','imp','unimp','surface')
  polydat <- read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/water_2018_03_08.feather')
} else {
  levels <- c('imp','unimp','od')
  polydat <- read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/sani_2018_03_08.feather')
}

polydat <- cooking[polygon==T]
shp <- unique(polydat$shapefile)[1]
subset <- polydat[shapefile==shp]

polydat <- filter(polydat, is.na(lat) & !is.na(shapefile) & !is.na(location_code))
subset <- polydat[which(polydat$shapefile == shp),]

shape_master <- shapefile(paste0('/home/j//WORK/11_geospatial/05_survey shapefile library/Shapefile directory/',shp,'.shp'))

for (pid in levels) {
  setwd('/home/j/WORK/11_geospatial/wash/data/resamp/')
  generated_pts <- list()
  
  subset_loc <- subset[,setdiff(names(subset),setdiff(levels,pid))]
  
  for (loc in unique(subset$location_code)) {
    shape <- shape_master[shape_master$GAUL_CODE == loc,]
    subset_loc2 <- filter(subset_loc, location_code == loc)
    
    for (q in 1:nrow(subset_loc2)) {
      
      subset_loc3 <- subset_loc2[q,]
      
      year <- subset_loc3$year_start
      if (year <= 2000) {
        pop_raster <- raster('/snfs1/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 1)
      } else {
        if (year > 2000 & year <= 2005) {
          pop_raster <- raster('/snfs1/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 2)
        } else {
          if (year > 2005 & year <= 2010) {
            pop_raster <- raster('/snfs1/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 3)
          } else {
            pop_raster <- raster('/snfs1/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 4)
          }
        } 
      }
      
      raster_crop <- mask(crop(x = pop_raster, y = shape), shape)
      if (length(unique(raster_crop)) < 1) {
        samp_pts <- gCentroid(shape)@coords
        samp_pts <- as.data.frame(samp_pts)
        samp_pts$weight <- 1
        
      } else {
        samp_pts <- getPoints(shape = shape, raster = raster_crop, n = 0.01, perpixel = T)  
        samp_pts <- as.data.frame(samp_pts)
      }
      
      names(samp_pts) <- c("long", "lat","weight")
      samp_pts$shapefile <- shp
      
      subset_loc3 <- left_join(samp_pts, subset_loc3, by = 'shapefile')
      subset_loc3$point <- 0
      
      generated_pts[[length(generated_pts) + 1]] <- subset_loc3
    }
    
  }
}  
