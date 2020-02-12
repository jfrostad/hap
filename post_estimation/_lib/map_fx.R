####################################################################################################
## Description:   Make maps of estimates in Africa for specified years and year pairs.
##
## Inputs:        mean rasters
##                adm0 and adm1 estimates
##                country outlines, lakes, and population mask
##
## Output:        PDF of maps (/share/geospatial/mbg/[indicator_group]/[indicator]/output/
##                  [run_date]/results_maps/[indicator]_raked_mean.pdf').
####################################################################################################
## load_map_annotations ----------------------------------------------------------------------------

load_map_annotations <- function(use.sf=F) {

  if(use.sf) {
    
    ## Base shapefile (country outlines)
    message('->loading country borders')
    stage1 <- st_read('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/shps_by_stage/stage1_ad0_gadm.shp')
    stage2 <- st_read('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/shps_by_stage/stage2_ad0_gadm.shp')
    adm0 <- rbind(stage1, stage2)
    
    ## Lakes
    message('-->loading lake mask')
    lakes <- raster('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/global_files/global_lakes.tif') %>% 
      as(., 'SpatialPolygonsDataFrame') %>% 
    st_as_sf
    
    ## Population mask
    message('--->loading population mask')
    mask <- raster('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/global_files/global_mask_master.tif') %>% 
      as(., 'SpatialPolygonsDataFrame') %>% 
      st_as_sf
    
    ## Stage 3 mask
    message("---->loading stage 3 mask")
    stage3 <- st_read('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/global_files/stage_3_mask.shp')
    
  } else {
  
    ## Base shapefile (country outlines)
    message('->loading country borders')
    stage1 <- shapefile('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/shps_by_stage/stage1_ad0_gadm.shp')
    stage2 <- shapefile('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/shps_by_stage/stage2_ad0_gadm.shp')
    adm0 <- bind(stage1, stage2) %>% fortify
  
    ## Lakes
    message('-->loading lake mask')
    lakes <- raster('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/global_files/global_lakes.tif') %>% 
      rasterToPoints %>% 
      as.data.table %>% 
      setnames(., c("long", 'lat', 'lakes'))
  
    ## Population mask
    message('--->loading population mask')
    mask <- raster('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/global_files/global_mask_master.tif') %>% 
    rasterToPoints %>% 
      as.data.table %>% 
      setnames(., c("long", 'lat', 'mask'))
    
    ## Stage 3 mask
    message("---->loading stage 3 mask")
    stage3 <- shapefile('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/global_files/stage_3_mask.shp') %>% fortify
  
  }

  return(list(adm0 = adm0, lakes = lakes, mask = mask, stage3 = stage3))

}

## load_map_results --------------------------------------------------------------------------------

load_map_results <- function(indicator, indicator_group, run_date, raked, 
                             start_year, end_year, single_year,
                             geo_levels = c("raster", "admin1", "admin2"),
                             custom_path=NULL, subvars=NULL,
                             use.sf=T,
                             cores=1, 
                             debug=F) {
  
  if (debug) browser()
  
  years <- paste(start_year, end_year, sep="_")
  
  if (missing(single_year)) year_list <- start_year:end_year 
  else year_list <- single_year

  ## Set the input directory
  maps_path <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date)
  
  ## Helper function to prepare the admin level results
  prep_admin_x <- function(x) {
    
    #setup the column name with requested division
    code_var <- paste0('ADM', x, '_CODE')
    
    #read from standard location if not provided with custom path
    if(custom_path %>% is.null) {
      pred <- paste0(maps_path, "/pred_derivatives/admin_summaries/", indicator, "_admin_", x, 
                     ifelse(raked, "_raked", "_unraked"), 
                     "_summary.csv") %>% fread
    } else pred <- custom_path[[ paste0('admin', x)]] %>% fread
    
    message('-> admin', x, ' found and fread')
    
    
    if (subvars %>% is.null) pred <- pred[year %in% year_list] 
    else {
      pred <- pred[year %in% year_list] %>% 
        .[, c('ADM0_CODE', code_var, 'year', subvars), with = F] #subset vars if requested
    }
    
    if (use.sf) {
      
      shp <- get_admin_shapefile(x) %>% 
        st_read %>% 
        filter(get(code_var) %in% pred[,  get(code_var)]) %>% 
        merge(., pred, by=c('ADM0_CODE', code_var), allow.cartesian=T)
      
      message('--> admin', x, ' results merged to sf')
      
    } else {
      
      #TODO update or deprecate, currently broken
      
      # admin2 <- shapefile(get_admin_shapefile(2))
      # admin2 <- admin2[admin2@data$ADM2_CODE %in% pred$ADM2_CODE,]
      # admin2 <- SpatialPolygonsDataFrame(gSimplify(admin2, tol = 0.1, topologyPreserve = T), data = admin2@data)
      # for (i in 1:length(admin2)) admin2@polygons[[i]]@ID <- as.character(admin2@data[i, "ADM2_CODE"])
      # admin2 <- data.table(fortify(admin2))
      # admin2[, ADM2_CODE := as.numeric(id)]
      # admin2 <- merge(admin2, pred, by="ADM2_CODE", allow.cartesian=T)
      # setkey(admin2, id, group, order, year)
      # 
      # message('--> admin2 results merged to shapefile')
      
    }
    
    return(shp)
    
  }

  ## raster estimates
  if ("raster" %in% geo_levels) {
    message('loading raster data')
    if(missing(custom_path)) {
      raster <- paste0(maps_path, "/", indicator, "_", 
                       ifelse(type == "cirange", "range", type),
                       "_", 
                       ifelse(raked, "raked_", ""), 
                       years, ".tif") %>% brick
    } else raster <- brick(custom_path$raster)
    
    message('-> raster found and bricked')

    raster <- mclapply(year_list, 
                       function(y) {

                         message('--> sending to points (year=', y, ')')
                         df <- rasterToPoints(raster[[y - 1999]]) %>% data.table
                         setnames(df, c("long", 'lat', 'outcome'))
                         df[, year := y]
                         
                         return(df)
                         
                       },
                       mc.cores=cores) %>% 
      rbindlist %>% 
      setkey(., long, lat, year)
      
    message('--> converted to dt and keyed')
    
  }

  ## admin1 estimates and shape file
  if ("admin1" %in% geo_levels) admin1 <- prep_admin_x(1)


  ## admin2 estimates and shape file
  if ("admin2" %in% geo_levels) admin2 <- prep_admin_x(2)
    
  ## combine and return all estimates
  mget(geo_levels) %>% return

}

## calc_diff_map -----------------------------------------------------------------------------------

calc_diff_map <- function(pred, diff_years) {
  diff <- lapply(names(pred), function(g) {
    rbindlist(lapply(diff_years, function(y) {
      temp <- pred[[g]][year %in% y, ]
      temp <- temp[, list(outcome = outcome[year == y[2]] - outcome[year == y[1]]), by=setdiff(names(temp), c("outcome", "year"))]
      temp[, years := paste(y, collapse="-")]
      temp
    }))
  })
  names(diff) <- names(pred)
  return(diff)
}

## plot_map ----------------------------------------------------------------------------------------
plot_map <- function(map_data, annotations, title, limits, this_var='outcome',
                     legend_colors, legend_color_values, legend_breaks, legend_labels, legend_title, 
                     custom_scale=F, flip_legend=F,
                     pop_mask=T, lake_mask=T, borders=T, stage3_mask=T,
                     zoom,
                     debug=F) {
  
  if (debug) browser()
  
  message('checking arguments')
  #set function arguments based on argument classes
  map_sf <- 'sf' %in% class(map_data)
  annotate_sf <- 'sf' %in% sapply(annotations, class)
  
  message('building limits/scale')
  
  ## Enforce limits & define plot_var for simplicity
  #TODO set in to enforce lower limit as well?
  map_data$plot_var <- pmax(limits[1], pmin(limits[2], as.data.frame(map_data)[, this_var])) 

  if (!custom_scale) {

    start_range <- range(map_data$plot_var, na.rm = T)

    ## Create breaks
    breaks <- pretty(limits, 5)
    if (limits[1] < 0 & limits[2] > 0) breaks <- sort(unique(c(0, breaks)))
  
    ## Create labels
    labels <- format(breaks, nsmall = 2)
    if (min(limits) >= 0) divider <- "-" else divider <- " to "
    if (start_range[1] < limits[1]) {
      labels[1] <- paste0(format(floor(100*start_range[1])/100, nsmall=2), divider, labels[1])
    }
    if (start_range[2] > limits[2]) {
      labels[length(labels)] <- paste0(labels[length(labels)], divider, format(ceiling(100*start_range[2])/100, nsmall=2))
    }
    
  } else {breaks <- legend_breaks; labels <- legend_labels}
  
  message('plotting canvas')
  
  ## Crop the annotations for speed
  if (!missing(zoom) & annotate_sf) { 
    message('cropping canvas to zoom')
    annotations <- lapply(annotations, st_crop, xmin=zoom$x1, xmax=zoom$x2, ymin=zoom$y1, ymax=zoom$y2)
  }
  #TODO add non.sf option
  
  ## Plot the base map (this is what shows in places with no estimates and no mask)
  if (annotate_sf) canvas <- ggplot() + geom_sf(data = annotations$adm0, lwd=0.1, color = 'black', fill = 'gray90')
  else {
    canvas <- ggplot() +
      geom_polygon(data = annotations$adm0, aes(x = long, y = lat, group = group), color = 'gray90', fill = 'gray90')
    
    if (!missing(zoom)) {
      canvas <- canvas +
        xlim(zoom$x1, zoom$x2)  +
        ylim(zoom$y1, zoom$y2)
    }
    
  }

  message('plotting outcome')

  ## Plot predictions
  if (map_sf) {
    gg <- canvas + geom_sf(data = map_data, aes(fill = plot_var), lwd=0) + coord_sf(datum = NA)
  } else if ("group" %in% names(map_data)) {
    gg <- canvas + geom_polygon(data = map_data, aes(fill = plot_var, y = lat, x = long, group = group)) + 
      coord_equal(ratio = 1) 
  } else {
    gg <- canvas + geom_raster(data = map_data, aes(fill = plot_var, y = lat, x = long)) + 
      coord_equal(ratio = 1)
  }

  message('plotting annotations')
  
  ## Plot mask, lakes, and adm boarders using SF
  if (annotate_sf & pop_mask) gg <- gg + geom_sf(data = annotations$mask, lwd=0, color = 'gray70', fill = 'gray70')
  if (annotate_sf & lake_mask) gg <- gg + geom_sf(data = annotations$lakes, lwd=0, color = 'gray70', fill = 'lightblue')
  if (annotate_sf & borders) gg <- gg + geom_sf(data = annotations$adm0, lwd=0.1, color = 'black', fill=NA)
  if (annotate_sf & stage3_mask) gg <- gg + geom_sf(data = annotations$stage3, color = 'gray70')

  ## Plot mask, lakes, and adm boarders using polygons
  if (!annotate_sf & pop_mask) gg <- gg + annotate(geom = 'raster', x = annotations$mask$long, y = annotations$mask$lat, fill = 'gray70')
  if (!annotate_sf & lake_mask) gg <- gg + annotate(geom = 'raster', x = annotations$lakes$long, y = annotations$lakes$lat, fill = 'lightblue')
  if (!annotate_sf & borders) gg <- gg + geom_path(data = annotations$adm0, aes(x = long, y = lat, group = group), color = 'black', size = 0.2)
  if (!annotate_sf & stage3_mask) gg <- gg + geom_path(data = annotations$stage3, aes(x = long, y = lat, group = group), color = 'gray70', size = 0.2)

  
  message('defining aesthetics')
  
  ## Scales
  gg <- gg +
    scale_fill_gradientn(colors = legend_colors, values = legend_color_values,
                         limits = range(breaks), breaks = breaks, labels = labels, name = legend_title)

  ## Labels & aesthetics
  gg <- gg +
    labs(x="", y="", title=title) +
    theme_classic() +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          legend.position = c(ifelse(flip_legend, .8, 0), 0), legend.justification = c(0, 0),
          #legend.text=element_text(size=10),
          plot.title = element_text(hjust=0.5), plot.margin=unit(c(0, 0, 0, 0), "in")) +
    guides(fill = guide_colorbar(barwidth = 0.5, barheight = 7))

  return(gg)
  
}
