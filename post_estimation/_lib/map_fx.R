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

load_map_annotations <- function() {

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

  return(list(adm0 = adm0, lakes = lakes, mask = mask, stage3 = stage3))

}

## load_map_results --------------------------------------------------------------------------------

load_map_results <- function(indicator, indicator_group, run_date, type, raked, start_year, end_year, single_year=0,
                             geo_levels = c("raster", "admin1", "admin2"),
                             custom_path, custom_vars,
                             use.sf,
                             cores=1) {
  
  years <- paste(start_year, end_year, sep="_")
  
  if (single_year == 0) year_list <- start_year:end_year else year_list <- single_year

  ## Set the input directory
  maps_path <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date)

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
  if ("admin1" %in% geo_levels) {
    message('loading admin1 data')
    
    if(missing(custom_path)) {
      pred <- paste0(maps_path, "/pred_derivatives/admin_summaries/", indicator, "_admin_1_", 
                     ifelse(raked, "raked", "unraked"), "_summary.csv") %>% fread
    } else {
      pred <- fread(custom_path$admin1)
      if(!missing(custom_vars)) setnames(pred, custom_vars$admin1, type) 
    }
    message('-> admin1 found and fread')

    pred <- pred[year %in% year_list, c('ADM1_CODE', 'year', type), with = F]
    setnames(pred, type, 'outcome')

    if(use.sf==T) {
      
      admin1 <- get_admin_shapefile(1) %>% st_read
      admin1 <- admin1[admin1$ADM1_CODE %in% pred$ADM1_CODE,]
      admin1 <-merge(admin1, pred, by="ADM1_CODE", allow.cartesian=T)
      
      message('--> admin1 results merged to sf')
      
    } else {
    
      admin1 <- shapefile(get_admin_shapefile(1)) #TODO: allow to select GAUL vs GADM version
      admin1 <- admin1[admin1@data$ADM1_CODE %in% pred$ADM1_CODE,]
      admin1 <- SpatialPolygonsDataFrame(gSimplify(admin1, tol = 0.1, topologyPreserve = T), data = admin1@data)
      for (i in 1:length(admin1)) admin1@polygons[[i]]@ID <- as.character(admin1@data[i, "ADM1_CODE"])
      admin1 <- data.table(fortify(admin1))
      admin1[, ADM1_CODE := as.numeric(id)]
      admin1 <- merge(admin1, pred, by="ADM1_CODE", allow.cartesian=T)
      setkey(admin1, id, group, order, year)
      
      message('--> admin1 results merged to shapefile')
      
    }
  }

  ## admin2 estimates and shape file
  if ("admin2" %in% geo_levels) {
    message('loading admin2 data')
    
    if(missing(custom_path)) {
      pred <- paste0(maps_path, "/pred_derivatives/admin_summaries/", indicator, "_admin_2_", 
                     ifelse(raked, "raked", "unraked"), "_summary.csv") %>% fread
    } else {
      pred <- fread(custom_path$admin2)
      if(!missing(custom_vars)) setnames(pred, custom_vars$admin2, type) 
    }
    message('-> admin2 found and fread')

    pred <- pred[year %in% year_list, c('ADM2_CODE', 'year', type), with = F]
    setnames(pred, type, 'outcome')
    
    if(use.sf==T) {

      admin2 <- get_admin_shapefile(2) %>% st_read
      admin2 <- admin2[admin2$ADM2_CODE %in% pred$ADM2_CODE,]
      admin2 <-merge(admin2, pred, by="ADM2_CODE", allow.cartesian=T)
      
      message('--> admin2 results merged to sf')
      
    } else {

      admin2 <- shapefile(get_admin_shapefile(2))
      admin2 <- admin2[admin2@data$ADM2_CODE %in% pred$ADM2_CODE,]
      admin2 <- SpatialPolygonsDataFrame(gSimplify(admin2, tol = 0.1, topologyPreserve = T), data = admin2@data)
      for (i in 1:length(admin2)) admin2@polygons[[i]]@ID <- as.character(admin2@data[i, "ADM2_CODE"])
      admin2 <- data.table(fortify(admin2))
      admin2[, ADM2_CODE := as.numeric(id)]
      admin2 <- merge(admin2, pred, by="ADM2_CODE", allow.cartesian=T)
      setkey(admin2, id, group, order, year)
      
      message('--> admin2 results merged to shapefile')
    
    }
  }

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
plot_map <- function(map_data, annotations, title, limits, 
                     legend_colors, legend_color_values, legend_breaks, legend_labels, legend_title, custom_scale=F,
                     pop.mask=T, lake.mask=T, borders=T, stage3.mask=T,
                     zoom,
                     debug=F) {
  
  if (debug) browser()
  
  ## Enforce limits
  map_data$plot_var <- pmax(limits[1], pmin(limits[2], map_data$outcome)) #TODO set in to enforce lower limit as well?

  if (!custom_scale) {

    start_range <- range(map_data$outcome, na.rm = T)

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
    
  } else {
    breaks <- legend_breaks
    labels <- legend_labels
  }
  
  ## Plot the base map (this is what shows in places with no estimates and no mask)
    canvas <- ggplot() + 
      geom_polygon(data = annotations$adm0, aes(x = long, y = lat, group = group), color = 'gray90', fill = 'gray90')
  
  ## Zoom
  if (!missing(zoom)) {
    canvas <- canvas + 
      xlim(zoom$x1, zoom$x2)  +
      ylim(zoom$y1, zoom$y2)
  }

  ## Plot predictions
  if ("group" %in% names(map_data)) {
    gg <- canvas + geom_polygon(data = map_data, aes(fill = plot_var, y = lat, x = long, group = group)) + 
      coord_equal(ratio = 1)
  } else if (class(map_data)[1] =='sf') {
    gg <- canvas + geom_sf(data = map_data, aes(fill = plot_var), lwd=0) + coord_sf(datum = NA)
  } else {
    gg <- canvas + geom_raster(data = map_data, aes(fill = plot_var, y = lat, x = long)) + 
      coord_equal(ratio = 1)
  }

  ## Plot mask, lakes, and adm boarders
  if (pop.mask==T) gg <- gg + annotate(geom = 'raster', x = annotations$mask$long, y = annotations$mask$lat, fill = 'gray70')
  if (lake.mask==T) gg <- gg + annotate(geom = 'raster', x = annotations$lakes$long, y = annotations$lakes$lat, fill = 'lightblue')
  if (borders==T) gg <- gg + geom_path(data = annotations$adm0, aes(x = long, y = lat, group = group), color = 'black', size = 0.2)
  if (stage3.mask==T) gg <- gg + geom_path(data = annotations$stage3, aes(x = long, y = lat, group = group), color = 'gray70', size = 0.2)

  ## Scales
  gg <- gg +
    scale_fill_gradientn(colors = legend_colors, values = legend_color_values,
                         limits = range(breaks), breaks = breaks, labels = labels, name = legend_title)
  
  ## Labels & aesthetics
  gg <- gg +
    labs(x="", y="", title=title) +
    theme_classic() +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          legend.position = c(0, 0), legend.justification = c(0, 0),
          #legend.text=element_text(size=10),
          plot.title = element_text(hjust=0.5), plot.margin=unit(c(0, 0, 0, 0), "in")) +
    guides(fill = guide_colorbar(barwidth = 0.5, barheight = 7))

  return(gg)
  
}

## map_model_results -------------------------------------------------------------------------------

map_model_results <- function(indicator,
                              indicator_group,
                              run_date,
                              type = 'mean',
                              raked = T,
                              lvl_years = c(2000, 2005, 2010, 2016),
                              lvl_colors = c('#FDE0DD', '#FCC5C0', '#FA9FB5', '#F768A1', '#DD3497', '#AE017E', '#7A0177', '#49006A'),
                              lvl_limits = c(0, 0.25),
                              diff_years = list(c(2000, 2005), c(2005, 2010), c(2010, 2016), c(2000, 2016)),
                              diff_colors = c('#003C30', '#01665E', '#35978F', '#80CDC1', '#F6E8C3', '#DFC27D', '#BF812D', '#8C510A', '#543005'),
                              diff_limits = c(-0.10, 0.10),
                              include_diff = TRUE,
                              limits_type = "absolute",
                              geo_levels = c("raster", "admin1", "admin2"),
                              plot_by_year = TRUE,
                              plot_combined = TRUE,
                              file_type = "pdf") {

  ## Quick argument checks
  if (!limits_type %in% c("absolute", "quantile")) stop("limits_type must be 'absolute' or 'quantile'")
  if (length(lvl_limits) != 2 | length(diff_limits) != 2) stop("lvl_limits & diff_limits must both be length 2")
  if (sum(!geo_levels %in% c("raster", "admin1", "admin2")) > 0) stop("geo_levels can only include 'raster', 'admin1', and 'admin2'")
  if (!file_type %in% c("pdf", "png")) stop("file_type must be 'pdf' or 'png'")
  if (!type %in% c("mean", "cirange", "cfb")) stop("type must be 'mean', 'cirange', or 'cfb'")

  ## Create output directory
  out_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date, '/results_maps/')
  dir.create(out_dir, showWarnings = F)

  ## Load data
  message("Loading data")
  annotations <- load_map_annotations()
  pred <- load_map_results(indicator, indicator_group, run_date, type, raked, unique(c(lvl_years, unlist(diff_years))), geo_levels)

  ## Make level maps
  message("Make levels maps")
  if (limits_type == "quantile") {
    lvl_limits <- quantile(unlist(lapply(pred, function(x) x$outcome)), probs = lvl_limits, na.rm = T)
    lvl_limits <- c(plyr::round_any(lvl_limits[1], 0.01, floor), plyr::round_any(lvl_limits[2], 0.01, ceiling))
  }

  legend_title <- c(mean = "Prev.", cirange = "UI range", cfb = "CFB")[type]

  for (g in names(pred)) {
    message(paste0("...", g))

    # make maps and plot by year
    plot_list <- lapply(lvl_years, function(y) {
      message(paste0("......", y))
      gg <- plot_map(map_data = pred[[g]][year == y,], annotations = annotations, title = y,
                     legend_title = legend_title, limits = lvl_limits, legend_colors = lvl_colors)
      if (plot_by_year) {
        file_name <- paste0(out_dir, indicator, '_', type, if (raked) '_raked_' else '_unraked_', g, '_', y, '.', file_type)
        if (file_type == "pdf") pdf(file_name, height=7, width=7)
        if (file_type == "png") png(file_name, height=7, width=7, units = "in", res = 1200)
        plot(gg)
        dev.off()
      }
      return(gg)
    })

    # plot combined
    if (plot_combined & length(lvl_years) > 1) {
      message("......combined")
      file_name <- paste0(out_dir, indicator, '_', type, if (raked) '_raked_' else '_unraked_', g, '_combined.', file_type)
      if (file_type == "pdf") pdf(file_name, height=14, width=14)
      if (file_type == "png") png(file_name, height=14, width=14, units = "in", res = 1200)
      do.call("grid.arrange", plot_list)
      dev.off()
    }

    rm(plot_list)
  }

  ## Make difference maps
  if (include_diff) {
    message("Make differences maps")
    diff <- calc_diff_map(pred, diff_years)
    if (limits_type == "quantile") {
      diff_limits <- quantile(unlist(lapply(diff, function(x) x$outcome)), probs = diff_limits, na.rm = T)
      if (diff_limits[1] < 0 & diff_limits[2] > 0) diff_limits <- c(-1, 1)*max(abs(diff_limits)) # if crossing zero, ensure symmetry.
      diff_limits <- c(plyr::round_any(diff_limits[1], 0.01, floor), plyr::round_any(diff_limits[2], 0.01, ceiling))
    }

    legend_title <- paste0("Change in\n", legend_title)

    for (g in names(diff)) {
      message(paste0("...", g))

      # make maps and plot by year
      plot_list <- lapply(diff_years, function(y) {
        yrs <- paste(y, collapse = "-")
        message(paste0("......", yrs))
        gg <- plot_map(map_data = diff[[g]][years == yrs,], annotations = annotations, title = yrs,
                       legend_title = legend_title, limits = diff_limits, legend_colors = diff_colors)
        if (plot_by_year) {
          file_name <- paste0(out_dir, indicator, '_diff_', type, if (raked) '_raked_' else '_unraked_', g, '_', y[1], '_', y[2], '.', file_type)
          if (file_type == "pdf") pdf(file_name, height=7, width=7)
          if (file_type == "png") png(file_name, height=7, width=7, units = "in", res = 1200)
          plot(gg)
          dev.off()
        }
        return(gg)
      })

      # plot combined
      if (plot_combined & length(diff_years) > 1) {
        message("......combined")
        file_name <- paste0(out_dir, indicator, '_diff_', type, if (raked) '_raked_' else '_unraked_', g, '_combined.', file_type)
        if (file_type == "pdf") pdf(file_name, height=14, width=14)
        if (file_type == "png") png(file_name, height=14, width=14, units = "in", res = 1200)
        do.call("grid.arrange", plot_list)
        dev.off()
      }

      rm(plot_list)
    }
  }

  return("Maps saved!")
}
