#custom GBD data loading fx, which switches diacritics to F (line 14)
#TODO find out why T is breaking your code with the error:
#Error during wrapup: 'old' is longer than 'new'
load_gbd_data2 <- function(gbd_type,
         gbd_name,
         gaul_list,
         measure_id = 6,
         age_group_id = 1,
         metric_id = 3,
         year_ids = c(2000:2015),
         return_by_age_sex = "no",
         collapse_age_sex = FALSE,
         gbd_round_id = 4) {

  # get GAUL to location_id mapping
  gaul_to_loc_id <- get_location_code_mapping(remove_diacritics = F)
  gaul_to_loc_id <- gaul_to_loc_id[, list(location_id = loc_id, GAUL_CODE)]
  
  loc_ids <- gaul_to_loc_id[GAUL_CODE %in% gaul_list, location_id]
  
  # load covariate data
  if (gbd_type == "covariate") {
    
    # get covariate metadata from covariate_name_short
    metadata <- get_covariate_metadata()
    covariate_id <- metadata[covariate_name_short == tolower(gbd_name), covariate_id]
    covariate_by_age <- metadata[covariate_name_short == tolower(gbd_name), by_age]
    covariate_by_sex <- metadata[covariate_name_short == tolower(gbd_name), by_sex]

    # get covariate data
    source('/snfs1/temp/central_comp/libraries/current/r/get_covariate_estimates.R')
    if (covariate_by_age) gbd_estimates <- get_covariate_estimates(covariate_id = covariate_id, location_id = loc_ids, year_id = year_ids, age_group_id = age_group_id, gbd_round_id = gbd_round_id)
    if (!covariate_by_age) gbd_estimates <- get_covariate_estimates(covariate_id = covariate_id, location_id = loc_ids, year_id = year_ids, gbd_round_id = gbd_round_id)
    
    # collapse to all age, both sexes (if specified, and if there are multiple age and/or sex groups)
    if (collapse_age_sex & gbd_estimates[, uniqueN(age_group_name) > 1 | uniqueN(sex_id) > 1]) {
      
      # get population data
      source('/snfs1/temp/central_comp/libraries/current/r/get_population.R')
      gbd_pops <- get_population(age_group_id = gbd_estimates[, unique(age_group_id)],
                                 location_id = gbd_estimates[, unique(location_id)],
                                 year_id = gbd_estimates[, unique(year_id)],
                                 sex_id = gbd_estimates[, unique(sex_id)])
      
      # population-weight the covariate data
      gbd_estimates <- merge(gbd_estimates, gbd_pops, by=c('location_id', 'sex_id', 'age_group_id', 'year_id'))
      gbd_estimates <- gbd_estimates[, list(mean_value = weighted.mean(mean_value, population, na.rm=T)), by='location_id,year_id']
    }
    
    # format and return
    gbd_estimates <- merge(gbd_estimates, gaul_to_loc_id, by="location_id")
    setnames(gbd_estimates, c("GAUL_CODE", "year_id", "mean_value"), c("name", "year", "mean"))
    
    if(return_by_age_sex=='no') gbd_estimates <- gbd_estimates[, list(name, year, mean)]
    if(return_by_age_sex=='yes') gbd_estimates <- gbd_estimates[, list(name, year, mean, sex_id, age_group_id)]
    
    return(gbd_estimates)
  }
  
  # load cause data
  if (gbd_type == "output") {
    
    # get cause metadata
    source('/snfs1/temp/central_comp/libraries/current/r/get_cause_metadata.R')
    metadata <- get_cause_metadata(cause_set_id = 2, gbd_round_id = gbd_round_id)
    cause_id <- suppressWarnings(as.numeric(gbd_name))
    if (is.na(cause_id)) cause_id <- metadata[acause == gbd_name, cause_id]
    
    # get cause data
    source('/snfs1/temp/central_comp/libraries/current/r/get_outputs.R')
    gbd_estimates <- get_outputs(topic = "cause",
                                 version = "best",
                                 gbd_round_id = gbd_round_id,
                                 cause_id = cause_id,
                                 measure_id = measure_id,
                                 metric_id = metric_id,
                                 age_group_id = age_group_id,
                                 location_id = loc_ids,
                                 year_id = year_ids)
    
    all_data <- merge(gaul_to_loc_id, gbd_estimates, by="location_id")
    setnames(all_data, c("GAUL_CODE", "year_id", "val"), c("name", "year", "mean"))
    all_data <- all_data[, list(name, year, mean)]
    
    return(all_data)
    
  }
  
}

input_aggregate_admin2 <- function(indicator,
         indicator_group,
         regions = c("cssa", "wssa", "essa", "sssa", "name"),
         run_date = NULL,
         input_data = NULL,
         indicator_family = "binomial",
         svy_id = "nid",
         sample_column = "sum_of_sample_weights",
         subnational_nids = NULL) {
  
  # Make sure this is run on singularity container in Rstudio or lbd singularity to use sf package
  if(!is_lbd_singularity() & !is_rstudio(check_singularity = TRUE)){
    stop("must be run in lbd singularity or Rstudio singularity container")
  }
  
  require(dplyr)
  require(data.table)
  require(sf)
  require(ggplot2)
  require(stringr)
  
  # If input_data not passed to the function, load in input data from model, or from mbg if the appropriate column name is not specified.
  if (is.null(input_data)){
    mod_dir <- sprintf('/share/geospatial/mbg/%s/%s/output/%s/', indicator_group, indicator, run_date)
    message(paste0("Input data was not provided, so loading from model directory: ", mod_dir,
                   ". \n If this does not exist, consider passing input data as an argument to this function"))
    input_data <- fread(paste0(mod_dir, 'input_data.csv'))
    
    if (!sample_column %in% names(input_data)) {
      message("Sample weights column not found in model directory, pulling from input data on J drive")
      input_data <- fread(paste0("/home/j/WORK/11_geospatial/10_mbg/input_data/", indicator, ".csv"))
      if (!sample_column %in% names(input_data)) stop("Sample weights column not found on J drive, make sure to add a sum of sample weights column)                                         in collapse code and specify columns name as sample_column argument in function")
    }
  } else {
    if (!sample_column %in% names(input_data)) {
      stop("Sample weights column not found in the provided input data. Make sure that this column is included and specified by the sample_column
           argument in the function call.")
    }
    }
  
  if (!"source" %in% names(input_data)) stop("Need to specify the source of the data under a source column in the input data")
  if (!"point"  %in% names(input_data)) stop("You need a column in your input data called 'points' that classifies point data as 1 and polygon as 0")
  
  # Load in shapefile
  admin_shp <- st_read(get_admin_shapefile(admin_level = 2), quiet=T)
  
  # Add country name (not just 3 letter abbreviation) to input data
  # Subset input data to given region defined by the regions argument 
  gaul_codes <- get_gaul_codes(regions) 
  
  gaul_to_loc_id <- 
    gaul_to_loc_id <- get_location_code_mapping(remove_diacritics = F) %>% #TODO figure out why this is breaking
    dplyr::select(GAUL_CODE, loc_name, ihme_lc_id) %>% 
    dplyr::rename(location_name = loc_name) %>%
    filter(GAUL_CODE %in% gaul_codes)
  
  
  # Join input data to location names to match shapefile. Some of those countries need to be manually changed to fit shapefile names
  input_data <-
    input_data %>%
    left_join(gaul_to_loc_id, by = c("country" = "ihme_lc_id")) %>%
    rowwise() %>% 
    mutate(location_name = ifelse(location_name == "Tanzania", "United Republic of Tanzania", location_name)) %>%
    mutate(location_name = ifelse(location_name == "The Gambia", "Gambia", location_name)) %>% 
    mutate(location_name = ifelse(location_name == "Cote d'Ivoire", "C??te d'Ivoire", location_name)) %>%
    ungroup() %>% data.table() %>%
    setnames(c(svy_id, sample_column), c("svy_id", "sample_column"))
  
  missing <-
    input_data %>%
    filter(is.na(location_name)) %>% nrow()
  
  message(paste0(round(100* missing / nrow(input_data), 2), " % of data is from outside of specified regions: ", paste(regions, collapse = " ")))
  
  input_data <-
    input_data %>% filter(!is.na(location_name))
  
  # Subset shapefile to include countries we have data on
  countries <- input_data$location_name %>% unique()
  
  admin_shp <-
    admin_shp %>%
    mutate(ADM0_NAME = as.character(ADM0_NAME)) %>%
    filter(ADM0_NAME %in% countries)
  
  # Assign input data to correct admin0/admin1/dmin2 in one step
  message("Assigning lat/longs to correct admin level")
  input_admin <-
    input_data %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(admin_shp)) %>%
    st_join(admin_shp) %>%
    st_set_geometry(NULL) %>%
    setnames(eval(indicator), "prev")
  
  missing <-
    input_admin %>%
    filter(is.na(ADM0_CODE), is.na(ADM1_CODE), is.na(ADM2_CODE)) %>% nrow()
  
  message(paste0(round(100* missing / nrow(input_admin), 3), " % of data could not be matched to an admin level"))
  
  input_admin <-
    input_admin %>%
    filter(!is.na(ADM0_CODE), !is.na(ADM1_CODE), !is.na(ADM2_CODE)) %>% data.table() #remove those that are not assigned to an admin0/admin2
  
  # Make sure they are assigned to the correct country, some are right over the border and so are pushed into a different area.
  # These are excluded (usually not substantial) becuase they then do not nest well with admin1 and admin2 estimates
  wrong_admin0 <- nrow(input_admin[location_name != ADM0_NAME])
  message(paste0(round(wrong_admin0/ nrow(input_admin), 3), " % of input data is matched to a different country.\nThese are usually located on the border and will be dropped for the visualization."))
  
  input_admin <- input_admin[location_name == ADM0_NAME]
  
  # If binomial make sure it is prevalence space
  if (indicator_family == "binomial") input_admin[, prev := prev / N]
  
  # Collapse to admin 0 level
  message("collapsing to admin 0 level")
  input_admin0 <-
    input_admin %>%
    group_by(svy_id, source, point, ADM0_NAME, ADM0_CODE) %>%
    dplyr::summarise(
      year = floor(median(year, na.rm = T)),
      outcome = weighted.mean(prev, sample_column),
      N = sum(N * weight)
    ) %>%
    ungroup() %>%
    data.table()
  
  # Collapse to admin 1 level
  message("collapsing to admin 1 level")
  input_admin1 <-
    input_admin %>%
    group_by(svy_id, source, point, ADM0_NAME, ADM0_CODE, ADM1_NAME, ADM1_CODE) %>%
    dplyr::summarise(
      year = floor(median(year, na.rm = T)),
      outcome = weighted.mean(prev, sample_column),
      N = sum(N * weight)
    ) %>%
    ungroup() %>%
    data.table()
  
  # Collapse to admin 2 level
  message("collapsing to admin 2 level")
  input_admin2 <-
    input_admin %>%
    group_by(svy_id, source, point, ADM0_NAME, ADM0_CODE, ADM1_NAME, ADM1_CODE, ADM2_NAME, ADM2_CODE) %>%
    dplyr::summarise(
      year = floor(median(year, na.rm = T)),
      outcome = weighted.mean(prev, sample_column),
      N = sum(N * weight)
    ) %>%
    ungroup() %>%
    data.table()
  
  # Change source/polygon to factor and shorten survey names to fit on plot legend
  input_clean <- function(df) {
    df %>%
      rowwise() %>% 
      mutate(source = ifelse(source == "MACRO_DHS", "DHS", source)) %>%
      mutate(source = ifelse(source == "MACRO_AIS", "AIS", source)) %>%
      mutate(source = ifelse(source == "UNICEF_MICS", "MICS", source)) %>%
      mutate(source = ifelse(source == "COUNTRY_SPECIFIC", "CS", source)) %>%
      mutate(source = ifelse(source == "WB_CWIQ", "CWIQ", source)) %>%
      mutate(source = ifelse(source == "WB_CWIQ", "CWIQ", source)) %>%
      mutate(source = ifelse(source == "WB_LSMS", "LSMS", source)) %>%
      mutate(source = ifelse(source == "WB_LSMS_ISA", "ISA", source)) %>%
      mutate(source = ifelse(source == "WB_PRIORITY_SURVEY", "PRI_S", source)) %>%
      mutate(source = ifelse(source == "ARAB_LEAGUE_PAPFAM", "PAPFAM", source)) %>%
      mutate(source = ifelse(source == "JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020", "PMA", source)) %>% 
      mutate(source = ifelse(nchar(source) > 6, str_trunc(source, 6, ellipsis = ""), source)) %>% #truncate source if it is too long and not specified above
      ungroup() %>% 
      mutate(source = as.factor(source)) %>%
      mutate(point = as.factor(point)) %>%
      data.table()
  }
  
  input_admin0 <- input_clean(input_admin0)
  input_admin1 <- input_clean(input_admin1)
  input_admin2 <- input_clean(input_admin2)
  
  # If subnational NID's are included, assign them values 2 and 3 for point and polygon data that are subnationally representative, respectively
  subnational_nid_subset <- function(df) {
    df %>% 
      mutate(point = as.numeric(levels(point))[point]) %>% 
      rowwise() %>% 
      mutate(point = ifelse(svy_id %in% subnational_nids, point + 2, point)) %>% 
      ungroup() %>% 
      mutate(point = as.factor(point)) %>% 
      data.table()
  }
  
  if (!is.null(subnational_nids)){
    input_admin0 <- subnational_nid_subset(input_admin0)
    input_admin1 <- subnational_nid_subset(input_admin1)
    input_admin2 <- subnational_nid_subset(input_admin2)
  }
  
  # Final list contains each aggregated admin
  input_admins <- list(ad0 = input_admin0, ad1 = input_admin1, ad2 = input_admin2)
  return(input_admins)
}

subnational_ts_plots2 <- function(ad0_df,
         ad1_df,
         ad2_df,
         ad0_shape = NULL,
         ad1_shape = NULL,
         ad2_shape = NULL,
         ind_title,
         out_dir,
         out_filename_format = "subnational_ts_plots_%s.pdf",
         val_range = c(0,1),
         highisbad = T,
         ad0_map_regions = NULL,
         ad0_map_region_titles = NULL,
         ad1_map_countries = NULL,
         plot_levels = c("ad0", "ad1", "ad2"),
         multiple_runs = F,
         plot_data = F,
         ad0_data = NULL,
         ad1_data = NULL,
         ad2_data = NULL,
         title_plot_size = NULL,
         title_grob_size = NULL,
         verbose = F) {
  
  ################################################################################
  # 0. SETUP #####################################################################
  # Load packages, define themes, and set up objects needed for plotting #########
  ################################################################################
  
  # Load libraries and functions -------------------------------------------------
  
  library(ggrepel)
  library(gridExtra)
  library(ggplot2)
  
  # Check to see if plot_data = T that admin data is attached
  if (plot_data == T) {
    if ("ad0" %in% plot_levels & is.null(ad0_data)) stop(paste("To plot data, you must include aggregated ad0_data.",
                                                               "See input_aggregate_admin function"))
    if ("ad1" %in% plot_levels & is.null(ad1_data)) stop(paste("To plot data, you must include aggregated ad1_data.",
                                                               "See input_aggregate_admin function"))
    if ("ad2" %in% plot_levels & is.null(ad2_data)) stop(paste("To plot data, you must include aggregated ad2_data.",
                                                               "See input_aggregate_admin function"))
  }
  
  if (plot_data == T & multiple_runs == T) message(paste("You are choosing to plot multiple indicators/model runs with data.\n",
                                                         "This is usually done if plotting multiple model runs from the same indicator"))
  
  # If plotting multiple model runs, make sure there is a column title 'run'
  if (multiple_runs == T){
    if (!"run" %in% names(ad0_df)) stop("Must have a column title 'run' that differentiates between model runs if plotting multiple indicators/model runs. See add_run_label function")
    if (!"run" %in% names(ad1_df)) stop("Must have a column title 'run' that differentiates between model runs if plotting multiple indicators/model runs. See add_run_label function")
    if (!"run" %in% names(ad2_df)) stop("Must have a column title 'run' that differentiates between model runs if plotting multiple indicators/model runs. See add_run_label function")
  }
  # Create directories -----------------------------------------------------------
  
  dir.create(out_dir, showWarnings = F)
  
  # Prepare data and shapefiles --------------------------------------------------
  
  # Merge on ihme_lc_ids
  gaul_to_loc_id <- get_location_code_mapping(remove_diacritics = F)
  gaul_to_loc_id <- subset(gaul_to_loc_id, select = c("GAUL_CODE", "ihme_lc_id"))
  setnames(gaul_to_loc_id,"GAUL_CODE", "ADM0_CODE")
  
  ad0_df <- merge(copy(as.data.table(ad0_df)), gaul_to_loc_id, all.x=T, all.y=F, by = 'ADM0_CODE')
  ad1_df <- merge(copy(as.data.table(ad1_df)), gaul_to_loc_id, all.x=T, all.y=F, by = 'ADM0_CODE')
  ad2_df <- merge(copy(as.data.table(ad2_df)), gaul_to_loc_id, all.x=T, all.y=F, by = 'ADM0_CODE')
  
  if (plot_data == T){
    ad0_data <- merge(copy(as.data.table(ad0_data)), gaul_to_loc_id, all.x=T, all.y=F, by = 'ADM0_CODE')
    ad1_data <- merge(copy(as.data.table(ad1_data)), gaul_to_loc_id, all.x=T, all.y=F, by = 'ADM0_CODE')
    ad2_data <- merge(copy(as.data.table(ad2_data)), gaul_to_loc_id, all.x=T, all.y=F, by = 'ADM0_CODE')
  }
  
  # Drop Ma'tan al-Sarra if present
  ad0_df <- subset(ad0_df, ADM0_CODE != 40762)
  ad1_df <- subset(ad1_df, ADM0_CODE != 40762)
  ad2_df <- subset(ad2_df, ADM0_CODE != 40762)
  
  # Set up default values for title font size if not specified
  if (is.null(title_plot_size)) title_plot_size <- 20
  if (is.null(title_grob_size)) title_grob_size <- 30
  
  if (verbose == T) message("Simplifying shapes")
  
  # Wrapper for gSimplify() to allow it to work with SPDFs
  simplify_spdf <- function(the_spdf, ...) {
    simple_spdf <- gSimplify(the_spdf, topologyPreserve = T, ...)
    return(SpatialPolygonsDataFrame(simple_spdf, the_spdf@data))
  }
  
  # Subset and simplify shapefiles for memory & speed
  ad0_codes <- unique(ad0_df$ADM0_CODE)
  # If any region of Africa is being used, use full map of Africa so it does not look disjointed
  if (any(ad0_map_regions %in% c("cssa", "wssa", "essa", "sssa", "name"))) ad0_codes <- union(ad0_codes, get_gaul_codes("Africa"))
  subset_codes <- ad0_codes
  
  # Use simplified GAUL shapefiles as default if none provided, if provided simplify the shapefile for speed
  if (is.null(ad0_shape)){
    ad0_shape_simple <- readRDS('/share/geospatial/mbg/time_trend_plots/ad0_shape_simple.rds') %>% subset(ADM0_CODE %in% subset_codes)
  } else {
    ad0_shape_simple <- subset(ad0_shape, ADM0_CODE %in% subset_codes) %>% simplify_spdf(tol = 0.001)
  }
  
  if (is.null(ad1_shape)){
    ad1_shape_simple <- readRDS('/share/geospatial/mbg/time_trend_plots/ad1_shape_simple.rds') %>% subset(ADM0_CODE %in% subset_codes)
  } else {
    ad1_shape_simple <- subset(ad1_shape, ADM0_CODE %in% subset_codes) %>% simplify_spdf(tol = 0.001)
  }
  
  if (is.null(ad2_shape)){
    ad2_shape_simple <- readRDS('/share/geospatial/mbg/time_trend_plots/ad2_shape_simple.rds') %>% subset(ADM0_CODE %in% subset_codes)
  } else {
    ad2_shape_simple <- subset(ad2_shape, ADM0_CODE %in% subset_codes) %>% simplify_spdf(tol = 0.001)
  }
  
  # Make sure that the admin codes are converted to numeric if they are factors
  if (is.factor(ad0_shape_simple@data$ADM0_CODE)) ad0_shape_simple@data$ADM0_CODE <- as.numeric(levels(ad0_shape_simple@data$ADM0_CODE))[ad0_shape_simple@data$ADM0_CODE]
  if (is.factor(ad1_shape_simple@data$ADM1_CODE)) ad1_shape_simple@data$ADM1_CODE <- as.numeric(levels(ad1_shape_simple@data$ADM1_CODE))[ad1_shape_simple@data$ADM1_CODE]
  if (is.factor(ad2_shape_simple@data$ADM2_CODE)) ad2_shape_simple@data$ADM2_CODE <- as.numeric(levels(ad2_shape_simple@data$ADM2_CODE))[ad2_shape_simple@data$ADM2_CODE]
  
  # Add simple world shapefile if region is specified as Africa
  if ("ad0" %in% plot_levels & "Africa" %in% ad0_map_regions) world_shape_simple <- readRDS('/share/geospatial/mbg/time_trend_plots/world_shape_simple.rds')
  
  # Define some utility functions -----------------------------------------------------------------------------------------------------
  # Function to fix aspect ratios
  aspect_ratio_plot <- function(plt, aspect_ratio = 1, expand=0.05){
    
    xlims <- ggplot_build(plt)$layout$panel_scales$x[[1]]$range$range
    ylims <- ggplot_build(plt)$layout$panel_scales$y[[1]]$range$range
    
    # Try a different way if null - varies by ggplot build
    if (is.null(xlims)) xlims <- ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range
    if (is.null(ylims)) ylims <- ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range
    
    xmid <- mean(xlims); xrange <- xlims[2]-xlims[1]
    ymid <- mean(ylims); yrange <- ylims[2]-ylims[1]
    
    if (xrange/yrange < aspect_ratio) {
      new_yrange <- yrange*(1+expand)
      new_xrange <- new_yrange*aspect_ratio
    }
    
    if (xrange/yrange > aspect_ratio) {
      new_xrange = xrange*(1+expand)
      new_yrange <- new_xrange/aspect_ratio
    }
    
    new_xlim <- c(xmid - 0.5*new_xrange, xmid + 0.5*new_xrange)
    new_ylim <- c(ymid - 0.5*new_yrange, ymid + 0.5*new_yrange)
    
    return(plt+expand_limits(x=new_xlim,
                             y=new_ylim))
  }

  # Title generate makes the title Grob that goes in the right hand corner of the pdf
  title_generate <- function(ind_title,
                             year_list,
                             admin = 0,
                             ad0_reg_title = "",
                             ctry = "",
                             ad1_df_ad1 = ""){
    arrangeGrob(textGrob("", gp = gpar(fontsize = 30)),
                textGrob(str_wrap(ind_title, 18),
                         gp = gpar(fontsize = title_grob_size, fontface = "bold")),
                textGrob("", gp = gpar(fontsize = 10)),
                textGrob(if (admin == 0) ifelse(ad0_reg_title == "All countries", NULL, ad0_reg_title) else ctry,
                         gp = gpar(fontsize = 20)),
                textGrob("", gp = gpar(fontsize = 10)),
                textGrob(if (admin == 0) "National estimates" else if (admin == 1) "By First-level Administrative Unit" else paste0("Admin 1: ", unique(ad1_df_ad1$ADM1_NAME)),
                         gp = gpar(fontsize = 15)),
                textGrob("", gp = gpar(fontsize = 10)),
                textGrob(paste0(min(year_list), " - ", max(year_list)),
                         gp = gpar(fontsize = 18)),
                ncol = 1,
                heights = c(30, 25, 10, 25, 10, 15, 10, 20))
  }
  
  # Plot overlay defines where each object (ggplot and title) are placed on the pdf.
  # This depends on if data is being plotted, and if multiple runs are used
  plot_overlay <- function(plot_data, multiple_runs){
    if (plot_data == T) {
      if (multiple_runs == T) {
        lay <- rbind(
          c(1, 1, 1, 1, 1, 2, 2, 3, 3),
          c(1, 1, 1, 1, 1, 2, 2, 3, 3),
          c(1, 1, 1, 1, 1, 4, 4, 4, NA),
          c(1, 1, 1, 1, 1, 4, 4, 4, NA),
          c(1, 1, 1, 1, 1, 4, 4, 4, 5)
        )
      } else {
        lay <- rbind(
          c(1, 1, 1, 1, 1, 2, 2, 3, 3),
          c(1, 1, 1, 1, 1, 2, 2, 3, 3),
          c(1, 1, 1, 1, 1, 4, 4, 4, 4),
          c(1, 1, 1, 1, 1, 4, 4, 4, 4),
          c(1, 1, 1, 1, 1, 4, 4, 4, 4)
        )
      }
    } else {
      lay <- rbind(
        c(1, 1, 1, 1, 2, 2, 3, 3),
        c(1, 1, 1, 1, 2, 2, 3, 3),
        c(1, 1, 1, 1, 4, 4, 4, 4),
        c(1, 1, 1, 1, 4, 4, 4, 4),
        c(1, 1, 1, 1, 4, 4, 4, 4),
        c(1, 1, 1, 1, NA, NA, NA, 5)
      )
    }
    return(lay)
  }
  
  # Custom empty theme
  theme_empty <- theme_classic() +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
          strip.background = element_blank(),
          strip.text.x = element_blank())
  
  # Add IHME logo for plotting later
  ihme_logo <- png::readPNG('/share/geospatial/mbg/time_trend_plots/ihme_logo.png')
  ihme_grob <- rasterGrob(ihme_logo)
  
  # Add slightly different version for plotting multiple runs
  ihme_grob_multiple <- rasterGrob(ihme_logo, y = 0.2)
  
  # Only list countries that are within the specified regions
  if (!is.null(ad0_map_regions)) {
    ad0_code_list <- lapply(ad0_map_regions, get_gaul_codes)
    names(ad0_code_list) <- ad0_map_region_titles
  } else {
    ad0_code_list <- list(unique(ad0_df$ADM0_CODE))
    names(ad0_code_list) <- "All countries"
  }
  
  # Only list countries that are within the given regions
  countries <- sort(unique(ad0_df[ADM0_CODE %in% unlist(ad0_code_list)]$ADM0_NAME))
  
  # Get years
  year_list <- unique(ad0_df$year)
  
  ################################################################################
  # I. NATIONAL-LEVEL PLOTS FOR ALL COUNTRIES ####################################
  ################################################################################
  
  if ("ad0" %in% plot_levels) {
    
    pdf_filename <- paste0(out_dir, sprintf(out_filename_format, "all_countries"))
    
    if (verbose == T) {
      message("\n############################################################")
      message("############################################################")
      message(paste0("Plotting national-level estimates by region"))
      message(paste0("  Writing output to ", pdf_filename))
    }
    
    pdf(file = pdf_filename,
        height = 10,
        width = 18)
    
    for (i in 1:length(ad0_code_list)) {
      
      # Set up names & titles for this region ---------------------------------------
      ad0_reg_codes <- ad0_code_list[[i]]
      ad0_reg_title <- names(ad0_code_list)[i]
      
      ad0_df_reg <- subset(ad0_df, ADM0_CODE %in% ad0_reg_codes)
      if (plot_data == T) ad0_data_reg <- subset(ad0_data, ADM0_CODE %in% ad0_reg_codes)
      
      if (verbose == T) message(paste0("  ", ad0_reg_title))
      
      # First, time series plot of all admin0s in the region
      n_ad0s <- length(unique(ad0_df_reg$ADM0_CODE))
      
      # Try to wrap names of countries as needed
      if (n_ad0s >  0 & n_ad0s <= 16) wrap_width <- 18
      if (n_ad0s > 16 & n_ad0s <= 25) wrap_width <- 12
      if (n_ad0s > 25)                wrap_width <- 9
      
      # Set up plot names; include national-level estimates; ensure ordering correct.
      # Use location ID as label modeling over more than 25 countries
      if (n_ad0s < 25) {
        ad0_df_reg[, plot_name := stringr::str_wrap(ADM0_NAME, width = wrap_width)]
      } else {
        ad0_df_reg[, plot_name := stringr::str_wrap(ihme_lc_id, width = wrap_width)]
      }
      
      # When plotting data, aligning plot names depending on number of admin0 in region
      if (plot_data == T){
        if (n_ad0s < 25) {
          ad0_data_reg[, plot_name := stringr::str_wrap(ADM0_NAME, width = wrap_width)]
        } else {
          ad0_data_reg[, plot_name := stringr::str_wrap(ihme_lc_id, width = wrap_width)]
        }
      }
      
      # Admin 0 time series plot --------------------------------------------------------------
      if (plot_data == F & multiple_runs == F) gg_ad0_ts <- time_series(ad0_df_reg, admin = 0, val_range, title_plot_size, ind_title)
      if (plot_data == T & multiple_runs == F) gg_ad0_ts <- time_series_data(ad0_df_reg, ad0_data_reg, admin = 0, val_range, title_plot_size, ind_title)
      if (plot_data == F & multiple_runs == T) gg_ad0_ts <- time_series_multiple(ad0_df_reg, admin = 0, val_range, title_plot_size, ind_title)
      if (plot_data == T & multiple_runs == T) gg_ad0_ts <- time_series_multiple_data(ad0_df_reg, ad0_data_reg, admin = 0, val_range, title_plot_size, ind_title)
      
      # A locator map --------------------------------------------------------------
      gg_locator_map <- location_map_draw(subset(ad0_shape_simple, ADM0_CODE %in% ad0_reg_codes), ad0_shape_simple)
      if (ad0_reg_title == "Africa") gg_locator_map <- location_map_draw(subset(world_shape_simple, ADM0_CODE %in% ad0_reg_codes), world_shape_simple)
      
      # A labeled map by countries -------------------------------------------------
      ad0_shape_reg <- subset(ad0_shape_simple, ADM0_CODE %in% ad0_reg_codes)
      
      centroids <- gCentroid(ad0_shape_reg, byid = T, id = ad0_shape_reg$ADM0_CODE)
      centroids <- as.data.frame(centroids) %>%
        cbind(rownames(.), .) %>%
        as.data.table(.) %>%
        setnames(., names(.), c("ADM0_CODE", "x", "y"))
      centroids$ADM0_CODE <- as.numeric(levels(centroids$ADM0_CODE))[centroids$ADM0_CODE]
      centroids <- merge(centroids, as.data.table(ad0_shape_reg), by = "ADM0_CODE")
      
      plot_df <- 
        ad0_df_reg %>% filter(year == max(year_list)) %>% dplyr::select(ADM0_CODE, ihme_lc_id, mean)
      
      # If plotting multiple runs, make sure there arent duplicated ADM0 codes
      if (multiple_runs == T) plot_df <- plot_df %>% distinct() %>% data.table()
      
      centroids <- merge(centroids, subset(plot_df, select = c("ADM0_CODE", "ihme_lc_id")))
      
      # Merge and fortify outside of ggplot to speed things up
      if (multiple_runs == F) ad0_shape_reg <- suppressWarnings(merge(ad0_shape_reg, plot_df))
      
      ad0_shape_reg@data$id = rownames(ad0_shape_reg@data)
      ad0_shape_reg_df = fortify(ad0_shape_reg, region="id")
      ad0_shape_reg_df <- merge(ad0_shape_reg_df, ad0_shape_reg@data, by = "id")
      ad0_shape_reg_df <- as.data.table(ad0_shape_reg_df)
      
      # Merge and fortify outside of ggplot to speed things up
      ad0_shape_reg <- suppressWarnings(merge(ad0_shape_reg, plot_df))
      
      if(highisbad) distiller_direction <- -1
      if(!highisbad) distiller_direction <- 1
      
      # Prepare last year maps
      if (multiple_runs == F) gg_lastyear_map <- last_year_map(ad0_shape_reg_df, centroids, admin = 0, val_range, distiller_direction, max(year_list))
      if (multiple_runs == T) gg_lastyear_map <- last_year_map_multiple(ad0_shape_reg_df, centroids, admin = 0, max(year_list))
      
      # Correct the aspect ratio if not plotting data
      if (plot_data == F) gg_lastyear_map <- aspect_ratio_plot(gg_lastyear_map, 4/3, 0.25)
      
      # Set up the final layout ---------------------------------------------------------------------------
      
      # Create a title grob
      #browser()
      title_grob <- title_generate(ind_title, year_list, title_grob_size, admin = 0, ad0_reg_title = ad0_reg_title)
      
      # Create the overall plot by arranging the grobs
      lay <- plot_overlay(plot_data, multiple_runs)
      if (plot_data == T & multiple_runs == F) master_plot <- arrangeGrob(gg_ad0_ts, gg_locator_map, title_grob, gg_lastyear_map, layout_matrix = lay)
      if (plot_data == T & multiple_runs == T) master_plot <- arrangeGrob(gg_ad0_ts, gg_locator_map, title_grob, gg_lastyear_map, ihme_grob_multiple, layout_matrix = lay)
      if (plot_data == F) master_plot <- arrangeGrob(gg_ad0_ts, gg_locator_map, title_grob, gg_lastyear_map, ihme_grob,
                                                     layout_matrix = lay, heights = c(1,1,1,1,1,0.6))
      grid.draw(master_plot)
      if (i != length(ad0_code_list)) plot.new()
      
      
    } # END `for (i in 1:length(ad0_code_list))`
    dev.off() # For PDF
  } # END `if` wrapper for ad0
  
  ################################################################################
  # II. ADMIN-1-LEVEL PLOTS FOR ALL COUNTRIES ####################################
  ################################################################################
  
  if ("ad1" %in% plot_levels) {
    
    pdf_filename <- paste0(out_dir, sprintf(out_filename_format, "all_countries_by_admin1"))
    
    if (verbose == T) {
      message("\n############################################################")
      message("############################################################")
      message(paste0("Plotting Admin-1-level estimates by country\n"))
      message(paste0("  Writing output to ", pdf_filename, "\n"))
      
    }
    
    pdf(file = pdf_filename,
        height = 10,
        width = 18)
    
    # Loop over countries
    # If specified, only map over listed countries
    if (!is.null(ad1_map_countries)){
      message(paste0("You have chosen to map only specified countries:\n",
                     paste(ad1_map_countries, collapse = " ")))
      countries <- ad1_map_countries
    }
    
    
    for (ctry in sort(countries)) {
      
      if(verbose == T) {
        message(paste0("  --> ", ctry, "..."))
      }
      
      # Get the ad0 code for this country
      ad0_code <- unique(ad0_df[ADM0_NAME == ctry]$ADM0_CODE)
      
      # Create subsets of ad1 and ad2 dfs just for this country (for convenience)
      ad0_df_ctry <- subset(ad0_df, ADM0_CODE == ad0_code)
      ad1_df_ctry <- subset(ad1_df, ADM0_CODE == ad0_code)
      ad2_df_ctry <- subset(ad2_df, ADM0_CODE == ad0_code)
      
      
      if (plot_data == T) {
        ad0_data_ctry <- subset(ad0_data, ADM0_CODE == ad0_code)
        ad1_data_ctry <- subset(ad1_data, ADM0_CODE == ad0_code)
        ad2_data_ctry <- subset(ad2_data, ADM0_CODE == ad0_code)
      }
      
      # Create a lookup table of ad1s
      ad1_table <- unique(subset(ad1_df_ctry, select = c("ADM1_NAME", "ADM1_CODE")))
      
      # Subset maps
      ad0_national <- subset(ad0_shape_simple, ADM0_CODE == ad0_code)
      ad1_national <- subset(ad1_shape_simple, ADM0_CODE == ad0_code)
      ad2_national <- subset(ad2_shape_simple, ADM0_CODE == ad0_code)
      
      # Wrap facet labels & make sure ADMIN 1 comes first
      n_ad1s <- length(unique(ad1_table$ADM1_CODE))
      
      if (n_ad1s >  0 & n_ad1s <= 16) wrap_width <- 18
      if (n_ad1s > 16 & n_ad1s <= 25) wrap_width <- 12
      if (n_ad1s > 25)                wrap_width <- 9
      
      # Set up plot names; include national-level estimates; ensure ordering correct
      ad1_df_ctry[, plot_name := stringr::str_wrap(ADM1_NAME, width = wrap_width)]
      ad0_df_ctry[, plot_name := "NATIONAL"]
      ad1_df_plot <- rbind(ad1_df_ctry, ad0_df_ctry, fill=T)
      ad1_lvls <- c("NATIONAL", setdiff(unique(ad1_df_plot$plot_name), "NATIONAL"))
      ad1_df_plot$plot_name <- factor(ad1_df_plot$plot_name, levels = ad1_lvls)
      
      if (plot_data == T) ad1_data_ctry[, plot_name := as.factor(stringr::str_wrap(ADM1_NAME, width = wrap_width))]
      
      # Admin 1 time series plot ----------------------------------------------------
      
      if (plot_data == F & multiple_runs == F) gg_ad1ts <- time_series(ad1_df_plot, admin = 1, val_range, title_plot_size, ind_title)
      if (plot_data == T & multiple_runs == F) gg_ad1ts <- time_series_data(ad1_df_plot, ad1_data_ctry, admin = 1, val_range, title_plot_size, ind_title)
      if (plot_data == F & multiple_runs == T) gg_ad1ts <- time_series_multiple(ad1_df_plot, admin = 1, val_range, title_plot_size, ind_title)
      if (plot_data == T & multiple_runs == T) gg_ad1ts <- time_series_multiple_data(ad1_df_plot, ad1_data_ctry, admin = 1, val_range, title_plot_size, ind_title)
      
      # A locator map --------------------------------------------------------------
      
      gg_locator_map <- location_map_draw(subset(ad0_shape_simple, ADM0_CODE == ad0_code), ad0_shape_simple)
      
      # A labeled map by admin1 ---------------------------------------------------
      
      centroids <- gCentroid(ad1_national, byid = T, id = ad1_national$ADM1_CODE)
      centroids <- as.data.frame(centroids) %>%
        cbind(rownames(.), .) %>%
        as.data.table(.) %>%
        setnames(., names(.), c("ADM1_CODE", "x", "y"))
      centroids$ADM1_CODE <- as.numeric(levels(centroids$ADM1_CODE))[centroids$ADM1_CODE]
      centroids <- merge(centroids, as.data.table(ad1_national), by = "ADM1_CODE")
      plot_df <- 
        ad1_df_ctry %>% filter(year == max(year_list)) %>% dplyr::select(ADM1_CODE, mean)
      
      # Merge and fortify outside of ggplot to speed things up
      if (multiple_runs == F) ad1_national <- suppressWarnings(merge(ad1_national, plot_df))
      if (multiple_runs == T) plot_df <- plot_df %>% distinct() %>% data.table()
      
      ad1_national@data$id = rownames(ad1_national@data)
      ad1_national_df = fortify(ad1_national, region="id")
      ad1_national_df <- merge(ad1_national_df, ad1_national@data, by = "id")
      ad1_national_df <- as.data.table(ad1_national_df)
      
      if(highisbad) distiller_direction <- -1
      if(!highisbad) distiller_direction <- 1
      
      # Prepare for plotting
      if (multiple_runs == F) gg_ad1_map <- last_year_map(ad1_national_df, centroids, admin = 1, val_range, distiller_direction, max(year_list))
      if (multiple_runs == T) gg_ad1_map <- last_year_map_multiple(ad1_national_df, centroids, admin = 1, max(year_list))
      
      # Correct the aspect ratio if not plotting data
      if (plot_data == F) gg_ad1_map <- aspect_ratio_plot(gg_ad1_map, 4/3, 0.25)
      
      # Set up the final layout ----------------------------------------------------
      
      # Create a title grob
      title_grob <- title_generate(ind_title, year_list, title_grob_size, admin = 1, ctry = ctry)
      
      # Create the overall plot by arranging the grobs
      lay <- plot_overlay(plot_data, multiple_runs)
      if (plot_data == T & multiple_runs == F) master_plot <- arrangeGrob(gg_ad1ts, gg_locator_map, title_grob, gg_ad1_map, layout_matrix = lay)
      if (plot_data == T & multiple_runs == T) master_plot <- arrangeGrob(gg_ad1ts, gg_locator_map, title_grob, gg_ad1_map, ihme_grob_multiple, layout_matrix = lay)
      if (plot_data == F) master_plot <- arrangeGrob(gg_ad1ts, gg_locator_map, title_grob, gg_ad1_map, ihme_grob,
                                                     layout_matrix = lay, heights = c(1,1,1,1,1,0.6))
      grid.draw(master_plot)
      
      if (ctry != sort(countries)[length(sort(countries))]) plot.new()
      
    } # END ` for (ctry in sort(countries))`
    
    dev.off() # For PDF
    
  } #END `if` wrapper for ad1
  
  ################################################################################
  # III. ADMIN-2-LEVEL PLOTS BY COUNTRY ##########################################
  ################################################################################
  
  if ("ad2" %in% plot_levels) {
    
    if (verbose == T) {
      message("\n############################################################")
      message("############################################################")
      message(paste0("Plotting Admin-2-level estimates by country\n"))
      
    }
    # If specified, only map over listed countries
    if (!is.null(ad1_map_countries)){
      message(paste0("You have chosen to map only specified countries:\n",
                     paste(ad1_map_countries, collapse = " ")))
      countries <- ad1_map_countries
    }
    
    # Loop over countries and create a PDF for each country
    for (ctry in sort(countries)) {
      
      # Create subsets of ad1 and ad2 dfs just for this country (for convenience)
      ad0_df_ctry <- subset(ad0_df, ADM0_NAME == ctry)
      ad1_df_ctry <- subset(ad1_df, ADM0_NAME == ctry)
      ad2_df_ctry <- subset(ad2_df, ADM0_NAME == ctry)
      
      # Create a lookup table of ad1s
      ad1_table <- unique(subset(ad1_df_ctry, select = c("ADM1_NAME", "ADM1_CODE")))
      
      # Get the ad0 code for this country
      ad0_code <- unique(ad0_df_ctry$ADM0_CODE)
      
      # Subset maps
      ad0_national <- subset(ad0_shape_simple, ADM0_CODE == ad0_code)
      ad1_national <- subset(ad1_shape_simple, ADM0_CODE == ad0_code)
      ad2_national <- subset(ad2_shape_simple, ADM0_CODE == ad0_code)
      
      if(plot_data == T) {
        ad0_data_ctry <- subset(ad0_data, ADM0_NAME == ctry)
        ad1_data_ctry <- subset(ad1_data, ADM0_NAME == ctry)
        ad2_data_ctry <- subset(ad2_data, ADM0_NAME == ctry)
      }
      
      # Set up PDF filename
      pdf_filename <- paste0(out_dir, sprintf(out_filename_format, paste0(unique(ad0_df_ctry$ihme_lc_id), "_by_admin_2")))
      
      pdf(file = pdf_filename,
          height = 10,
          width = 18)
      
      if(verbose == T) {
        message(paste0("  --> ", ctry, "..."))
        message(paste0("      Writing file to ", pdf_filename))
      }
      
      # Create a lookup table of ad1s
      ad1_table <- unique(subset(ad1_df_ctry, select = c("ADM1_NAME", "ADM1_CODE")))
      
      for (ad1_code in ad1_table$ADM1_CODE) {
        
        # Get ad2 and ad1 tables just for this admin1 code
        # For this loop, ad2_df_ad1 means that it's a data frame of ad2s within the ad1
        # As opposed to ad1_df_ctry (ad1s within the country), etc.
        
        ad2_df_ad1 <- subset(ad2_df_ctry, ADM1_CODE == ad1_code)
        ad1_df_ad1 <- subset(ad1_df_ctry, ADM1_CODE == ad1_code)
        ad1_df_ad1[, plot_name := "ADMIN 1"]
        
        if(plot_data == T) ad2_data_ad1 <- subset(ad2_data_ctry, ADM1_CODE == ad1_code)
        
        if (verbose == T) message(paste0("     --> ", unique(ad1_df_ad1$ADM1_NAME)))
        
        # Create a single df for plotting
        
        # Wrap facet labels & make sure ADMIN 1 comes first
        n_ad2s <- length(unique(ad2_df_ad1$ADM2_CODE))
        
        if (n_ad2s >  0 & n_ad2s <= 16) wrap_width <- 18
        if (n_ad2s > 16 & n_ad2s <= 25) wrap_width <- 12
        if (n_ad2s > 25)                wrap_width <- 9
        
        ad2_df_ad1[, plot_name := stringr::str_wrap(ADM2_NAME, width = wrap_width)]
        plot_df <- rbind(ad1_df_ad1, ad2_df_ad1, fill=T)
        ad2_lvls <- c("ADMIN 1", unique(ad2_df_ad1$plot_name))
        plot_df$plot_name <- factor(plot_df$plot_name, levels = ad2_lvls)
        
        if(plot_data == T) {
          ad2_data_ad1[, plot_name := stringr::str_wrap(ADM2_NAME, width = wrap_width)]
          ad2_data_ad1$plot_name <- factor(ad2_data_ad1$plot_name, levels = ad2_lvls)
        }
        
        # Admin 2 time-series plot -------------------------------------------------
        
        if (plot_data == F & multiple_runs == F) gg_ad2ts <- time_series(plot_df, admin = 2, val_range, title_plot_size, ind_title)
        if (plot_data == T & multiple_runs == F) gg_ad2ts <- time_series_data(plot_df, ad2_data_ad1, admin = 2, val_range, title_plot_size, ind_title)
        if (plot_data == F & multiple_runs == T) gg_ad2ts <- time_series_multiple(plot_df, admin = 2, val_range, title_plot_size, ind_title)
        if (plot_data == T & multiple_runs == T) gg_ad2ts <- time_series_multiple_data(plot_df, ad2_data_ad1, admin = 2, val_range, title_plot_size, ind_title)
        
        # A locator map ------------------------------------------------------------
        
        gg_locator_map <- ggplot() +
          geom_polygon_quiet(data = subset(ad1_national, ADM1_CODE == ad1_code),
                             aes(x = long, y = lat, group = group),
                             fill = "red") +
          geom_path_quiet(data = ad1_national,
                          aes(x=long, y=lat, group=group),
                          size = 0.2) +
          geom_path_quiet(data = ad0_national,
                          aes(x=long, y = lat, group=group),
                          size = 0.5) +
          coord_equal() +
          theme_empty
        
        
        # A labeled map of the admin1 unit by admin2 -----------------------------------------
        ad2_ad1 <- subset(ad2_national, ADM1_CODE == ad1_code)
        
        centroids <- gCentroid(ad2_ad1, byid = T, id = ad2_ad1$ADM2_CODE)
        centroids <- as.data.frame(centroids) %>%
          cbind(rownames(.), .) %>%
          as.data.table(.) %>%
          setnames(., names(.), c("ADM2_CODE", "x", "y"))
        centroids$ADM2_CODE <- as.numeric(levels(centroids$ADM2_CODE))[centroids$ADM2_CODE]
        centroids <- merge(centroids, as.data.table(ad2_ad1), by = "ADM2_CODE")
        
        plot_df <- 
          ad2_df_ad1 %>% filter(year == max(year_list)) %>% dplyr::select(ADM2_CODE, mean)
        
        # Merge and fortify outside of ggplot to speed things up
        if (multiple_runs == T) plot_df <- plot_df %>% distinct() %>% data.table()
        if (multiple_runs == F) ad2_ad1 <- suppressWarnings(merge(ad2_ad1, plot_df, duplicateGeoms = T))
        
        ad2_ad1@data$id = rownames(ad2_ad1@data)
        ad2_ad1_df = fortify(ad2_ad1, region="id")
        ad2_ad1_df <- merge(ad2_ad1_df, ad2_ad1@data, by = "id")
        ad2_ad1_df <- as.data.table(ad2_ad1_df)
        
        if(highisbad) distiller_direction <- -1
        if(!highisbad) distiller_direction <- 1
        
        # Prepare for plotting
        if (multiple_runs == F) gg_ad2_map <- last_year_map(ad2_ad1_df, centroids, admin = 2, val_range, distiller_direction, max(year_list))
        if (multiple_runs == T) gg_ad2_map <- last_year_map_multiple(ad2_ad1_df, centroids, admin = 2, max(year_list))
        
        if (plot_data == F) gg_ad2_map <- aspect_ratio_plot(gg_ad2_map, 4/3, 0.25)
        
        # Set up the final layout ------------------------------------------------------------------
        # Create a title grob
        title_grob <- title_generate(ind_title, year_list, title_grob_size, admin = 2, ad1_df_ad1 = ad1_df_ad1, ctry = ctry)
        
        # Create the overall plot by arranging the grobs
        lay <- plot_overlay(plot_data, multiple_runs)
        if (plot_data == T & multiple_runs == F) master_plot <- arrangeGrob(gg_ad2ts, gg_locator_map, title_grob, gg_ad2_map, layout_matrix = lay)
        if (plot_data == T & multiple_runs == T) master_plot <- arrangeGrob(gg_ad2ts, gg_locator_map, title_grob, gg_ad2_map, ihme_grob_multiple, layout_matrix = lay)
        if (plot_data == F) master_plot <- arrangeGrob(gg_ad2ts, gg_locator_map, title_grob, gg_ad2_map, ihme_grob,
                                                       layout_matrix = lay, heights = c(1,1,1,1,1,0.6))
        
        grid.draw(master_plot)
        
        if (ad1_code != ad1_table$ADM1_CODE[length(ad1_table$ADM1_CODE)]) plot.new()
        
      } #END `for (ad1_code in ad1_table$ADM1_CODE)`
      
      dev.off() # For PDF (by country)
      
    } # END ` for (ctry in sort(countries))`
  } #END `if` wrapper for ad2
}