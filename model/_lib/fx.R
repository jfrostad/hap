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