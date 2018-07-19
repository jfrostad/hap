initialCleaning <- function(dt, var_family = indi_fam, dat_type = data_type) {
  
  #first see if we are working on IPUMS
  census <- ifelse(dt$survey_series %>% any(.%like%'IPUMS', na.rm=T), T, F)
  
  message('Subset to relevant variables')
  if (var_family == 'cooking') {
    dt <- dt[, .(nid, iso3, lat, long, survey_series, hhweight, urban, cooking_fuel_mapped, hh_size, year_start, hhweight, shapefile, location_code)]
  } 

  if (var_family == 'heating') {
    if (census) {
      #TODO update with the correct vars for this fam
      ptdat_0 <- dplyr::select(mydat, nid, iso3, lat, long, survey_series, hhweight, urban, 
                             t_type, shared_san, hh_size, year_start,hhweight,
                             shapefile,location_code,sewage)  
    } else {
      ptdat_0 <- dplyr::select(mydat, nid, iso3, lat, long, survey_series, hhweight, urban, 
                             t_type, shared_san, hh_size, year_start,hhweight,
                             shapefile,location_code)

    }
  }

  if (var_family == 'housing') {
    #TODO update with the correct vars for this fam
    ptdat_0 <- dplyr::select(mydat, nid, iso3, lat, long, survey_series, hhweight, urban, 
                             hw_station, hw_soap, hw_water, hh_size, year_start,hhweight,
                             shapefile,location_code)

  }

  problem_list <- dt[hh_size <=0] #TODO output this list somewhere?
  
  ### Standardize iso3s
  setnames(dt, 'iso3', 'ihme_loc_id')
  dt[, iso3 := substr(ihme_loc_id, 1, 3)]
  
  message('Create a unique cluster id')
  if (dat_type == 'pt') {
    dt[, id := .GRP, by=.(iso3, lat, long, nid, year_start)]
  } else {
    dt[, id := .GRP, by=.(iso3, shapefile, location_code, nid, year_start)]
  }

  message('Change weight to 1 if collapsing point data')
  if (dat_type == "pt") dt[, hhweight := 1]

  message('Change shapefile and location code to missing if collapsing point data')
  if (dat_type == "pt") dt[, c('shapefile', 'location_code') := NA]

  return(dt)
}

