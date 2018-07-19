# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 07/19/2018
# Purpose: Define helper functions for the HAP collapse script
# source("/homes/jfrostad/_code/hap/extract/functions/collapse_fx.R", echo=T)
#***********************************************************************************************************************

# ----Cleaning----------------------------------------------------------------------------------------------------------
#function to do some initial cleaning and prep for the data
initialCleaning <- function(dt, var_family = indi_fam, dat_type = data_type) {
  
  #first see if we are working on IPUMS
  census <- ifelse(dt$survey_series %>% any(.%like%'IPUMS', na.rm=T), T, F)
  
  message('Subset to relevant variables')
  if (var_family == 'cooking') {
    dt <- dt[, .(nid, iso3, lat, long, survey_series, hhweight, urban, cooking_fuel_mapped, hh_size, year_start, shapefile, location_code)]
  } 

  if (var_family == 'heating') {
    if (census) {
      #TODO update with the correct vars for this fam
      ptdat_0 <- dplyr::select(mydat, nid, iso3, lat, long, survey_series, hhweight, urban, 
                             t_type, shared_san, hh_size, year_start,
                             shapefile,location_code,sewage)  
    } else {
      #TODO update with the correct vars for this fam
      ptdat_0 <- dplyr::select(mydat, nid, iso3, lat, long, survey_series, hhweight, urban, 
                             t_type, shared_san, hh_size, year_start,
                             shapefile,location_code)

    }
  }

  if (var_family == 'housing') {
    #TODO update with the correct vars for this fam
    ptdat_0 <- dplyr::select(mydat, nid, iso3, lat, long, survey_series, hhweight, urban, 
                             hw_station, hw_soap, hw_water, hh_size, year_start,
                             shapefile,location_code)

  }

  problem_list <- dt[hh_size <=0] #TODO output this list somewhere?
  
  ### Standardize iso3s
  setnames(dt, 'iso3', 'ihme_loc_id')
  dt[, iso3 := substr(ihme_loc_id, 1, 3)]
  
  message('Create a unique cluster id')
  if (dat_type == 'pt') {
    dt[, cluster_id := .GRP, by=.(iso3, lat, long, nid, year_start)]
  } else {
    dt[, cluster_id := .GRP, by=.(iso3, shapefile, location_code, nid, year_start)]
  }
  
  #define a row id for other operations and key on it
  dt[, row_id := .I] 
  setkey(dt, row_id)

  if (dat_type == "pt") {
    
    message('Change weight to 1 if collapsing point data')
    message('Change shapefile and location code to missing if collapsing point data')
    
    dt[, hhweight := 1]
    dt[, c('shapefile', 'location_code') := NA]
    
  }

  return(dt)
  
}

#***********************************************************************************************************************

# ----Definitions-------------------------------------------------------------------------------------------------------
#function to convert extracted variables based on current definitions
defIndicator <- function(dt, definitions=def.file, var_family = indi_fam, debug = F) {
  
  #allow for interactive debugs
  if (debug) browser()
  
  #first see if we are working on IPUMS
  census <- ifelse(dt$survey_series %>% any(.%like%'IPUMS', na.rm=T), T, F)
  
  #read in the appropriate definitions file
  def.dt <- read_xlsx(path=definitions, sheet=var_family) %>% as.data.table
  def.dt[, notes := NULL] #cleanup for reshaping
  
  #define a function to merge on definitions for each variable
  mergeDef <- function(data, defs, this.var) {

    #set the original row count in order to alert if anything goes wrong in remapping merges
    og.count <- nrow(data)
    
    defs <- defs[variable==this.var] #subset defs to working var
    
    #reshape the definitions and capture the new variable names for output
    defs <- dcast(defs, value~variable, value.var=c('bin', 'ord'), sep='_')
    new.cols <- names(defs)[!(names(defs) %like% 'value')]
    
    #remap
    data[get(this.var)=="", c(this.var) := 'unknown' ] #TODO are blank responses (as opposed to NA) unknown?
    out <- merge(data, defs, by.x=this.var, by.y='value', all.x=T) #merge onto data using original values
    
    #assert that merge was successful
    if(nrow(out) != og.count){ 
      stop(this.var, ': remap is causing data loss, investigate!!')
    } else out[, c('row_id', new.cols), with=F] %>% setkey(., row_id) %>% return #keep only the new variables and the identifying row info

  }
  
  maps <- lapply(unique(def.dt$variable), mergeDef, data=dt, defs=def.dt)

  merged <- Reduce(function(...) merge(..., all = T), list(dt, maps))
  
  if (var_family == 'cooking') {
    
    #right now we are only doing binary solid fuel use
    
    
  }
  
  return(dt)
  
}
