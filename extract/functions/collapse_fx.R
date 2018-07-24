# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 07/19/2018
# Purpose: Define helper functions for the HAP collapse script
# source("/homes/jfrostad/_code/hap/extract/functions/collapse_fx.R", echo=T)
#***********************************************************************************************************************

# ----Cleaning----------------------------------------------------------------------------------------------------------
#function to do some initial cleaning and prep for the data
initialClean <- function(input.dt, var.fam=this.family, is.point=point, census=ipums) {
  
  message('Subset to relevant variables')
  
  if (var.fam == 'cooking') {
    dt <- input.dt[, .(nid, iso3, lat, long, survey_series, hhweight, urban, cooking_fuel_mapped, hh_size, year_start, shapefile, location_code)]
  } 

  problem_list <- dt[hh_size <=0] #TODO output this list somewhere?
  
  ### Standardize iso3s
  setnames(dt, 'iso3', 'ihme_loc_id')
  dt[, iso3 := substr(ihme_loc_id, 1, 3)]
  
  message('Create a unique cluster id')
  if (is.point) {
    dt[, cluster_id := .GRP, by=.(iso3, lat, long, nid, year_start)]
  } else {
    dt[, cluster_id := .GRP, by=.(iso3, shapefile, location_code, nid, year_start)]
  }
  
  if (is.point) {
    
    message('Change weight to 1 if collapsing point data')
    message('Change shapefile and location code to missing if collapsing point data')
    
    dt[, hhweight := 1]
    dt[, c('shapefile', 'location_code') := NA]
    
  }
  
  #define a row id for other operations and key on it
  dt[, row_id := .I] 
  setkey(dt, row_id) %>%
    return

}

#***********************************************************************************************************************

# ----Definitions-------------------------------------------------------------------------------------------------------
#function to convert extracted variables based on current definitions
defIndicator <- function(dt, definitions=def.file, var.fam=this.family, census=ipums, debug=F) {
  
  #allow for interactive debugs
  if (debug) browser()

  #read in the appropriate definitions file
  def.dt <- read_xlsx(path=definitions, sheet=var.fam) %>% as.data.table
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
    if (nrow(out) != og.count){ 
      
      stop(this.var, ': remap is causing data loss, investigate!!')
      
    } else out[, c('row_id', new.cols), with=F] %>% setkey(., row_id) %>% return #keep only the new vars and ID

  }
  
  maps <- lapply(unique(def.dt$variable), mergeDef, data=dt, defs=def.dt)

  #merge the new mapped vars with the original data (note that they all need to be keyed on row ID)
  Reduce(function(...) merge(..., all = T), list(dt, maps)) %>% 
    return

}

#***********************************************************************************************************************

# ----Missingness-------------------------------------------------------------------------------------------------------
#function to identify missingness in key variables, with option for weighted %
#TODO ask ani why the original function only worked on a few vars for each ind family
idMissing <- function(input.dt, this.var, criteria=.2, wt.var=NA, debug=F) {
  
  #allow for interactive debugs
  if (debug) browser()
  
  #set as copy so you dont save the new vars
  dt <- copy(input.dt)
  
  #set the original row count in order to print the data loss due to missingness
  og.count <- nrow(dt)
  
  #define the weight variable 
  if (is.na(wt.var)) {
    dt[, wt := 1]
  } else dt[, wt := get(wt.var)]

  # Calculate data missingness by cluster for given variable
  dt[, miss := get(this.var) %>% is.na %>% as.numeric]
  #TODO why is hh_size missing so often?
  #TODO missingness seems to be all or none depending by cluster..f/u in raw data
  dt[, pct_miss := sum(miss*wt, na.rm=T)/sum(wt, na.rm=T), by=cluster_id] #calc pct miss, weight by hh_size

  # Return the IDs of any clusters with more than 20% weighted missingness
  message("identified #", dt[pct_miss>criteria, cluster_id] %>% uniqueN, " clusters...or \n~", 
          round((nrow(dt[pct_miss>criteria])/og.count)*100), 
          "% of rows based on criteria of <=", criteria*100, "% missingness of ", this.var)
  
  dt[pct_miss>criteria, cluster_id] %>% 
    unique %>% 
    return
  
}

#***********************************************************************************************************************

# ----xWalk HH Size-----------------------------------------------------------------------------------------------------
#TODO
#***********************************************************************************************************************

# ----Aggregate---------------------------------------------------------------------------------------------------------
#aggregate the given indicator
aggIndicator <- function(input.dt, var.fam=this.family, is.point=point, debug=F) {
  
  #allow for interactive debugs
  if (debug) browser()
  
  #set the variables to work on based on the indicator family
  if (var.fam == 'cooking') {
    these.vars <- 'bin_cooking_fuel_mapped'
  } 
  
  #point data needs to be collapsed using urbanicity
  key.cols <- c('cluster_id', 'nid', 'lat', 'long', 'survey_series', 'year_start', 'shapefile', 'location_code', 
                switch(is.point, NULL, 'urban')) #if points data, add urbanicity to key

  #set as copy so you dont save the new vars
  dt <- copy(input.dt) %>% setkeyv(., key.cols)

  #calculations
  dt[, (these.vars) := lapply(.SD, function(x, wt) sum(x*wt, na.rm=T)/sum(wt, na.rm=T), wt=hhweight*hh_size), 
     .SDcols=these.vars, by=key(dt)] #aggregate each variable and modify in place
  
  dt[, total_hh := sum(hhweight*hh_size)^2/sum(hhweight^2*hh_size), by=key(dt)]
  
  #for polygon data, urbanicity status is no longer meaningful
  if(!is.point) dt[, urban := NA] #TODO what did urbanicity status for poly data originally mean?

  dt %>%  #collapse the dt to unique values based on the key and output
    unique(., by=key(dt)) %>% 
    return

}

#***********************************************************************************************************************

# ----SCRAP-------------------------------------------------------------------------------------------------------------
#first see if we are working on IPUMS
#is.census <- ifelse(dt$survey_series %>% any(.%like%'IPUMS', na.rm=T), T, F)
