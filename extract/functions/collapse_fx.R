# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 07/19/2018
# Purpose: Define helper functions for the HAP collapse script
# source("/homes/jfrostad/_code/hap/extract/functions/collapse_fx.R", echo=T)
#***********************************************************************************************************************

# ----Cleaning----------------------------------------------------------------------------------------------------------
#function to do some initial cleaning and prep for the data
initialClean <- function(input.dt, var.fam, is.point, this.out.temp=NULL) {
  
  message("\nBegin Initial Cleaning...")
  message('->Subset to relevant variables')

  if (var.fam == 'cooking') {
    dt <- input.dt[, .(nid, iso3, lat, long, survey_series, hhweight, urban, hh_size, 
                       year_start, int_year, shapefile, location_code,
                       cooking_fuel_mapped, cooking_location_mapped, cooking_type_mapped, cooking_type_chimney_mapped)]
  } else if (var.fam == 'housing') {
    dt <- input.dt[, .(nid, iso3, lat, long, survey_series, hhweight, urban, hh_size, 
                       year_start, int_year, shapefile, location_code,
                       housing_roof, housing_wall, housing_floor,
                       housing_roof_num, housing_wall_num, housing_floor_num)]
  } 

  problem_list <- dt[hh_size <=0] #TODO output this list somewhere?
  
  ### Standardize iso3s
  setnames(dt, 'iso3', 'ihme_loc_id')
  dt[, iso3 := substr(ihme_loc_id, 1, 3)]
  
  message('-->Create a unique cluster id')
  if (is.point) {
    dt[, cluster_id := .GRP, by=.(iso3, lat, long, nid, int_year)]
  } else {
    dt[, cluster_id := .GRP, by=.(iso3, shapefile, location_code, nid, int_year)]
  }
  
  if (is.point) {
    
    message('--->Change weight to 1 for point data')
    message('---->Change shapefile/location_code to NA for point data')
    
    dt[, hhweight := 1]
    dt[, c('shapefile', 'location_code') := NA]
    
  }
  
  #output an intermediate file prior to collapse for preliminary analysis
  if (!is.null(this.out.temp)) { 
    message('----->Save raw data to temp folder')
    saveRDS(dt, 
            file=file.path(this.out.temp,
                           var.fam,
                           paste0('uncollapsed_',
                                  ifelse(is.point, 'points', 'poly'), '.RDS')))
  }
  
  #define a row id for other operations and key on it
  dt[, row_id := .I] 
  setkey(dt, row_id) %>%
    return

}

#***********************************************************************************************************************

# ----Definitions-------------------------------------------------------------------------------------------------------
#function to convert extracted variables based on current definitions
defIndicator <- function(dt, var.fam, definitions, debug=F) {
  
  message("\nBegin Indicator Definition...")
  
  #allow for interactive debugs
  if (debug) browser()

  #read in the appropriate definitions file
  def.dt <- read_xlsx(path=definitions, sheet=var.fam) %>% as.data.table
  def.dt[, notes := NULL] #cleanup for reshaping
  
  #impute cooking vars
  #TODO use a more sophisticated imputation process
  if (var.fam == 'cooking') { 
    impute.vars <- c('cooking_location_mapped', 'cooking_type_mapped', 'cooking_type_chimney_mapped')
  }
  
  #define a function to merge on definitions for each variable
  mergeDef <- function(data, defs, this.var, impute.vars=NULL) {

    #set the original row count in order to alert if anything goes wrong in remapping merges
    og.count <- nrow(data)
    
    defs <- defs[variable==this.var] #subset defs to working var

    #reshape the definitions and capture the new variable names for output
    new.cols <- names(defs)[!(names(defs) %in% c('variable', 'value'))]
    defs <- dcast(defs, value~variable, value.var=new.cols, sep='_')
    new.cols <- names(defs)[!(names(defs) %in% c('variable', 'value'))] #account for new var names (added stub)

    #remap
    #coerce to character because sometimes NA vars = logical & this cannot merge to the codebook
    data[, (this.var) := get(this.var) %>% as.character] 
    #TODO workaround because blank/NA vars cant get merged on to my codebook
    if (this.var %in% impute.vars) data[get(this.var) %>% is.na | get(this.var) == "", c(this.var) := 'unknown' ]
    out <- merge(data, defs, by.x=this.var, by.y='value', all.x=T) #merge onto data using original values
    
    #assert that merge was successful
    if (nrow(out) != og.count){ 
      
      stop(this.var, ': remap is causing data loss, investigate!!')
      
    } else out[, c('row_id', new.cols), with=F] %>% setkey(., row_id) %>% return #keep only the new vars and ID

  }
  
  maps <- lapply(unique(def.dt$variable), mergeDef, data=dt, defs=def.dt, impute.vars=impute.vars)

  #merge the new mapped vars with the original data (note that they all need to be keyed on row ID)
  out <- Reduce(function(...) merge(..., all = T), list(dt, maps))
  
  #generate final indicators based on the intermediate vars
  if (var.fam == 'cooking') {
    
    ord.vars <- names(out)[names(out) %like% 'ord_c']
    out[, cooking_risk := rowSums(.SD), .SDcols=ord.vars] #sum the indicators in order to generate aggregated risk
    ind.vars <- c('cooking_clean', 'cooking_med', 'cooking_dirty')
    out[!is.na(cooking_risk), (ind.vars) := 0] #initialize, then fill based on risk
    #TODO investigate assumptions
    out[cooking_risk<6, cooking_clean := 1]
    out[cooking_risk==6, cooking_med := 1]
    out[cooking_risk>6, cooking_dirty := 1]
    
    #cleanup intermediate vars
    remove.vars <- names(out)[names(out) %like% "row_id|mapped"]
    out <- out[, (remove.vars) := NULL]
    
  }
  
  #redefine a row id for other operations and key on it
  out[, row_id := .I] 
  setkey(out, row_id) %>%
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
  message("\nidentified #", dt[pct_miss>criteria, cluster_id] %>% uniqueN, " clusters...or ~", 
          round((nrow(dt[pct_miss>criteria])/og.count)*100), 
          "% of rows \n based on criteria of <=", criteria*100, "% missingness of ", this.var)
  
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
aggIndicator <- function(input.dt, var.fam, is.point, debug=F) {
  
  #allow for interactive debugs
  if (debug) browser()
  
  #set the variables to work on based on the indicator family
  if (var.fam == 'cooking') {
    these.vars <- c('cooking_clean',
                    'cooking_med',
                    'cooking_dirty')
  } 
  
  #point data needs to be collapsed using urbanicity
  key.cols <- c('cluster_id', 'nid', 'lat', 'long', 'survey_series', 'int_year', 'shapefile', 'location_code', 
                switch(is.point, NULL, 'urban')) #if points data, add urbanicity to key

  #set as copy so you dont save the new vars
  dt <- copy(input.dt) %>% setkeyv(., key.cols)

  #calculations
  dt[, (these.vars) := lapply(.SD, function(x, wt) sum(x*wt, na.rm=T)/sum(wt, na.rm=T), wt=hhweight*hh_size), 
     .SDcols=these.vars, by=key(dt)] #aggregate each variable and modify in place
  
  dt[, N := sum(hhweight*hh_size)^2/sum(hhweight^2*hh_size), by=key(dt)]
  dt[, sum_of_sample_weights := sum(hhweight)]
  
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
