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
    if (nrow(out) != og.count){ 
      
      stop(this.var, ': remap is causing data loss, investigate!!')
      
    } else out[, c('row_id', new.cols), with=F] %>% setkey(., row_id) %>% return #keep only the new vars and ID

  }
  
  maps <- lapply(unique(def.dt$variable), mergeDef, data=dt, defs=def.dt)

  #merge the new mapped vars with the original data (note that they all need to be keyed on row ID)
  Reduce(function(...) merge(..., all = T), list(dt, maps)) %>% return

}

#***********************************************************************************************************************

# ----Missingness-------------------------------------------------------------------------------------------------------
#function to identify missingness in key variables, with option for weighted %
#TODO ask ani why the original function only worked on a few vars for each ind family
idMissing <- function(input.dt, this.var, dt_type = data_type, criteria=.2, wt.var=NA, debug=F) {
  
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
  
  dt[pct_miss>criteria, cluster_id] %>% unique %>% return
  
}

#***********************************************************************************************************************

# ----xWalk HH Size-----------------------------------------------------------------------------------------------------
#function to crosswalk hh size to impute missing
cw <- function(data, debug = F, var_family = indi_fam) {
  
  if (debug) {browser()}
  library(dplyr)
  
  # Remove all missing hh_size obs
  data <- filter(data, !is.na(hh_size))
  
  # Duplicate data into reference and comparison sets with dummy encoding
  data_1 <- data %>% mutate(cw = 1)
  
  data_2 <- data %>% mutate(cw = 0)
  data_2$hh_size <- 1
  data_2$id_short <- data_2$id_short + max(data_1$id_short)
  
  data <- rbind(data_1, data_2)
  
  if (var_family == 'water') {
    data <- rename(data, indi = piped) }
  
  if (var_family == 'sani') {
    data <- rename(data, indi = od) }
  
  if (var_family == 'hw') {
    data <- rename(data, indi = hw_station) }
  
  # Aggregate data into clusters
  data <- data %>% mutate(wt_indi = hhweight*indi*hh_size, wt_denom = hhweight*hh_size) %>% 
    group_by(id_short, cw) %>% 
    summarize(wtavg_indi = sum(wt_indi, na.rm = T)/sum(wt_denom, na.rm = T),
              total_hh = sum(hh_size)) 
  
  # Fit a binomial model and get a ratio estimate for crosswalking missing household sizes
  model <- glm(data = data, formula = wtavg_indi ~ cw, family = binomial(link = 'logit'),
               weights = data$total_hh)
  ratio <- model$coefficients['cw']
  ratio <- exp(ratio)
  return(ratio)
}

hh_cw <- function(data, debug = F, var_family = indi_fam, reg, dtype) {
  
  if (debug) {browser()}
  
  if (length(data[which(is.na(data$hh_size)),1]) < 1) {
    print("No missing hh_sizes!")
    return(data)
  } else {
    
    library(dplyr)
    
    # Split data into urban and rural
    urban <- filter(data, urban == 1)
    rural <- filter(data, urban == 0)
    overall <- data
    
    # Obtain urban-rural specific ratios and overal ratios in case U/R is
    # missing
    u_ratio <- cw(urban)
    r_ratio <- cw(rural)
    o_ratio <- cw(overall)
    
    # Plug in ratios into hh_sizes based on urban-rural specificity
    results <- data.frame(urban = c(1,0,2), ratio = c(u_ratio,r_ratio,o_ratio),
                          region  = reg, data_type = dtype, indi_fam = var_family)
    data$hh_size[which(is.na(data$hh_size) &
                         data$urban == 1)] <- u_ratio
    data$hh_size[which(is.na(data$hh_size) &
                         data$urban == 0)] <- r_ratio
    data$hh_size[which(is.na(data$hh_size) &
                         is.na(data$urban))] <- o_ratio
    
    # Print ratios
    print(results)
    return(list(data, results))
    
  }
}

hh_cw_reg <- function(data, var_family = indi_fam, dt = data_type) {
  
  library(dplyr)
  
  #message('Only African Data is currently CWed by reg')
  message('The regs are sssa_hi, cssa, wsssa, name_hi, and essa_hilo')
  sssa_hi <- c('NAM','BWA','ZAF')
  cssa <- c('CAF','GAB','GNQ','COD','COG','AGO','STP')
  name_hi <- c('MAR','DZA','TUN','LBY','EGY')
  essa_hilo <- c('SDN','ERI','DJI','SOM','ETH','SSD',
                 'SSD','UGA','KEN','RWA','BDI','TZA',
                 'MWI','MOZ','ZMB','MDG','ZWE','SWZ','LSO',
                 'COM')
  wssa <- c('CPV','SEN','GMB','GIN','GNB','SLE','MLI','LBR',
            'CIV','GHA','TGO','BEN','NGA','NER','TCD','CMR',
            'BFA','MRT')
  africa <- c(sssa_hi, cssa, name_hi, essa_hilo, wssa)
  
  results <- list()
  ratios <- list()
  
  message('sssa_hi')
  mydat <- filter(data, iso3 %in% sssa_hi)
  if (nrow(mydat)>0) {
    output <- hh_cw(data = mydat, var_family = var_family,
                    reg = 'sssa_hi', dtype = dt)
    results[[1]] <- output[[1]]
    ratios[[1]] <- output[[2]]
  }
  
  message('wssa')
  mydat <- filter(data, iso3 %in% wssa)
  if (nrow(mydat)>0) {
    output <- hh_cw(data = mydat, var_family = var_family,
                    reg = 'wssa', dtype = dt)
    results[[2]] <- output[[1]]
    ratios[[2]] <- output[[2]]
  }
  
  message('cssa')
  mydat <- filter(data, iso3 %in% cssa)
  if (nrow(mydat)>0) {
    output <- hh_cw(data = mydat, var_family = var_family,
                    reg = 'cssa', dtype = dt)
    results[[3]] <- output[[1]]
    ratios[[3]] <- output[[2]]
  }
  
  message('essa_hilo')
  mydat <- filter(data, iso3 %in% essa_hilo)
  if (nrow(mydat)>0) {
    output <- hh_cw(data = mydat, var_family = var_family,
                    reg = 'essa_hilo', dtype = dt)
    results[[4]] <- output[[1]]
    ratios[[4]] <- output[[2]]
  }
  
  message('name_hi')
  mydat <- filter(data, iso3 %in% name_hi)
  if (nrow(mydat)>0) {
    output <- hh_cw(data = mydat, var_family = var_family,
                    reg = 'name_hi', dtype = dt)
    results[[5]] <- output[[1]]
    ratios[[5]] <- output[[2]]
  }
  
  message('non africa')
  mydat <- filter(data, !(iso3 %in% africa))
  if (nrow(mydat)>0) {
    output <- hh_cw(data = mydat, var_family = var_family,
                    reg = 'non africa', dtype = dt)
    results[[6]] <- output[[1]]
    ratios[[6]] <- output[[2]]
  }
  
  results <- do.call(rbind, results)
  ratios <- do.call(rbind, ratios)
  
  original <- try(read.csv('/home/j/WORK/11_geospatial/wash/definitions/hh_size_ratios.csv'),
                  silent = T)
  
  if (class(original) == 'try-error') {
    rm(original)
  }
  
  if (exists('original')) {
    data_present <- unlist(strsplit(unique(as.character(original$data_type)), ','))
    data_present <- gsub(' ', '', data_present)
    
    indi_present <- unlist(strsplit(unique(as.character(original$indi_fam)), ','))
    indi_present <- gsub(' ', '', indi_present)
    
    original <- select(original, -X)
  } else {
    data_present <- ''
    indi_present <- ''
  }
  
  if ((dt %in% data_present) & (var_family %in% indi_present)) {
    write.csv(ratios, '/home/j/WORK/11_geospatial/wash/definitions/hh_size_ratios.csv')
  } else {
    if (data_present != '') {
      ratios <- bind_rows(ratios, original)
      ratios <- filter(ratios, indi_fam == 'sani')
    } else {
      ratios$data_type <- dt
      ratios$indi_fam <- var_family
    }
  }
  
  write.csv(ratios, '/home/j/WORK/11_geospatial/wash/definitions/hh_size_ratios.csv')
  
  return(results)
}

assign_ipums_hh <- function(mydat = ptdat, dt = data_type) {
  current_iso3 <- unique(mydat$iso3)
  
  sssa_hi <- c('NAM','BWA','ZAF')
  cssa <- c('CAF','GAB','GNQ','COD','COG','AGO','STP')
  name_hi <- c('MAR','DZA','TUN','LBY','EGY')
  essa_hilo <- c('SDN','ERI','DJI','SOM','ETH','SSD',
                 'SSD','UGA','KEN','RWA','BDI','TZA',
                 'MWI','MOZ','ZMB','MDG','ZWE','SWZ','LSO',
                 'COM')
  wssa <- c('CPV','SEN','GMB','GIN','GNB','SLE','MLI','LBR',
            'CIV','GHA','TGO','BEN','NGA','NER','TCD','CMR',
            'BFA','MRT')
  africa <- c(sssa_hi, cssa, name_hi, essa_hilo, wssa)
  
  current_reg <- ifelse(current_iso3 %in% sssa_hi, 'sssa_hi',
                        ifelse(current_iso3 %in% cssa, 'cssa',
                               ifelse(current_iso3 %in% name_hi, 'name_hi',
                                      ifelse(current_iso3 %in% essa_hilo, 'essa_hilo',
                                             ifelse(current_iso3 %in% wssa, 'wssa', 'non africa'
                                             )))))
  
  ratios <- read.csv('/home/j/WORK/11_geospatial/wash/definitions/hh_size_ratios.csv',
                     stringsAsFactors = F)
  ratios <- filter(ratios, region == current_reg & data_type == dt)
  
  mydat$hh_size[which(is.na(mydat$hh_size) &
                        mydat$urban == 1)] <- ratios$ratio[which(ratios$urban == 1)]
  mydat$hh_size[which(is.na(mydat$hh_size) &
                        mydat$urban == 0)] <- ratios$ratio[which(ratios$urban == 2)]
  mydat$hh_size[which(is.na(mydat$hh_size) &
                        is.na(mydat$urban))] <- ratios$ratio[which(ratios$urban == 0)]
  
  return(mydat) 
}


#***********************************************************************************************************************

# ----Aggregate---------------------------------------------------------------------------------------------------------
#aggregate the given indicator
agg_indi <- function(input.dt, this.var, var_family = indi_fam, dt_type = data_type, debug=F) {
  
  #allow for interactive debugs
  if (debug) browser()
  
  #set as copy so you dont save the new vars
  dt <- copy(input.dt)

  if (dt_type == 'pt') {

      #TODO verify that these are all necessary
      key.cols <- c('cluster_id', 'nid', 'lat', 'long', 'survey_series', 'urban', 'year_start', 'shapefile', 'location_code')
      setkeyv(dt, key.cols)
      
      dt[, (this.var) := sum(get(this.var)*hhweight*hh_size, na.rm=T)/sum(hhweight*hh_size, na.rm=T), by=key(dt)]
      dt[, total_hh := sum(hhweight*hh_size)^2/sum(hhweight^2*hh_size), by=key(dt)]
      dt <- unique(dt, by=key(dt)) #collapse the dt to unique values based on the key
      
      mydatresults <- mydat %>% mutate(wt_indi = hhweight*indi*hh_size, wt_denom = hhweight*hh_size,
                                       eff_n_num = hhweight*hh_size, eff_n_denom = (hhweight^2)*hh_size) %>% 
        group_by(id_short, nid, iso3, lat, long, survey_series, urban, year_start, shapefile, location_code) %>% 
        summarize(wtavg_indi = sum(wt_indi, na.rm = T)/sum(wt_denom, na.rm = T),
                  total_hh = ((sum(eff_n_num))^2)/sum(eff_n_denom))
  }
    
  if (dt_type == 'poly') {
      
    #TODO verify that these are all necessary
    key.cols <- c('cluster_id', 'nid', 'lat', 'long', 'survey_series', 'year_start', 'shapefile', 'location_code')
    setkeyv(dt, key.cols)
        
    dt[, (this.var) := sum(get(this.var)*hhweight*hh_size, na.rm=T)/sum(hhweight*hh_size, na.rm=T), by=key(dt)]
    dt[, total_hh := sum(hhweight*hh_size)^2/sum(hhweight^2*hh_size), by=key(dt)]
    dt[, urban := NA]
    dt <- unique(dt, by=key(dt)) #collapse the dt to unique values based on the key

  }
  
  message("Merging all results...")

  return(mydat)
  
}

#***********************************************************************************************************************

# ----xxx---------------------------------------------------------------------------------------------------------
