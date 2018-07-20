
rm_miss <- function(mydat = ptdat, var_family = indi_fam, agg = agg_level, dt_type = data_type) {
  
  if (var_family == 'water') {
    mydat <- rename(mydat, indi = piped)
  }
  
  if (var_family == 'sani') {
    mydat <- rename(mydat, indi = od)
  }

  if (var_family == 'hw') {
    mydat <- rename(mydat, indi = hw_station)
  }
  
  # Calculate data missingness by cluster
  missing <- mutate(mydat, miss = ifelse(is.na(mydat$indi), 1, 0))
  missing <- mutate(missing, miss_wt = miss*hh_size)
  missing <- missing %>% group_by(id_short) %>% summarise(pct_miss = sum(miss_wt)/sum(hh_size))
  # Remove clusters with more than 20% weighted missingness
  miss_clusters <- dplyr::select(filter(missing, pct_miss > 0.2), id_short)
  mydat <- filter(mydat, !(id_short %in% miss_clusters$id_short))
  
  if(agg == 'country' & dt_type == 'pt') {
    # Calculate weight missingness by cluster for country agg & pts
    missing <- mutate(mydat, miss = ifelse(is.na(mydat$hhweight), 1, 0))
    missing <- mutate(missing, miss_wt = miss*hh_size)
    missing <- missing %>% group_by(id_short) %>% summarise(pct_miss = sum(miss_wt)/sum(hh_size))
  
    miss_clusters <- select(filter(missing, pct_miss > 0.2), id_short)
    mydat <- filter(mydat, !(id_short %in% miss_clusters$id_short))
  }
  
  if (var_family == 'water') {
    mydat <- rename(mydat, piped = indi)
  }
  
  if (var_family == 'sani') {
    mydat <- rename(mydat, od = indi)
  }
 
  if (var_family == 'hw') {
    mydat <- rename(mydat, hw_station = indi)
  }
  return(mydat)
}
  
  
impute_indi <- function(mydat = ptdat, var_family = indi_fam) {
  
  if (var_family == 'water') {

  # mdg way  
  #levels <- c('piped', 'surface','imp','unimp','bottled','bottled_sp','bottled_wl','well_cw',
  #           'well_imp','well_unimp','spring_cw','spring_imp','spring_unimp')

  # sdg way
  levels <- c('piped', 'surface','imp','unimp','well_cw','well_imp','well_unimp',
              'spring_cw','spring_imp','spring_unimp')
  }
  
  if (var_family == 'sani') {
    levels <- c('imp','unimp','od','latrine_cw','latrine_imp','latrine_unimp','shared')
  }
  
  if (var_family == 'hw') {
    levels <- c('hw_station','hw_unimp','hw_basic')
  }

  for (i in levels) {
    message(paste("Imputing",i))
    names(mydat)[which(names(mydat) == i)] <- 'indi'
    
    # Calculated household size weighted means for all clusters
    wtavg <-  mydat %>% mutate(wt_indi = hhweight*indi*hh_size, wt_denom = hhweight*hh_size) %>% group_by(id_short) %>% 
      summarize(wtavg_indi = sum(wt_indi, na.rm = T)/sum(wt_denom, na.rm = T))
    
    # Assign observations with NA indicator value the weighted average for the cluster
    mydat <- left_join(mydat, wtavg, by = "id_short")
    mydat$indi <- ifelse(is.na(mydat$indi), mydat$wtavg_indi, mydat$indi)
    mydat <- dplyr::select(mydat, -wtavg_indi)
    
    names(mydat)[which(names(mydat) == 'indi')] <- i
    
  }
  
  return(mydat)
  
}

### UNNECESSARY CODE BELOW ###

impute_indi_reg <- function(data, var_family = indi_fam) {

  library(dplyr)

  message('Only African Data is currently CWed by reg')
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
  results <- list()
  
  message('sssa_hi')
  mydat <- filter(data, iso3 %in% sssa_hi)
  if (nrow(mydat)>0) {
    results[[1]] <- impute_indi(mydat = mydat, var_family = var_family)
  }

  message('wssa')
  mydat <- filter(data, iso3 %in% wssa)
  if (nrow(mydat)>0) {
    results[[2]] <- impute_indi(mydat = mydat, var_family = var_family)
  }

  message('cssa')
  mydat <- filter(data, iso3 %in% cssa)
  if (nrow(mydat)>0) {
    results[[3]] <- impute_indi(mydat = mydat, var_family = var_family)
  }

  message('essa_hilo')
  mydat <- filter(data, iso3 %in% essa_hilo)
  if (nrow(mydat)>0) {
    results[[4]] <- impute_indi(mydat = mydat, var_family = var_family)
  }

  message('name_hi')
  mydat <- filter(data, iso3 %in% name_hi)
  if (nrow(mydat)>0) {
    results[[5]] <- impute_indi(mydat = mydat, var_family = var_family)
  }
  
  results <- do.call(rbind, results)
  return(results)
}

impute_indi_reg_time <- function(data = ptdat) {
  message('The periods are pre-2000,00-04,05-09,10-15')

  period1 <- 2000:2004
  period2 <- 2005:2009
  results <- list()

  library(dplyr)
  message('Period: pre-2000')
  mydat <- filter(data, year_start < 2000)
  if (nrow(mydat)>0) {
    results[[1]] <- impute_indi_reg(mydat)
  }

  message('Period: 00-04')
  mydat <- filter(data, year_start <= 20004 & year_start >= 2000)
  if (nrow(mydat)>0) {
    results[[2]] <- impute_indi_reg(mydat)
  }

  message('Period: 05-09')
  mydat <- filter(data, year_start <= 2009 & year_start >= 2005)
  if (nrow(mydat)>0) {
    results[[3]] <- impute_indi_reg(mydat)
  }

  message('Period: 10-15')
  mydat <- filter(data, year_start >= 2010)
  if (nrow(mydat)>0) {
    results[[4]] <- impute_indi_reg(mydat)
  }

  results <- do.call(rbind, results)
  return(results)
}