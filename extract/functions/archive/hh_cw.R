######################################################
##### HOUSEHOLD SIZE CROSSWALKING CODE ###############

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