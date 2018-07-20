cw_water <- function(mydat) {

	cw_dat <- read.csv('/home/j/WORK/11_geospatial/wash/definitions/cw_water.csv')

	results <- list()
	for (i in unique(mydat$iso3)) {
		message(i)
		cw_sub <- filter(cw_dat, iso3 == i)
		if (nrow(cw_sub) == 0) {
			cw_sub[1,] <- 0
		}
		attach(cw_sub)
		reg <- (sources < 5) |
			   (piped + piped_imp == 0) |
			   (well_imp + well_unimp == 0) |
			   (spring_imp + spring_unimp == 0) 
		detach(cw_sub)

		if (!reg) {
			attach(cw_sub)
			iwell_pct <- well_imp/(well_imp + well_unimp)
			ispring_pct <- spring_imp/(spring_imp + spring_unimp)
			ipiped_pct <- piped_imp/(piped + piped_imp)
			detach(cw_sub)
		} else {
			  # Define regions
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

			  # Assign regions
			  region <- NA
			  region <- ifelse(!(i %in% africa), 'not-africa',
			   			  ifelse(i %in% sssa_hi, 'sssa_hi',
			  			    ifelse(i %in% cssa, 'cssa',
			                  ifelse(i %in% name_hi, 'name_hi',
			   				    ifelse(i %in% essa_hilo, 'essa_hilo',
			  					  ifelse(i %in% wssa, 'wssa', NA
			  					  	))))))

			  cw_reg <- filter(cw_dat, reg == region)
			  cw_reg <- cw_reg %>% 
			  			group_by(reg) %>%
			  			summarize(well_imp = sum(well_imp),
			  					  well_unimp = sum(well_unimp),
			  					  spring_imp = sum(spring_imp),
			  					  spring_unimp = sum(spring_unimp),
			  					  piped = sum(piped),
			  					  piped_imp = sum(piped_imp))
			  attach(cw_reg)
			  iwell_pct <- well_imp/(well_imp + well_unimp)
			  ispring_pct <- spring_imp/(spring_imp + spring_unimp)
			  ipiped_pct <- piped_imp/(piped + piped_imp)
			  detach(cw_reg)

		}
		
		subdat <- mydat %>%
				 filter(iso3 == i)

		if (is.na(ipiped_pct)) {
			ipiped_pct <- 1
			subdat$piped_cw <- 0
		}

		if (is.na(iwell_pct)) {
			iwell_pct <- 1
			subdat$well_cw <- 0
		}

		if (is.na(ispring_pct)) {
			ispring_pct <- 1
			subdat$spring_cw <- 0
		}

		subdat <- subdat %>%
				 mutate(piped = piped + piped_cw * (1 - ipiped_pct),
				 		unimp = unimp + well_unimp + 
				 				(well_cw * (1 - iwell_pct)) +
				 				spring_unimp + (spring_cw * (1 - ispring_pct)),
				 		surface = surface) %>%
				 mutate(imp = piped + (piped_cw * ipiped_pct) +
				 			  well_imp + (well_cw * iwell_pct) +
				 			  spring_imp + (spring_cw * ispring_pct) +
				 			  imp) %>%
				 rename(N = total_hh) %>%
				 select(nid, iso3, survey_series, 
				 		lat, long, shapefile, location_code,
				 		year_start,
				 		N,
				 		piped, imp, unimp, surface)
		results[[length(results) + 1]] <- subdat

	}

	results <- do.call(rbind, results)

	return(results)
}

cw_sani <- function(mydat) {

	cw_dat <- read.csv('/home/j/WORK/11_geospatial/wash/definitions/cw_sani.csv')

	results <- list()
	for (i in unique(mydat$iso3)) {
		message(i)
		cw_sub <- filter(cw_dat, iso3 == i)
		if (nrow(cw_sub) == 0) {
			cw_sub[1,] <- 0
		}
		attach(cw_sub)
		reg <- (sources < 5) |
			   (latrine_imp + latrine_unimp == 0) |
			   (flush_imp + flush_unimp == 0)
		detach(cw_sub)

		if (!reg) {
			attach(cw_sub)
			ilatrine_pct <- latrine_imp/(latrine_imp + latrine_unimp)
			iflush_pct <- flush_imp/(flush_imp + flush_unimp)
			detach(cw_sub)
		} else {
			  # Define regions
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

			  # Assign regions
			  region <- NA
			  region <- ifelse(!(i %in% africa), 'not-africa',
			   			  ifelse(i %in% sssa_hi, 'sssa_hi',
			  			    ifelse(i %in% cssa, 'cssa',
			                  ifelse(i %in% name_hi, 'name_hi',
			   				    ifelse(i %in% essa_hilo, 'essa_hilo',
			  					  ifelse(i %in% wssa, 'wssa', NA
			  					  	))))))

			  cw_reg <- filter(cw_dat, reg == region)
			  cw_reg <- cw_reg %>% 
			  			group_by(reg) %>%
			  			summarize(latrine_imp = sum(latrine_imp),
			  					  latrine_unimp = sum(latrine_unimp),
			  					  flush_imp = sum(flush_imp),
			  					  flush_unimp = sum(flush_unimp))
			  attach(cw_reg)
   			  ilatrine_pct <- latrine_imp/(latrine_imp + latrine_unimp)
			  iflush_pct <- flush_imp/(flush_imp + flush_unimp)
			  detach(cw_reg)

		}
		

		subdat <- mydat %>%
				 filter(iso3 == i)

		if (is.na(ilatrine_pct)) {
			ilatrine_pct <- 1
			subdat$latrine_cw <- 0
		}

		if (is.na(iflush_pct)) {
			iflush_pct <- 1
			subdat$flush_cw <- 0
		}

		subdat <- subdat %>%
				 mutate(imp = imp + 
				 			  (ilatrine_pct * latrine_cw) + latrine_imp +
				 			  (iflush_pct * flush_cw) + flush_imp,
				 		unimp = unimp + 
				 				(latrine_cw * (1 - ilatrine_pct)) + latrine_unimp +
				 				(flush_cw * (1 - iflush_pct)) + flush_unimp,
				 		od = od) %>%
				 rename(N = total_hh) %>%
				 select(nid, iso3, survey_series, 
				 		lat, long, shapefile, location_code,
				 		year_start,
				 		N,
				 		imp, unimp, od)
		results[[length(results) + 1]] <- subdat

	}

	results <- do.call(rbind, results)

	return(results)
}