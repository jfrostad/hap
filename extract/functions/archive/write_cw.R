# This functions outputs crosswalking data for indicator crosswalking

write_cw_ratio <- function(mydat = ptdat, dt = data_type, census = ipums,
						   var_family = indi_fam) {
  
  if (census) {
  	dt <- 'ipums'
  }
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
  mydat$reg <- NA
  mydat$reg <- ifelse(!(mydat$iso3 %in% africa), 'not-africa', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% sssa_hi, 'sssa_hi', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% cssa, 'cssa', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% name_hi, 'name_hi', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% essa_hilo, 'essa_hilo', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% wssa, 'wssa', mydat$reg)

  # Summarize number of observations by each indicator level and write to cw .csv
  if (var_family == 'sani') {

  	  # Collapse data to number of observations
	  mydat <- mydat %>%
		  		   mutate(imp = imp * total_hh,
		  		   		  unimp = unimp * total_hh,
		  		   		  od = od * total_hh,
		  		   		  latrine_imp = latrine_imp * total_hh,
		  		   		  latrine_unimp = latrine_unimp * total_hh,
		  		   		  latrine_cw = latrine_cw * total_hh,
		  		   		  flush_imp = flush_imp * total_hh,
		  		   		  flush_unimp = flush_unimp * total_hh,
		  		   		  flush_cw = flush_cw * total_hh) %>%
		  		   group_by(iso3, reg) %>% 
		  		   summarize(N = sum(total_hh),
		  		   			 imp = sum(imp),
		  		   			 unimp = sum(unimp),
		  		   		  	 od = sum(od),
		  		   		  	 latrine_imp = sum(latrine_imp),
		  		   		  	 latrine_unimp = sum(latrine_unimp),
		  		   		  	 latrine_cw = sum(latrine_cw),
		  		   		  	 flush_imp = sum(flush_imp),
		  		   		  	 flush_unimp = sum(flush_unimp),
		  		   		  	 flush_cw = sum(flush_cw),
		  		   		  	 sources = length(unique(nid)),
		  		   		  	 data = dt)

	# Read in original cw file if it exists
	original <- try(read.csv('/home/j/WORK/11_geospatial/wash/definitions/cw_sani.csv', stringsAsFactors = F),
					silent = T)
	
	if (class(original) == 'try-error') {
		rm(original)
	}

	# Create indicator variable indicating data sources represented in the cw csv
	if (exists('original')) {
		data_present <- unlist(strsplit(unique(as.character(original$data)), ','))
		data_present <- unique(gsub(' ', '', data_present))
		original <- select(original, -X)
	} else {
		data_present <- ''
	}
				
	
	# if current data type is in the cw csv overwrite the csv with fresh run
	if ((dt %in% data_present) & dt != 'ipums') {
		write.csv(mydat, '/home/j/WORK/11_geospatial/wash/definitions/cw_sani.csv')
	} else {

		# If it current data type isnt represented rbind and re-collapse
		# the dataset to update counts
		# This is if csv doesn't exist then the first write is generated
		# with the current data type being processed
		if (data_present != '') {
			mydat <- bind_rows(mydat, original)
			if (dt != 'ipums') {
				mydat$data <- paste(dt, ',', paste(data_present, collapse = ', '))
			} else {
				if (dt %in% data_present) {
				  mydat$data <- paste(data_present, collapse = ', ')
				} else {
					mydat$data <- paste(dt, ',', paste(data_present, collapse = ', '))
				}
			}
		} else {
			mydat$data <- dt
		}

		mydat <- mydat %>%
    	  		 group_by(iso3, reg, data) %>% 
	  		     summarize(N = sum(N),
	  		   	     	   imp = sum(imp),
	  		   			   unimp = sum(unimp),
	  		   		  	   od = sum(od),
	  		   		  	   latrine_imp = sum(latrine_imp),
	  		   		  	   latrine_unimp = sum(latrine_unimp),
	  		   		  	   latrine_cw = sum(latrine_cw),
	  		   		  	   flush_imp = sum(flush_imp),
	  		   		  	   flush_unimp = sum(flush_unimp),
	  		   		  	   flush_cw = sum(flush_cw),
	  		   		  	   sources = sum(sources))

	  	write.csv(mydat, '/home/j/WORK/11_geospatial/wash/definitions/cw_sani.csv')

	}

  }

  if (var_family == 'water') {
	  mydat <- mydat %>%
		  		   mutate(imp = imp * total_hh,
		  		   		  unimp = unimp * total_hh,
		  		   		  surface = surface * total_hh,
		  		   		  spring_imp = spring_imp * total_hh,
		  		   		  spring_unimp = spring_unimp * total_hh,
		  		   		  spring_cw = spring_cw * total_hh,
		  		   		  well_imp = well_imp * total_hh,
		  		   		  well_unimp = well_unimp * total_hh,
		  		   		  well_cw = well_cw * total_hh,
		  		   		  piped_imp = piped_imp * total_hh,
		  		   		  piped = piped * total_hh,
		  		   		  piped_cw = piped_cw * total_hh) %>%
		  		   group_by(iso3, reg) %>% 
		  		   summarize(N = sum(total_hh),
		  		   			 imp = sum(imp),
			  		   		 unimp = sum(unimp),
			  		   		 surface = sum(surface),
			  		   		 spring_imp = sum(spring_imp),
			  		   		 spring_unimp = sum(spring_unimp),
			  		   		 spring_cw = sum(spring_cw),
			  		   		 well_imp = sum(well_imp),
			  		   		 well_unimp = sum(well_unimp),
			  		   		 well_cw = sum(well_cw),
			  		   		 piped_imp = sum(piped_imp),
			  		   		 piped = sum(piped),
			  		   		 piped_cw = sum(piped_cw),
		  		   		  	 sources = length(unique(nid)),
		  		   		  	 data = dt)
	
	# Read in original cw file if it exists
	original <- try(read.csv('/home/j/WORK/11_geospatial/wash/definitions/cw_water.csv'),
					silent = T)
	
	if (class(original) == 'try-error') {
		rm(original)
	}
	
	# Create indicator variable indicating data sources represented in the cw csv
	if (exists('original')) {
		data_present <- unlist(strsplit(unique(as.character(original$data)), ','))
		data_present <- gsub(' ', '', data_present)
		original <- select(original, -X)
	} else {
		data_present <- ''
	}
				
	
	# if current data type is in the cw csv overwrite the csv with fresh run
	if ((dt %in% data_present) & dt != 'ipums') {
		write.csv(mydat, '/home/j/WORK/11_geospatial/wash/definitions/cw_water.csv')
	} else {

		# If it current data type isnt represented rbind and re-collapse
		# the dataset to update counts
		# This is if csv doesn't exist then the first write is generated
		# with the current data type being processed
		if (data_present != '') {
			mydat <- bind_rows(mydat, original)
			if (dt != 'ipums') {
				mydat$data <- paste(dt, ',', paste(data_present, collapse = ', '))
			} else {
				if (dt %in% data_present) {
				  mydat$data <- paste(data_present, collapse = ', ')
				} else {
					mydat$data <- paste(dt, ',', paste(data_present, collapse = ', '))
				}
			}
		} else {
			mydat$data <- dt
		}

		mydat <- mydat %>%
    	  		 group_by(iso3, reg, data) %>% 
	  		     summarize(N = sum(N),
	  		   			   imp = sum(imp),
		  		   		   unimp = sum(unimp),
		  		   		   surface = sum(surface),
		  		   		   spring_imp = sum(spring_imp),
		  		   		   spring_unimp = sum(spring_unimp),
		  		   		   spring_cw = sum(spring_cw),
		  		   		   well_imp = sum(well_imp),
		  		   		   well_unimp = sum(well_unimp),
		  		   		   well_cw = sum(well_cw),
		  		   		   piped_imp = sum(piped_imp),
		  		   		   piped = sum(piped),
		  		   		   piped_cw = sum(piped_cw),
	  		   		  	   sources = sum(sources))

	  	write.csv(mydat, '/home/j/WORK/11_geospatial/wash/definitions/cw_water.csv')

		}

	}
 
	return(mydat)
}