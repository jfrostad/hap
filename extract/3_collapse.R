# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 06/12/2018
# Purpose: Prepare CGF data for the CIAF project
# source("/home/j/temp/jfrostad/ciaf/code/prep.R", echo=T)
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j/"
  h_root <- "/homes/jfrostad/"
  arg <- commandArgs()[-(1:3)] # First args are for unix use only
  
  if (length(arg)==0) {
    # arg <- c("IND", #current project iteration
    #          "8", #output version
    #          1) #number of cores provided to multicore functions
  }
  
  package_lib    <- sprintf('%s_code/_lib/pkg',h_root)
  ## Load libraries and  MBG project functions.
  .libPaths(package_lib)
  
  # necessary to set this option in order to read in a non-english character shapefile on a linux system (cluster)
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
} else {
  j_root <- "J:"
  h_root <- "H:"
  # arg <- c("IND", #current project iteration
  #          "4", #output version
  #          1) #number of cores provided to multicore functions
}

#load packages
pacman::p_load(data.table, dplyr, feather)

repo <- file.path(h_root, '_code/lbd/hap/extract')

#options
date <- "2018_07_17"

file.types <- c('poly', 'pt', 'ipums')
file.types <- c('poly', 'pt')

all.indicators <- c('cooking_fuel_mapped')

#### Load functions ####
for (file_type in file.types){
  message(paste("Loading",file_type, "data"))
  rm(pt_collapse)
  message('Loading Data...')
  # Load data
  if (!("pt_collapse" %in% ls()) & file_type == 'pt') {
    pt_collapse <- read_feather(paste0(j_root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/points_', date, ".feather")) %>% as.data.table
    # Encoding(pt_collapse$w_source_drink) <- "UTF-8"
    # Encoding(pt_collapse$w_source_other) <- "UTF-8"
    # Encoding(pt_collapse$t_type) <- "UTF-8"
    # pt_collapse$w_source_drink <- tolower(pt_collapse$w_source_drink)
    # pt_collapse$w_source_other <- tolower(pt_collapse$w_source_other)
    # pt_collapse$t_type <- tolower(pt_collapse$t_type)
    data_type <- 'pt'
  } 
    
  if (!("pt_collapse" %in% ls()) & file_type == 'poly') {
    pt_collapse <- read_feather(paste0(j_root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/poly_', date, ".feather")) %>% as.data.table
    # Encoding(pt_collapse$w_source_drink) <- "UTF-8"
    # Encoding(pt_collapse$w_source_other) <- "UTF-8"
    # Encoding(pt_collapse$t_type) <- "UTF-8"
    # pt_collapse$w_source_drink <- tolower(pt_collapse$w_source_drink)
    # pt_collapse$w_source_other <- tolower(pt_collapse$w_source_other)
    # pt_collapse$t_type <- tolower(pt_collapse$t_type)
     data_type <- 'poly'
  }

  if (file_type == 'ipums') {
    ipums_dir <- '/home/j/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/IPUMS_feathers'
    files <- list.files(ipums_dir, '.feather')
    files_length <- length(files)
    indicators <- all.indicators
  } else {
    files <- list(pt_collapse)
    files_length <- length(files)
    indicators <- all.indicators
  }

    
    ### Standardize iso3s
    setnames(pt_collapse, 'iso3', 'ihme_loc_id')
    pt_collapse[, iso3 := substr(pt_collapse$iso3, 1, 3)]
    
    for (indi_fam in indicators) {
    rm(definitions)
    message(paste('Processing:', indi_fam))

        message(paste("Collapsing ", indi_fam))

        message("Importing functions...")
        setwd(repo)
        source('functions/initial_cleaning.R')
        source('functions/hh_cw.R')
        source('functions/address_missing.R')
        source('functions/cw_indi.R')
        source('functions/agg_wash.R')
        source('functions/define_wash.R')
        source('functions/write_cw.R')

        #### Subset & Shape Data ####
        message("Initial Cleaning...")
        temp_list <- initial_cleaning(census = T)
        ptdat <- temp_list[[1]]; ptdat_0 <- temp_list[[2]]
        rm(temp_list)

        #### Define Indicator ####
        message("Defining Indicator...")
        ptdat <- define_indi(sdg_indi = T, census = ipums)

        #### Address Missingness ####
        message("Addressing Missingness...")
        
        # Remove clusters with more than 20% weighted missingness
        ptdat <- rm_miss()
        if (nrow(ptdat) == 0) {
          next
        }

        # Remove cluster_ids with missing hhweight or invalid hhs
        miss_wts <- unique(ptdat$id_short[which(is.na(ptdat$hhweight))])
        ptdat <- filter(ptdat, !(id_short %in% miss_wts))
        ptdat <- filter(ptdat, hhweight != 0)

        invalid_hhs <- unique(ptdat$id_short[which(ptdat$hh_size <= 0)])
        ptdat <- filter(ptdat, !(id_short %in% invalid_hhs))

        if (nrow(ptdat) == 0) {
          next
        }

        # Crosswalk missing household size data
        message("Crosswalking HH Sizes...")
        if (!ipums) {
          ptdat <- hh_cw_reg(data = ptdat)
        } else {
          ptdat <- assign_ipums_hh()
        }
        
        # Remove missing observations
        ptdat <- filter(ptdat, !is.na(imp))

        if (nrow(ptdat) == 0) {
          next
        }

        #### Aggregate Data ####
        # Bookmarking dataset so it can be looped over for conditional switch
        ptdat_preagg <- ptdat
        
        # Conditional switch is to switch collapsing for conditional vs unconditional indicators
        conditional <- 'unconditional'

        # Reseting the dataset to preagregate
        ptdat <- ptdat_preagg
        message(paste("Conditional variables status:",conditional))
       
        # Aggregate indicator to cluster level
        message("Aggregating Data...")
        ptdat <- agg_indi()

        # Skip the rest of the process if no rows of data are left
        if (nrow(ptdat) == 0) {
          next
        }

        # Delete cw dictionary if ipums already added to it previously
        if (index == 1 & ipums) {
          message('Checking if IPUMS already added to CW data previously')
          original <- try(read.csv('/home/j/WORK/11_geospatial/wash/definitions/cw_sani.csv', stringsAsFactors = F),
                        silent = T)
        
          if (class(original) == 'try-error') {
            rm(original)
          }  else {
            if ('ipums' %in% original$data_type) {
             system('rm /home/j/WORK/11_geospatial/wash/definitions/cw_sani.csv')
            } 
            rm(original)
          }  
        }
        
        # Write crosswalking dictionary
        message('Output CW files')    
        write_cw_ratio(census = ipums)

        #save poly and point collapses
        message("Saving Collapsed Data...")
        today <- gsub("-", "_", Sys.Date())
        
        if (!ipums) {
          if (data_type == "poly") {
            polydat <- ptdat
            rm(ptdat)
            write_feather(polydat, paste0(j_root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_",
                          indi_fam, '_', conditional, '_', today, ".feather"))
          } else{
            write_feather(ptdat, paste0(j_root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/ptdat_",
                          indi_fam, '_', conditional, '_', today, ".feather"))
          }
        }
        
        if (ipums) {
          write_feather(ptdat, paste0(j_root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/IPUMS/feather/",
                          indi_fam, '_', conditional, '_', today, '_', files[index]))
        }
        
      }
    }
  }
}