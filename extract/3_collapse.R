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
pacman::p_load(data.table, dplyr, feather, readxl)

#options
date <- "2018_07_17"

file.types <- c('poly', 'pt', 'ipums')
file.types <- c('poly', 'pt')

all.indicators <- c('cooking')
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path(j_root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/')
doc.dir <- file.path(j_root, 'WORK/11_geospatial/hap/documentation')
def.file <- file.path(doc.dir, 'definitions.xlsx')

###Output###

#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#general functions#
central.function.dir <- file.path(h_root, "_code/_lib/functions")
# this pulls the general misc helper functions
file.path(central.function.dir, "misc.R") %>% source
#hap functions#
hap.function.dir <- file.path(h_root, '_code/hap/extract/functions')
#this pulls hap collapse helper functions
file.path(hap.function.dir, '/collapse_fx.R') %>% source
#shared functions#
shared.function.dir <- file.path(j_root,  "temp/central_comp/libraries/current/r")
file.path(shared.function.dir, 'get_location_metadata.R') %>% source
file.path(shared.function.dir, 'get_ids.R') %>% source
file.path(shared.function.dir, 'get_covariate_estimates.R') %>% source

#***********************************************************************************************************************

# ---COLLAPSE-----------------------------------------------------------------------------------------------------------

for (file_type in file.types){
  message(paste("Loading",file_type, "data"))

  # Load data
  if (!("pt_collapse" %in% ls()) & file_type == 'pt') {
    raw <- paste0(data.dir, 'points_', date, ".feather") %>% read_feather %>% as.data.table
    # Encoding(pt_collapse$w_source_drink) <- "UTF-8"
    # Encoding(pt_collapse$w_source_other) <- "UTF-8"
    # Encoding(pt_collapse$t_type) <- "UTF-8"
    # pt_collapse$w_source_drink <- tolower(pt_collapse$w_source_drink)
    # pt_collapse$w_source_other <- tolower(pt_collapse$w_source_other)
    # pt_collapse$t_type <- tolower(pt_collapse$t_type)
    data_type <- 'pt'
  } 
    
  if (!("pt_collapse" %in% ls()) & file_type == 'poly') {
    raw <- paste0(data.dir, 'poly_', date, ".feather") %>% read_feather %>% as.data.table
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

    for (indi_fam in indicators) {

      message(paste('Processing:', indi_fam))

        message(paste("Collapsing ", indi_fam))

        #### Subset & Shape Data ####
        message("Initial Cleaning...")
        ptdat <- initialCleaning(raw)

        #### Define Indicator ####
        message("Defining Indicator...")
        ptdat <- defIndicator(ptdat)

        #### Address Missingness ####
        message("Addressing Missingness...")
        
        # ID clusters with more than 20% weighted missingness
        #TODO set this up to loop over all vars
        missing.vars <- idMissing(ptdat, this.var="bin_cooking_fuel_mapped", criteria=.2, wt.var='hh_size') 
        ptdat <- ptdat[!(cluster_id %in% missing.vars)] #remove these clusters
        if (nrow(ptdat) == 0) {
          next
        }

        # Remove cluster_ids with missing hhweight or invalid 
        missing.wts <- idMissing(ptdat, this.var="hhweight", criteria=0, wt.var=NA, debug=T) 
        
        ptdat <- ptdat[!(cluster_id %in% missing.wts)] #remove these clusters
        ptdat <- ptdat[!()]
        miss_wts <- unique(ptdat$cluster_id[which(is.na(ptdat$hhweight))])
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