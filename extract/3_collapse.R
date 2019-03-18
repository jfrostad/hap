# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 06/12/2018
# Purpose: Collapse data for HAP
# source("/homes/jfrostad/_code/lbd/hap/extract/3_collapse.R", echo=T)
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j/"
  h_root <- file.path("/ihme/homes", Sys.info()["user"])
  arg <- commandArgs()[-(1:3)] # First args are for unix use only
  
  if (length(arg)==0) {
    # arg <- c("IND", #current project iteration
    #          "8", #output version
    #          1) #number of cores provided to multicore functions
  }
  
  package_lib    <- file.path(h_root, '_code/_lib/pkg')
  ## Load libraries and  MBG project functions.
  #.libPaths(c( .libPaths(), package_lib))
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
#TODO verify which of these are actually necessary, took from a random image in Ani's wash dir
#/share/geospatial/mbg/wash/s_imp/model_image_history
pkg.list <- c('RMySQL', 'data.table', 'dismo', 'doParallel', 'dplyr', 'foreign', 'gbm', 'ggplot2', 'glmnet', 
              'grid', 'gridExtra', 'gtools', 'magrittr', 'pacman', 'parallel', 'plyr', 'raster', 'rgdal', 'rgeos',
              'seegMBG', 'seegSDM', 'tictoc') #will be loaded by MBG setup

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
cores <- 20
manual_date <- "2018_12_18" #set this value to use a manually specified extract date
latest_date <- T #set to TRUE in order to disregard manual date and automatically pull the latest value
save.intermediate <- F
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/')
census.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/census')
doc.dir <- file.path(j_root, 'WORK/11_geospatial/hap/documentation')
def.file <- file.path(doc.dir, 'definitions.xlsx')

###Output###
out.dir  <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapse/hap/')
model.dir  <- file.path(j_root, 'WORK/11_geospatial/10_mbg/input_data/hap')
share.model.dir  <- file.path('/share/geospatial/mbg/input_data/')
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#general functions#
central.function.dir <- file.path(h_root, "_code/_lib/functions")
# this pulls the general misc helper functions
file.path(central.function.dir, "misc.R") %>% source
#hap functions#
hap.function.dir <- file.path(h_root, '_code/lbd/hap/extract/functions')
#this pulls hap collapse helper functions
file.path(hap.function.dir, '/collapse_fx.R') %>% source
#shared functions#
gbd.shared.function.dir <- file.path(j_root,  "temp/central_comp/libraries/current/r")
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source
file.path(gbd.shared.function.dir, 'get_ids.R') %>% source
file.path(gbd.shared.function.dir, 'get_covariate_estimates.R') %>% source

lbd.shared.function.dir <- file.path(h_root, "_code/lbd/lbd_core/mbg_central")
file.path(lbd.shared.function.dir, 'setup.R') %>% source
  mbg_setup(repo=lbd.shared.function.dir, package_list=pkg.list) #load mbg functions
#***********************************************************************************************************************

# ---COLLAPSE-----------------------------------------------------------------------------------------------------------
#automatically pull latest date if manual date not provided
# get input version from most recently modified data file
if (latest_date==T) { 
  file_date <- gsub('.feather|points_|poly_', 
                    '', 
                    sort(list.files(data.dir, pattern = '*points'), decreasing=T)[1]) #pull latest poitns filename
} else file_date <- manual_date
  
#loop over points and polygons to collapse
collapseData <- function(this.family,
                         point=NULL,
                         census.file=NULL,
                         out.temp=NULL) {
  
  message("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

  #ipums files are done in parallel due to size
  if (!is.null(census.file)) {
    message("CENSUS FILE=", census.file)
    census <- T
    # Load data
    raw <- read_feather(census.file) %>% 
      as.data.table
    # Determine if point or poly
    point <- !all(raw[, lat] %>% unique %>% is.na)
    
  } else {
    census <- F
    # Load data
      if(point) {
        
        raw <- paste0(data.dir, 'points_', file_date, ".feather") %>% 
          read_feather %>% 
          as.data.table
        
      } else {
        
        #note that poly file is now split into 2 files due to size limitations
        raw <- lapply(paste0(data.dir, c('poly1', 'poly2'), '_', file_date, ".feather"), 
                      read_feather) %>% rbindlist
          
      }
  } 
  
  message("Loading data...[point=", point, "]/[census=", census, "]")

  #loop over various families of indicators
  message(paste('->Processing:', this.family))
  
  #### Subset & Shape Data ####
  dt <- initialClean(raw, var.fam=this.family, is.point=point)
    
  #output an intermediate file prior to collapse/indicator definition for preliminary analysis
  if (!is.null(out.temp)) {
    message('----->Save raw data to temp folder')
    
    
    out.dir <- file.path(out.temp, this.family) 
    if (!out.dir %>% dir.exists) dir.create(out.dir, recursive=T) #create dir if missing
    
    #build filename and then save a feather
    paste0(out.dir, '/uncollapsed_',
           ifelse(census, tools::file_path_sans_ext(basename(census.file)),
                  ifelse(point, 'points', 'poly')),
           '.feather') %>% write_feather(dt, path=.)
    
    paste0("saved intermediate files to:", out.temp) %>% return #end process here if saving int files
    
  } else {  
  
    #define the indicators based on the intermediate variables youve extracted  
    dt <- defIndicator(dt, var.fam=this.family, definitions=def.file, debug=F)
  
    #### Address Missingness ####
    message("\nBegin Addressing Missingness...")

    # ID clusters with more than 20% weighted missingness
    #TODO set this up to loop over all vars -> right now just using cooking_fuel_solid as the gold standard
    missing.vars <- idMissing(dt, this.var="cooking_fuel_solid", criteria=.2, wt.var='hh_size')
    
    #ID cluster_ids with missing hhweight
    #decided to use 10% unweighted criteria instead of 0 tolerance
    missing.wts <- idMissing(dt, this.var="hhweight", criteria=.1, wt.var=NA)
    
    #ID points with hhweight|hh_size<=0 (invalid)
    #drop clusters with more than 20% invalid, then drop invalid rows 
    invalid.wts <- idMissing(dt, this.var="hhweight", criteria=.1, wt.var=NA, check.threshold = T, threshold=0)
    invalid.sizes <- idMissing(dt, this.var="hh_size", criteria=.1, wt.var=NA, check.threshold = T, threshold=0)
    
    #ID missing hh sizes, then crosswalk values
    missing.sizes <- idMissing(dt, this.var="hh_size", criteria=.05, wt.var=NA)
    
    #also print the # of hh sizes that are missing (rowwise):
    message('There are #', nrow(dt[is.na(hh_size)]), '(',
            round(nrow(dt[is.na(hh_size)])/nrow(dt)*100), '%) rows missing hh_size') 
    
    #output diagnostics regarding invalid clusters
    #TODO move this to the idMissing function
    remove.clusters <- c(missing.vars, 
                         missing.wts,
                         invalid.wts,
                         invalid.sizes) %>% unique
    dt.drop <- dt[cluster_id %in% remove.clusters, .(nid, ihme_loc_id, int_year, cluster_id)] %>%
      setkey(., nid, ihme_loc_id, int_year, cluster_id) %>% 
      unique(., by=key(.))
    dt.drop[, missing_variables := sum(cluster_id %in% missing.vars), by=nid]
    dt.drop[, missing_hhweights := sum(cluster_id %in% missing.wts), by=nid]
    dt.drop[, invalid_hhweights := sum(cluster_id %in% invalid.wts), by=nid]
    dt.drop[, invalid_hhsizes := sum(cluster_id %in% invalid.sizes), by=nid]
    dt.drop[, missing_hhsizes := sum(cluster_id %in% missing.sizes), by=nid]
    unique(dt.drop[, cluster_id := NULL], by='nid') %>% 
      write.csv(., file=paste0(doc.dir, '/', this.family, '/dropped_clusters_',
                               ifelse(census, tools::file_path_sans_ext(basename(census.file)),
                                      ifelse(point, 'points', 'poly')), '.csv'))
    
    #remove these clusters and proceed
    message('dropping ', length(remove.clusters), 
            ' clusters based on variable missingness/invalidity above cluster-level criteria thresholds')
    dt <- dt[!(cluster_id %in% remove.clusters)]
    
    #remove invalid rows that were insufficient in number to trigger criteria thresholds
    message('dropping additional ', dt[(hhweight<=0)] %>% nrow, 
            ' rows based on hhweight missingness/invalidity below cluster-level criteria thresholds')
    dt <- dt[!(hhweight<=0)] #drop invalid rows as well
    
    # Crosswalk missing/invalid household size data
    #TODO discuss this part with ani after learning more, for now just impute as 1
    dt[(is.na(hh_size) | hh_size<=0), hh_size := 1]
    
    # message("Crosswalking HH Sizes...")
    # if (!ipums) {
    #   ptdat <- hh_cw_reg(data = ptdat)
    # } else {
    #   ptdat <- assign_ipums_hh()
    # }
    
    #read in location data to use in cw
    # locs = get_location_metadata(location_set_id = 9, gbd_round_id = 5)
    # dt <- merge(dt, locs[, .(ihme_loc_id, region_id, super_region_id)], by='ihme_loc_id')
    # cw(dt, this.var='cooking_fuel_solid', debug=T)
    
    #subset to years that are >= 2000 as we dont model before this time period
    message('\nCreate column with median year of each cluster. Subset to >2000')
    dt[, year_median := median(int_year, na.rm=T) %>% floor, by=cluster_id]
    dt[, year_median := weighted.mean(year_median, w=hhweight*hh_size) %>% floor, by=cluster_id]
    dt <- dt[year_median>=2000]
  
    #### Aggregate Data ####
    # Aggregate indicator to cluster level
    message("\nBegin Collapsing Variables")
    agg.dt <- aggIndicator(dt, var.fam=this.family, is.point=point, debug=F) #list of variables to aggregate
    agg.dt[, polygon := !(point)]
    message("->Complete!")
    
    # Standardize the year variable for each survey using the weighted mean of the NID
    # Weight by sum of sample weights
    message("\nStandardizing Year Variable")
    agg.dt[, year := weighted.mean(year_median, w=sum_of_sample_weights), by=nid]
    
    # Skip the rest of the process if no rows of data are left
    if (nrow(dt) == 0) { 
      message('no data left to return!')
      return(NULL)
    } else return(agg.dt)
    
  }
}
  
#populate vector of IPUMS filepaths
ipums.files = list.files(census.dir, pattern='*.feather', full.names = T)

#Run fx for each point/poly
cooking <- mapply(collapseData, point=F, this.family='cooking', SIMPLIFY=F) %>% rbindlist

#Run fx for each census file
cooking.census <- mcmapply(collapseData, census.file=ipums.files, this.family='cooking', SIMPLIFY=F, mc.cores=cores) %>% 
  rbindlist

#run all fx to generate intermediate input data for exploration plotting
if (save.intermediate == T) {
cooking <- mcmapply(collapseData, point=T:F, this.family='cooking', SIMPLIFY=F, mc.cores=1,
                    out.temp='/share/geospatial/jfrostad')
cooking.census <- mcmapply(collapseData, census.file=ipums.files, this.family='cooking', SIMPLIFY=F, mc.cores=cores,
                           out.temp='/share/geospatial/jfrostad')
housing <- mcmapply(collapseData, point=T:F, this.family='housing', SIMPLIFY=F, mc.cores=1,
                    out.temp='/share/geospatial/jfrostad')
housing.census <- mcmapply(collapseData, census.file=ipums.files, this.family='housing', SIMPLIFY=F, mc.cores=cores,
                           out.temp='/share/geospatial/jfrostad')
stop()
}

#Combine and redefine the row_id
cooking <- list(cooking, cooking.census) %>% rbindlist
cooking[, row_id := .I]
setkey(cooking, row_id)

#save poly and point collapses
#TODO loop over all fams in fx
this.family='cooking'
paste0(out.dir, "/", "data_", this.family, '_', today, ".feather") %>%
  write_feather(cooking, path=.)
#***********************************************************************************************************************

# ---RESAMPLE-----------------------------------------------------------------------------------------------------------
#prep for resampling
vars <- names(cooking)[names(cooking) %like% 'cooking']
#TODO should this var just be dropped by this stage?
vars <- vars[!(vars %like% 'risk')] #dont collapse the continuous risk var
cooking[, (vars) := lapply(.SD, function(x, count.var) {x*count.var}, count.var=N), .SDcols=vars]

#drop weird shapefiles for now
#TODO investigate these issues
cooking <- cooking[!(shapefile %like% "2021")]
cooking <-cooking[!(shapefile %like% "PRY_central_wo_asuncion")]
#cooking <-cooking[!(shapefile %like% "geo2_br1991_Y2014M06D09")]
cooking <-cooking[!(shapefile %like% "geo2015_2014_1")]
cooking <-cooking[!(shapefile %like% "#N/A")]


#only work on PER for now
#cooking <- cooking[iso3 == "PER"]

#run core resampling code
dt <- resample_polygons(data = cooking,
                        cores = 20,
                        indic = vars,
                        density = 0.001,
                        gaul_list = lapply(unique(cooking$iso3) %>% tolower, get_adm0_codes) %>% unlist %>% unique)

#redefine row ID after resampling
dt[, row_id := .I]
setkey(dt, row_id)

#save resampled data
paste0(model.dir, "/", "data_", this.family, '_', today, ".feather") %>%
  write_feather(dt, path=.)

#prep for MDG
setnames(dt,
         c('iso3'),
         c('country'))

#TODO these varnames are necessary for the ad0 aggregation code, are they necessary everywhere else?
dt[, source := survey_series]
dt[, point := !polygon]

#TODO should simplify dataset by dropping useless vars
# dt <- dt[, list(nid, country, year, latitude, longitude, survey_series, urban, N, sum_of_sample_weights,
#                 cooking_clean, cooking_med, cooking_dirty,
#                 cluster_id, polygon, shapefile, location_code, weight, pseudocluster)]

#save into MDG dir
file.path(share.model.dir, 'cooking_clean.RDS') %>% saveRDS(dt, file=.)
file.path(share.model.dir, 'cooking_clean.csv') %>% write.csv(dt, file=., row.names=F)
#save both ways so that you can try modelling from either end
file.path(share.model.dir, 'cooking_dirty.RDS') %>% saveRDS(dt, file=.)
file.path(share.model.dir, 'cooking_dirty.csv') %>% write.csv(dt, file=., row.names=F)
#also save a solid fuel only model
file.path(share.model.dir, 'cooking_fuel_solid.RDS') %>% saveRDS(dt, file=.)
file.path(share.model.dir, 'cooking_fuel_solid.csv') %>% write.csv(dt, file=., row.names=F)