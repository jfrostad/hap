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

  ## Load libraries and  MBG project functions.
  package_lib    <- file.path(h_root, '_code/_lib/pkg')
  .libPaths(package_lib)
  
  # necessary to set this option in order to read in a non-english character shapefile on a linux system (cluster)
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
} else {j_root <- "J:"; h_root <- "H:"}

#load packages
pacman::p_load(data.table, dplyr, feather, fst, googledrive, readxl, tidyverse, ellipsis, sf) 
package_list <- fread('/share/geospatial/mbg/common_inputs/package_list.csv') %>% t %>% c

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)
#today <- '2019_07_29' #TODO setup to pull latest date if !run_collapse

#options
cores <- 10
this.family='cooking'
modeling_shapefile_version <- "2019_09_10"
# manual_date <- "2018_12_18" #set this value to use a manually specified extract date
# collapse_date <- "2019_12_11" #which data of collapse to use if not rerunning
latest_date <- T #set to TRUE in order to disregard manual date and automatically pull the latest value
save_intermediate <- F
run_collapse <- F #set to TRUE if you have new data and want to recollapse
run_resample <- T #set to TRUE if you have new data and want to rerun polygon resampling
save_diagnostic <- F #set to TRUE to save the problematic survey diagnostic
new_vetting <- T #set to TRUE to refresh the vetting diagnostic
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/')
census.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/census')
doc.dir <- file.path(j_root, 'WORK/11_geospatial/hap/documentation')
def.file <- file.path(doc.dir, 'definitions.xlsx')
raw.dir <- file.path("/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/hap") #where your extractions are stored

#TODO problem NIDs that seem to be getting dropped
dropped.nids <- read_excel('/home/j/temp/albrja/diarrhea_lri_wash/hap/collapse_nid_analysis.xlsx', sheet='dropped_nids_collapse')

###Output###
out.dir  <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapse/hap/')
model.dir  <- file.path(j_root, 'WORK/11_geospatial/10_mbg/input_data/hap')
share.model.dir  <- file.path('/share/geospatial/mbg/input_data/')
temp.dir <- file.path('/share/geospatial/jfrostad')
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
gbd.shared.function.dir <- file.path(j_root,  "temp/central_comp/libraries/v69/r")
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source
file.path(gbd.shared.function.dir, 'get_ids.R') %>% source
file.path(gbd.shared.function.dir, 'get_covariate_estimates.R') %>% source

lbd.shared.function.dir <- file.path(h_root, "_code/lbd/hap/mbg_central")
file.path(lbd.shared.function.dir, 'setup.R') %>% source
mbg_setup(repo=lbd.shared.function.dir, package_list=package_list) #load mbg functions
#***********************************************************************************************************************

# ---COLLAPSE-----------------------------------------------------------------------------------------------------------
#read in codebook
codebook <- file.path(raw.dir, 'hap.xlsx') %>% read_xlsx(., sheet='codebook') %>% as.data.table
stages <- file.path(j_root, 'WORK/11_geospatial/10_mbg/stage_master_list.csv') %>% fread #read info about stages

#automatically pull latest date if manual date not provided
# get input version from most recently modified data file
if (latest_date) { 
  file_date <- gsub('.fst|points_|poly_', 
                    '', 
                    sort(list.files(data.dir, pattern = '*points'), decreasing=T)[1]) #pull latest poitns filename
  
  #pull latest points filename
  file_date <- list.files(data.dir, pattern = '*points') %>% 
    sort(., decreasing=T) %>% 
    .[1] %>% 
    gsub('.fst|points_|poly_',  '', .)
  
} else file_date <- manual_date
  
#loop over points and polygons to collapse
collapseData <- function(this.family,
                         census.file=NULL,
                         out.temp=NULL,
                         subcountry=NULL,
                         debug=F) {
  
  message("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

  #ipums files are done in parallel due to size
  if (!is.null(census.file)) {
    message("CENSUS FILE=", census.file)
    census <- T
    # Load data
    raw <- read.fst(census.file, as.data.table=T)
    dt <- initialClean(raw, var.fam=this.family)
    
  } else {
    census <- F
    # Load data
    pt <- paste0(data.dir, 'points_', file_date, ".fst") %>% read.fst(., as.data.table=T)
    poly <- paste0(data.dir, 'poly_', file_date, ".fst") %>% read.fst(., as.data.table=T)

    dt <- list(
      initialClean(pt, var.fam=this.family),
      initialClean(poly, var.fam=this.family)
    ) %>% rbindlist
    
    #cleanup
    raw <- list(pt, poly) %>% rbindlist
  } 
  
  message("Loading data...[census=", census, "]")

  #loop over various families of indicators
  message(paste('->Processing:', this.family))
  #### Subset & Shape Data ####
  #subset to countries, generally used interactively in order to see why surveys are being dropped
  if (!(is.null(subcountry))) {
    dt <- dt[ihme_loc_id==subcountry]
    raw <- raw[iso3==subcountry]
  }
  
  #launch browser to debug interactively
  if (debug) browser()

    
  #output an intermediate file prior to collapse/indicator definition for preliminary analysis
  if (!is.null(out.temp)) {
    message('----->Save raw data to temp folder')
    
    
    out.dir <- file.path(out.temp, this.family) 
    if (!out.dir %>% dir.exists) dir.create(out.dir, recursive=T) #create dir if missing
    
    #build filename and then save a feather
    paste0(out.dir, '/uncollapsed_',
           ifelse(census, tools::file_path_sans_ext(basename(census.file)),
                  ifelse(point, 'points', 'poly')),
           '.feather') %>% write_feather(raw, path=.)
    
    paste0("saved intermediate files to:", out.temp) %>% return #end process here if saving int files
    
  } else {  

    #define the indicators based on the intermediate variables youve extracted  
    dt <- defIndicator(dt, var.fam=this.family, definitions=def.file, debug=F)
    
    #### Address Missingness ####
    message("\nBegin Addressing Missingness...")
    
    # ID clusters with more than 20% weighted missingness
    #TODO set this up to loop over all vars -> right now just using cooking_fuel_solid as the gold standard
    missing.vars <- idMissing(dt, this.var="cooking_fuel_dirty", criteria=.2, wt.var='hh_size')
    
    #ID cluster_ids with missing hhweight
    #decided to use 20% unweighted criteria instead of 0 tolerance
    missing.wts <- idMissing(dt, this.var="hhweight", criteria=.2, wt.var=NA)
    
    #ID points with hhweight|hh_size<=0 (invalid)
    #drop clusters with more than 20% invalid, then drop invalid rows 
    invalid.wts <- idMissing(dt, this.var="hhweight", criteria=.2, wt.var=NA, check.threshold = T, threshold=0)
    invalid.sizes <- idMissing(dt, this.var="hh_size", criteria=.2, wt.var=NA, check.threshold = T, threshold=0)
    
    #ID missing hh sizes, then crosswalk values
    missing.sizes <- idMissing(dt, this.var="hh_size", criteria=.2, wt.var=NA)
    
    #also print the # of hh sizes that are missing (rowwise):
    message('There are #', nrow(dt[is.na(hh_size)]), '(',
            round(nrow(dt[is.na(hh_size)])/nrow(dt)*100), '%) rows missing hh_size') 
    
    #output diagnostics regarding invalid clusters
    #TODO move this to the idMissing function
    #TODO fix this for IPUMS, file is outputting incorectly
    remove.clusters <- c(missing.vars, 
                         missing.wts,
                         invalid.wts,
                         invalid.sizes) %>% unique
    
    #remove these clusters and proceed
    message('dropping ', length(remove.clusters), 
            ' clusters based on variable missingness/invalidity above cluster-level criteria thresholds')
    dt <- dt[!(cluster_id %in% remove.clusters)]
    
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
    
    # Crosswalk missing/invalid household size data
    #TODO recode HH sizes that have values like 98, etc which represent unknown or other?
    #TODO discuss this part with ani after learning more, for now just impute as 1
    #dt[(is.na(hh_size) | hh_size==0 | hh_size > 95), table(nid)]
    dt[(is.na(hh_size) | hh_size==0 | hh_size > 95), hh_size := 1]
    
    #remove invalid rows that were insufficient in number to trigger criteria thresholds
    message('dropping additional ', dt[(hhweight<=0)] %>% nrow, 
            ' rows based on hhweight missingness/invalidity below cluster-level criteria thresholds')
    dt <- dt[hhweight>0] #drop invalid rows as well
    message('dropping additional ', dt[(hh_size<=0)] %>% nrow, 
            ' rows based on hhsize invalidity below cluster-level criteria thresholds')
    dt <- dt[hh_size>0] #drop invalid rows as well
    
    #subset to years that are >= 2000 as we dont model before this time period
    message('\nCreate column with median year of each cluster. Subset to >2000')
    dt[, year_median := median(int_year, na.rm=T) %>% floor, by=cluster_id]
    dt[, year_median := weighted.mean(year_median, w=hhweight*hh_size) %>% floor, by=cluster_id]
    dt <- dt[year_median>=2000]
  
    #### Aggregate Data ####
    # Aggregate indicator to cluster level
    message("\nBegin Collapsing Variables")
    agg.dt <- aggIndicator(dt, var.fam=this.family, debug=F) #list of variables to aggregate
    message("->Complete!")

    # Standardize the year variable for each survey using the weighted mean of the NID
    # Weight by sum of sample weights
    message("\nStandardizing Year Variable")
    agg.dt[, year := weighted.mean(year_median, w=sum_of_sample_weights) %>% floor, by=nid]
    
    # # Report on surveys dropped during collapse
    # if(census.file %>% is.null) {
    #   for (iso in unique(raw$iso3) %>% sort) {
    #     
    #     dropped.nids <- unique(raw[iso3==iso, nid]) %>%
    #       .[!(. %in% unique(agg.dt[ihme_loc_id==iso, nid]))]
    #     
    #     if (length(dropped.nids)>0) message(iso, '\nNIDs dropped by collapse:\n'); cat(dropped.nids, sep='\n')
    #   }
    # }
    
    # Skip the rest of the process if no rows of data are left
    if (nrow(dt) == 0) {message('no data left to return!'); return(NULL)}
    else return(agg.dt)
    
  }
  
}
  
#populate vector of IPUMS filepaths
ipums.files = list.files(census.dir, pattern='*.fst', full.names = T)

#run all fx to generate intermediate input data for exploration plotting
if (save_intermediate) {
  
  cooking <- mcmapply(collapseData, point=T:F, this.family='cooking', SIMPLIFY=F, mc.cores=1, out.temp=tmp.dir)
  cooking.census <- mcmapply(collapseData, census.file=ipums.files, this.family='cooking', SIMPLIFY=F, mc.cores=cores, out.temp=tmp.dir)
  housing <- mcmapply(collapseData, point=T:F, this.family='housing', SIMPLIFY=F, mc.cores=1, out.temp=tmp.dir)
  housing.census <- mcmapply(collapseData, census.file=ipums.files, this.family='housing', SIMPLIFY=F, mc.cores=cores, out.temp=tmp.dir)
  stop('Finished saving intermediate files')

}


if (run_collapse) {
  
  #Run fx for each point/poly
  cooking <- collapseData('cooking')
  
  #Run fx for each census file
  cooking.census <- mcmapply(collapseData, census.file=ipums.files, this.family='cooking', SIMPLIFY=F, mc.cores=1) %>% 
    rbindlist
    
  #combine all and redefine the row ID
  cooking <- list(cooking, cooking.census) %>% 
    rbindlist %>% 
    .[, row_id := .I] %>% 
    setkey(., row_id)
  
  #cleanup
  rm(cooking.census)

  #save poly and point collapses
  #TODO loop over all fams in fx
  paste0(out.dir, "/", "collapsed_data_", this.family, ".fst") %>%
    write.fst(cooking, path=.)

  #combine diagnostics and cleanup intermediate files
  col.diagnostic <- 
    collapseCleanup(this.family, codebook=codebook, test.vars=c('cooking_fuel_dirty', 'hh_size'), cleanup=T, debug=F)
  
} else cooking <- paste0(out.dir, "/", "collapsed_data_", this.family, ".fst") %>% read.fst(as.data.table=T)
#***********************************************************************************************************************

# ---DATA EXCLUSION-----------------------------------------------------------------------------------------------------
#exclude datapoints based on HAP vetting
setwd(doc.dir)
if (new_vetting) {setwd(doc.dir); googledrive::drive_auth(cache='drive.httr-oauth'); drive_download(as_id('1nCldwjReSIvvYgtSF4bhflBMNAG2pZxE20JV2BZSIiQ'), overwrite=T)}
vetting <- file.path(doc.dir, 'HAP Tracking Sheet.xlsx') %>% read_xlsx(sheet='1. Vetting', skip=1) %>% as.data.table
excluded_nids <- vetting[`HAP Vetting Status`=='Excluded', nid] %>% unique #get list of excluded points
message('Excluding the following NIDs:')
print(excluded_nids)
cooking <- cooking[!(nid %in% excluded_nids)] #remove any excluded datapoints
#***********************************************************************************************************************

# ---RESAMPLE-----------------------------------------------------------------------------------------------------------
#prep for resampling
vars <- names(cooking) %>% .[. %like% 'cooking']
vars <- c('cooking_fuel_solid', 'cooking_fuel_dirty')

#convert to count space
cooking[, (vars) := lapply(.SD, function(x, count.var) {x*count.var}, count.var=N), .SDcols=vars]

#shapefile issues: document here shapefiles that cause resample_polygons to fail
shapefile_issues <- c('IRQ_ADM3_2019_OCHA', 'TLS_regions', 'g2015_2004_2')
shapefile_issues_nids <- cooking[shapefile %in% shapefile_issues, unique(nid)]
message('shapfile issues with the following iso3s: ', cooking[shapefile %in% shapefile_issues, unique(ihme_loc_id)])

#TODO, current only able to resample stage1/2 countries
dt <- cooking[iso3 %in% unique(stages[Stage %in% c('1', '2a', '2b'), iso3])] %>% 
  setnames(.,  c('lat', 'long'), c('latitude', 'longitude')) %>% #mbg formatting requirement
  .[!(shapefile %in% shapefile_issues)] #TODO investigate this shapefile issue

#resample the polygons using internal fx
#TODO potentially worth parallelizing by region?
if (run_resample) {
  
  pt <- dt[polygon==T] %>% #only pass the poly rows to the fx, pts are dropping
    resample_polygons(data = .,
                      cores = 20,
                      indic = vars,
                      density = 0.001,
                      gaul_list = lapply(unique(dt$iso3) %>% tolower, get_adm0_codes) %>% unlist %>% unique) %>% 
    .[, pseudocluster := NULL] #redundant

} else stop('run_resample=FALSE, cannot save model data')

#combine all points
dt <- list(dt[polygon==F], pt) %>% 
  rbindlist(use.names=T, fill=T) %>% 
  .[polygon==F, weight := 1] #weights are produced by the resample polygons fx

for (iso in unique(dt$ihme_loc_id) %>% sort) {
  
  dropped.nids <- unique(cooking[ihme_loc_id==iso, nid]) %>% 
    .[!(. %in% unique(dt[ihme_loc_id==iso, nid]))]
  
  if (length(dropped.nids)>0) message('\n', iso, '...NIDs dropped by resample:\n'); cat(dropped.nids, sep=' | ')
}

#redefine row ID after resampling
dt[, row_id := .I]
setkey(dt, row_id)

#save resampled data
paste0(model.dir, "/", "resampled_data_", this.family, ".fst") %>%
  write.fst(dt, path=.)

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
#save each one for modelling in binary/ordinal space
file.path(share.model.dir, 'cooking_fuel_solid.RDS') %>% saveRDS(dt, file=.)
file.path(share.model.dir, 'cooking_fuel_solid.csv') %>% write.csv(dt, file=., row.names=F)
file.path(share.model.dir, 'cooking_fuel_kerosene.RDS') %>% saveRDS(dt, file=.)
file.path(share.model.dir, 'cooking_fuel_kerosene.csv') %>% write.csv(dt, file=., row.names=F)
file.path(share.model.dir, 'cooking_fuel_dirty.RDS') %>% saveRDS(dt, file=.)
file.path(share.model.dir, 'cooking_fuel_dirty.csv') %>% write.csv(dt, file=., row.names=F)
file.path(share.model.dir, 'cooking_fuel_clean.RDS') %>% saveRDS(dt, file=.)
file.path(share.model.dir, 'cooking_fuel_clean.csv') %>% write.csv(dt, file=., row.names=F)
#***********************************************************************************************************************************
 
#---ID PROBLEM SURVEYS--------------------------------------------------------------------------------------------------
#identify any surveys that did not make it through the pipeline but were codebooked for cooking_fuel
codebooked_nids <- codebook[!is.na(cooking_fuel) & ihme_loc_id %in% unique(stages[Stage %in% c('1', '2a', '2b'), iso3]) & year_start > 2000, nid] %>% unique
missing_nids <- codebooked_nids %>% .[!(. %in% unique(dt$nid))]

#update user to NIDs that need to be added to tracking sheet
tracking <- file.path(doc.dir, 'HAP Tracking Sheet.xlsx') %>% read_xlsx(sheet='2. Tracking', skip=1) %>% as.data.table

message('These NIDs need to be added to the tracking sheet:\n')
missing_nids %>% 
  .[!(. %in% unique(tracking$nid))] %>% 
  cat(., sep='\n')
#***********************************************************************************************************************************
 
#---MERGE CSV's of PROBLEMATIC SURVEYS----------------------------------------------------------------------------------------------
#merging: new geographies to match, cooking fuel missing strings, dropped clusters 

#TODO continue cleaning up and converting to DT code
if (save_diagnostic) {
  
  #read in csv's
  geographies <- fread('/ihme/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/new_geographies_to_match.csv')
  missing_strings <- fread(paste0(j_root, '/WORK/11_geospatial/hap/documentation/str_review/cooking_fuel_missing_strings.csv'))
  dropped_clusters <- fread(paste0(j_root, '/WORK/11_geospatial/hap/documentation/cooking/dropped_clusters.csv'))
  
  
  #prep geographies_to_match csv
  geographies<- geographies[Stage!=3, .(iso3, nid, year_start, survey_name)] %>% .[, geomatch := 'X']
  # geographies <- subset(geographies, !Stage %in% '3')
  # geographies <- select(geographies, iso3, nid, year_start, survey_name)
  # geographies$geomatch <- 'X'
  
  #prep missing_strings csv
  missing_strings[, unmapped_percent := (prop*100) %>% round(., digits=1) %>% paste0(., '%')]
  missing_strings <- missing_strings[, .(missing_strings, nid, ihme_loc_id, int_year, survey_name, 
                                         var, var_mapped, var_og, unmapped_percent)]
  
  # missing_strings$unmapped_var <- paste(missing_strings$var, missing_strings$var_mapped) 
  # missing_strings$unmapped_string <- missing_strings$var_og 
  # percent <- missing_strings$prop * 100
  # percent <- round(percent, digits = 1)
  # missing_strings$unmapped_percent <- paste0(percent, "%") 
  # missing_strings <- select(missing_strings, nid, ihme_loc_id, int_year, survey_name, 
  #                           unmapped_var, unmapped_string, unmapped_percent)
  
  #prep dropped_clusters csv
  dropped_clusters$dropped_var <- paste(dropped_clusters$var, dropped_clusters$type)
  percent <- dropped_clusters$count / dropped_clusters$total * 100 
  percent <- round(percent, digits = 1) 
  dropped_clusters$dropped_percent <- paste0(percent, "%  -  (", dropped_clusters$count, "/", 
                                             dropped_clusters$total, ")")  
  dropped_clusters <- select(dropped_clusters, nid, ihme_loc_id, int_year, dropped_var, 
                             dropped_percent) %>% distinct
  
  #merge
  problem_surveys <- merge(geographies, missing_strings, by.x = c('nid', 'iso3', 'year_start', 'survey_name'), 
                           by.y = c('nid', 'ihme_loc_id', 'int_year', 'survey_name'), all.x = TRUE, all.y = TRUE)
  problem_surveys <- merge(problem_surveys, dropped_clusters, by.x = c('nid', 'iso3', 'year_start'), 
                           by.y = c('nid', 'ihme_loc_id', 'int_year'), all.x = TRUE, all.y = TRUE)
  problem_surveys <- problem_surveys[, year_start > 1999]
  
  #output
  write.csv(problem_surveys, paste0(j_root, '/WORK/11_geospatial/hap/documentation/all_problematic_surveys.csv'), row.names=F)

}
#***********************************************************************************************************************************