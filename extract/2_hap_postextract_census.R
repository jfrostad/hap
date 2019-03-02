rm(list=ls())

#Define values
topic <- "hap"
extractor_ids <- c('jfrostad', 'qnguyen1', 'albrja')
redownload <- T #update the codebook from google drive
cluster <- TRUE #running on cluster true/false
geos <- TRUE #running on geos nodes true/false
cores <- 20
#FOR THE CLUSTER:
#qlogin -now n -pe multi_slot 5 -P proj_geospatial -l geos_node=TRUE
#source('/homes/jfrostad/_code/lbd/hap/extract/2_hap_postextract_census.R')

#Setup
h <- ifelse(Sys.info()[1]=="Windows", "H:/", file.path("/ihme/homes", Sys.info()["user"])) #Your username
j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j")
l <- ifelse(Sys.info()[1]=="Windows", "L:/", "/ihme/limited_use/")
folder_in <- file.path(l, "LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions", topic, 'batch') #where your extractions are stored
folder_out <- paste0(l, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/", topic, "/census") #where you want to save the big csv of all your extractions together

# ####### YOU SHOULDN'T NEED TO CHANGE ANYTHING BELOW THIS LINE. SORRY IF YOU DO ##################################################
stages <- read.csv(paste0(j, "/temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F)
if (geos){
  package_lib <- '/temp/geospatial/geos_packages'
  .libPaths(package_lib)
  library(feather) #as of 11/20/2017 feather does not work on prod
} else {
  package_lib <- '/temp/geospatial/packages'}
.libPaths(package_lib)

#Load packages
packages <- c('haven', 'stringr', 'data.table', 'dplyr', 'magrittr', 'parallel', 'doParallel')
packages <- lapply(packages, library, character.only=T)
library(gsheet, lib.loc = file.path(h, '_code/_lib/pkg'))

#timestamp
today <- Sys.Date() %>% gsub("-", "_", .)

message("Getting common column names")
if (topic == "hap" & geos){
  #get the most recent pt and poly feathers and parse them for column names
  pt <- paste0(l, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/") %>% list.files(pattern="points", full.names=T) %>% grep(pattern=".feather$", value=T)
  pt <- pt[length(pt)] %>% feather_metadata
  poly <- paste0(l, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/") %>% list.files(pattern="points", full.names=T) %>% grep(pattern=".feather$", value=T)
  poly <- poly[length(poly)] %>% feather_metadata
  pt_names <- pt[3] %>% unlist %>% names %>% gsub(pattern="types.", replacement="")
  poly_names <- poly[3] %>% unlist %>% names %>% gsub(pattern="types.", replacement="")
  noms <- c(pt_names, poly_names) %>% unique
} else{
  message("The error you're about to get has to do with the fact that you're not running on geos and/or you're not prepping hap data.")
  stop("I don't know how to parse .Rdata files for column headers in a timely way. Please figure something out and make a pull request.")
}

message("List IPUMS dtas")
extractions <- list.files(folder_in, pattern="IPUMS_CENSUS", full.names=T)

#Change to handle batch extractions by only reading in those IDs that have been extracted by Queenie
# if(redownload==T) drive_download(as_id('1Nd3m0ezwWxzi6TmEh-XU4xfoMjZLyvzJ7vZF1m8rv0o'), overwrite=T)
# codebook <- read_xlsx('hap.xlsx', sheet='sheet1') %>% as.data.table
codebook <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1Nd3m0ezwWxzi6TmEh-XU4xfoMjZLyvzJ7vZF1m8rv0o/edit#gid=0')
codebook <- as.data.table(codebook)
#create output name, note that we need to remove the leading info on some of the survey names(take only str after /)
codebook[, output_name := paste0(ihme_loc_id, '_', tools::file_path_sans_ext(basename(survey_name)), '_', year_start, '_', year_end, '_', nid, '.csv')]
#subset the codebook to ONLY the files that our extractors have worked on
codebook <- codebook[assigned %in% extractor_ids]
#get the names of all the new files
new.files <- codebook[, output_name] %>% unique %>% paste(., collapse="|")
extractions <- grep(new.files, extractions, invert=F, value=T)
#extractions <- grep('PER', extractions, invert=F, value=T) #only working on peru

# message("Subset to Africa")
# stages <- read.csv(paste0(j, "temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F)
# africa <- stages[stages$Stage == 1, "alpha.3"]
# africa <- paste0(africa, collapse="|")
# 
# files <- grep(africa, files, value=T)

message("Read in IPUMS Geo Codebook")
geo <- read.csv(paste0(j, "/WORK/11_geospatial/05_survey shapefile library/codebooks/IPUMS_CENSUS.csv"), stringsAsFactors = F, encoding="windows-1252")
geo <- geo[, c("nid", "iso3", 'shapefile', 'location_code', 'lat', 'long', 'admin_level', 'point', 'geospatial_id', 'start_year', 'end_year')]

#TODO add the same functionality in the regular post extract of saving a list of broken/problem extractions
#needs to be setup within the loop to return this information serially

#define function to merge IPUMS files with geographies
ipums_merge <- function(file, geo, folder_out, noms){
  
  dt <- fread(file)

  #get survey info
  nid <- dt$nid[1] %>% as.character
  message(nid)
  iso3 <- dt$ihme_loc_id[1] %>% as.character
  year_start <- dt$year_start[1] %>% as.character
  year_end <- dt$year_end[1] %>% as.character
  survey_module <- dt$survey_module[1] %>% as.character
  
  #skip bad data
  has_pweight_not_hhweight <- "pweight" %in% names(dt) & !("hhweight" %in% names(dt))
  missing_all_weights <- !("pweight" %in% names(dt)) & !("hhweight" %in% names(dt))
  missing_gid <- !("geospatial_id" %in% names(dt))
  missing_hap <- !("cooking_fuel" %in% names(dt)) & !(grepl('housing', names(dt)) %>% any)
  if (has_pweight_not_hhweight){
    setnames(dt, "pweight", "hhweight")
  }
  if (missing_all_weights | missing_hap | missing_gid){
    return(nid)
  }

  if (!missing_gid) dt[, geospatial_id := as.character(geospatial_id)] #force geospatial IDs to character to match the sheet
  m <- try(merge(dt, geo, by.x=c("nid", "ihme_loc_id", 'geospatial_id'), by.y=c("nid", 'iso3', 'geospatial_id'), all.x=T))

  if (class(m) == "try-error"){
    message(paste("Check try error", nid))
    return(nid)
  } else{
    outname <- paste("IPUMS_CENSUS", nid, survey_module, iso3, year_start, year_end, sep="_")
    m$survey_series <- m$survey_name
    m$iso3 <- m$ihme_loc_id
    orig_names <- names(m)
    new_names <- noms[!(noms %in% orig_names)]
    for (nam in new_names){
      m[, nam] <- NA
    }
    #m <- m[, c("nid", 'survey_series', "year_start", "year_end", "iso3", "lat", "long", "shapefile", "location_code", "w_source_drink", "t_type", "sewage", "shared_san", "mins_ws", "dist_ws", "hw_station", "hw_water", "hw_soap", "hhweight", "hh_size", 'urban')]
    has_shp <- any(!is.na(m$shapefile)) & any(!is.na(m$location_code))
    has_lat_long <- any(!is.na(m$lat)) & any(!is.na(m$long))
    has_geography <- has_shp | has_lat_long
    if (!has_geography){
      message(paste("Check geography", nid, year_end))
      return(nid)
    }

    out <- paste0(folder_out, "/", outname, ".feather")
    write_feather(m, out)
    return(NULL)
  }
}

# need_some_love <- "106512"
# files <- grep(need_some_love, files, value=T)

message("starting mclapply")
bad_nids <- mclapply(extractions, ipums_merge, geo=geo, folder_out=folder_out, noms=noms, mc.cores=cores) %>% unlist
write.csv(bad_nids, paste0(folder_out, "/fix_these_nids.csv"), row.names = F, na='')
  #write.csv(to_do, to_do_outpath, row.names=F)

