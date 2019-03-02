#####################################################################
# POST UBCOV EXTRACTION DATA CLEANING FOR GEOSPATIAL DATA EXTRACTIONS & GEOGRAPHY MATCHING
# PIONEERED BY ANNIE BROWNE
# UPDATED & OVERHAULED BY MANNY GARCIA
# STANDARDIZED BY SCOTT SWARTZ
# EMAIL ABROWNE@WELL.OX.AC.UK
# EMAIL GMANNY@UW.EDU
# EMAIL SSWARTZ@UW.EDU

# INSTRUCTIONS:
# UBCOV OUTPUTS MUST BE SAVED IN LIMITED USE DIRECTORY
# source('/homes/jfrostad/_code/lbd/hap/extract/2_hap_postextract.R')
#####################################################################

#####################################################################
############################## SETUP ################################
#####################################################################
rm(list=ls())

#Define values
topic <- "hap"
extractor_ids <- c('jfrostad', 'qnguyen1', 'albrja')
redownload <- T #update the codebook from google drive
cluster <- T #running on cluster true/false
geos <- T #running on geos nodes true/false
cores <- 25
#FOR THE CLUSTER:
#qlogin -now n -pe multi_slot 5 -P proj_geospatial -l geos_node=TRUE
#source('/snfs2/HOME/gmanny/backups/Documents/Repos/geospatial-data-prep/common/post_extraction_3.R')

#Setup
j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j")
h <- ifelse(Sys.info()[1]=="Windows", "H:/", file.path("/ihme/homes", Sys.info()["user"])) #Your username
l <- ifelse(Sys.info()[1]=="Windows", "L:/", "/ihme/limited_use/") 
folder_in <- file.path(l, "LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions", topic, 'batch') #where your extractions are stored
folder_out <- file.path(l, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/", topic) #where you want to save the big csv of all your extractions together
setwd(folder_in)

package_lib    <- file.path(h, '_code/_lib/pkg')
## Load libraries and  MBG project functions.
.libPaths(package_lib)

####### YOU SHOULDN'T NEED TO CHANGE ANYTHING BELOW THIS LINE. SORRY IF YOU DO ##################################################
#Load packages
pacman::p_load(haven, stringr, data.table, dplyr, magrittr, feather, parallel, doParallel, googledrive, readxl)

#timestamp
today <- Sys.Date() %>% gsub("-", "_", .)

#TODO update to a new stage csv??
stages <- file.path(j, "/temp/gmanny/geospatial_stages_priority.csv") %>% fread
#####################################################################
######################## DEFINE FUNCTIONS ###########################
#####################################################################
#Read in geo codebook, add column with corresponding survey series
read_add_name_col <- function(file){
  #FOR GEOGRAPHY CODEBOOKS. READS THEM IN AND ADDS A COLUMN WITH THEIR CORRESPONDING SURVEY_SERIES
  message(file)
  rn <- gsub(".csv", "", file, ignore.case=T)
  spl <- strsplit(rn, "/") %>% unlist()
  svy <- spl[length(spl)]
  df <- read.csv(file, encoding="windows-1252", stringsAsFactors = F) #this encoding scheme plays nice with the default excel format
  df <- as.data.table(df)
  df[, survey_series := svy]
  df <- lapply(df, as.character, stringsAsFactors = FALSE)
  return(df)
}

#####################################################################
######################## BIND UBCOV EXTRACTS ########################
#####################################################################
#Change to handle batch extractions by only reading in those IDs that have been extracted by Queenie
if(redownload==T) drive_download(as_id('1Nd3m0ezwWxzi6TmEh-XU4xfoMjZLyvzJ7vZF1m8rv0o'), overwrite=T)
codebook <- read_xlsx('hap.xlsx', sheet='codebook') %>% as.data.table

#subset codebook based on outliers, or files that are too large to handle here
#census is handled separately
codebook <- codebook[!(survey_name %like% 'IPUMS')]

#234353 is a massive India dataset that slows everything down and gets us killed on the cluster. It is handled separately.
#233917 is another IND survey that isn't quite as large but it also has to be loaded and collapsed separately.
#23219 is the same, causing our feather reads to crash.
#157050 is IND DHS, size issues
#TODO split the extracts in order to fix the feather reading issue
codebook <- codebook[!(nid %in% c("234353", "233917", "23219"))]
codebook <- codebook[nid != 157050]

#angola mics based on negative comment about its quality in response to EBF paper (see dia_lri_modelers slack chat) 
#TODO better to handle using outlier column with notes in the data vetting sheet
codebook <- codebook[nid != 687]

#create output name, note that we need to remove the leading info on some of the survey names(take only str after /)
codebook[, output_name := paste0(ihme_loc_id, '_', tools::file_path_sans_ext(basename(survey_name)), '_', year_start, '_', year_end, '_', nid, '.csv')]
#subset the codebook to ONLY the files that our extractors have worked on
codebook <- codebook[assigned %in% extractor_ids]
#get the names of all the new files
new.files <- codebook[, output_name] %>% unique %>% paste(., collapse="|")

#Generate list of extraction filepaths and crosscheck against the new files
extractions <- list.files(folder_in, full.names=T, pattern = ".csv", ignore.case=T, recursive = F)
extractions <- grep(new.files, extractions, invert=F, value=T)

#append all ubcov extracts together
if(cluster == TRUE) {
  message("Make cluster")
  cl <- makeCluster(cores)
  message("Register cluster")
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  registerDoParallel(cl)
  message("Start foreach")
  #Read in each .dta file in parallel - returns a list of data frames
  top <- foreach(i=1:length(extractions), .packages="data.table") %dopar% {
    dt <- fread(extractions[i])
  }
  message("Foreach finished")
  message("Closing cluster")
  stopCluster(cl)
} else if(cluster == FALSE) {
  top <- foreach(i=1:length(extractions)) %do% {
    message(paste0("Reading in: ", extractions[i]))
    dt <- fread(extractions[i])
    return(dt)
  }
}

message("rbindlist all extractions together")
topics <- rbindlist(top, fill=T, use.names=T)
rm(top)
gc()

##Save raw data file, if desired
#TODO need to split this file because its getting the null embedded error too
write_feather(topics, path=paste0(folder_out, "/topics_no_geogs_", today, ".feather"))

#####################################################################
######################## VERIFY EXTRACTIONS##########################
#####################################################################
#Double check your compiled extractions against the codebook to make sure 
#that all variables are being extracted as expected.
#Return CSV with errors for each NID

#also return a list of all the NIDs that are present in the codebook but not in the extracted topics
#subset to make sure they are not stage3 or <2000
codebook.nids <- codebook[!(year_end < 2000 | ihme_loc_id %in% stages[Stage==3, alpha.3]), nid] %>% unique
broken_extractions <- codebook.nids[!(codebook.nids %in% unique(topics$nid))]
write.csv(broken_extractions, paste0(folder_out, "/broken_extractions.csv"), na="", row.names=F)

#make a vector of the expected variables
var.list <- c('cooking_fuel', 'cooking_location', 'cooking_type', 'cooking_type_chimney',
              'heating_fuel', 'heating_type', 'heating_type_chimney', 'lighting_fuel', 
              'electricity',
              'housing_roof', 'housing_wall', 'housing_floor',
              'housing_roof_num', 'housing_wall_num', 'housing_floor_num')

#summarize the codebook and extracted topics inversely
#for the topics, identify any columns in our list that are entirely missing
topics.miss <- topics[, lapply(.SD, function(x) is.na(x) %>% all), .SDcols=var.list, by='nid']
#for the codebooks, identify any columns in our list that are present
#note that we subset by those NIDs that didnt fail to extract, because obviously those are missing
codebook.prez <- codebook[!(nid %in% broken_extractions), lapply(.SD, function(x) !is.na(x)), .SDcols=var.list, by='nid']

#bind together these two tables and then summarize using the product of each NID
#this will have the effect of returning true for any NIDs that were present in the codebook but missing in the topics
verification <- rbindlist(list(topics.miss,codebook.prez))[, lapply(.SD, prod, na.rm = TRUE), .SDcols=var.list, by='nid']
verification[, failures := rowSums(.SD), .SDcols=var.list]
#also merge on the notes column from the codebook, to help ID cases where there was missingess in the raw data
verification <- merge(verification, codebook[, .(nid, notes)], by='nid', all.x=T)
write.csv(verification[failures>0], paste0(folder_out, "/problem_extractions.csv"), na="", row.names=F)

#####################################################################
######################## PULL IN GEO CODEBOOKS ######################
#####################################################################
#Get all geog codebooks and package them together
message("Retrieve geo codebook filepaths")
files <- list.files(paste0(j, "/WORK/11_geospatial/05_survey shapefile library/codebooks"), pattern=".csv$", ignore.case = T, full.names = T)
files <- grep("IPUMS|special", files, value = T, invert = T) # list any strings from geo codebooks you want excluded here

message("Read geo codebooks into list")
geogs <- lapply(files, read_add_name_col)

message("Append geo codebooks together")
geo <- rbindlist(geogs, fill=T, use.names=T)
geo[is.na(admin_level), admin_level := "NA"] #set NA values for admin_level to "NA" as a string to keep the following line from dropping them because of bad R logic
geo <- geo[admin_level != "0", ] #drop anything matched to admin0
geo <- geo[survey_module != "GSEC1"] #drop this geomatch which is creating a m:m mismatch on different keys issue
rm(geogs)

#Dedupe the geography codebook by geospatial_id, iso3, and nid
geo <- distinct(geo, nid, iso3, geospatial_id, .keep_all=T)

#coerce lat/longs to numeric
geo <- geo[, lat := as.numeric(lat)]
geo <- geo[, long := as.numeric(long)]

#####################################################################
######################## PREP DATA FOR MERGE ########################
#####################################################################
#Reconcile ubCov & geo codebook data types
message("make types between merging datasets match")
if (class(topics$nid) == "numeric"){
  geo[, nid := as.numeric(nid)]
} else if (class(topics$nid) == "integer"){
  geo[, nid := as.integer(nid)]
} else if (class(topics$nid) == "character"){
  geo[, nid := as.character(nid)]
} else{
  message("update code to accomodate topics nid as")
  message(class(topics$nid))
}

#Drop unnecessary geo codebook columns
geo_keep <- c("nid", "iso3", "geospatial_id", "point", "lat", "long", "shapefile", "location_code", "survey_series")
geo_k <- geo[, geo_keep, with=F]
## If the merge returns an 'allow.cartesian' error, we've likely over-dropped characters - contact Scott Swartz to address it ##

#####################################################################
############################### MERGE ###############################
#####################################################################

message("Merge ubCov outputs & geo codebooks together")
names(topics)[names(topics) == 'ihme_loc_id'] <- 'iso3'
geo_k$geospatial_id <- as.character(geo_k$geospatial_id)
topics$geospatial_id <- as.character(topics$geospatial_id)
all <- merge(geo_k, topics, by.x=c("nid", "iso3", "geospatial_id"), by.y=c("nid", "iso3", "geospatial_id"), all.x=F, all.y=T)
all[iso3 == "KOSOVO", iso3 := "SRB"] #GBD rolls Kosovo data into Serbia

#####################################################################
############################### MERGE DIAGNOSTIC ####################
#####################################################################

geo_nids <- unique(geo$nid)
topic_nids <- unique(topics$nid)
merged_correctly <- all[(!is.na(shapefile) & !is.na(location_code)) | (!is.na(lat) & !is.na(long)),]
merged_nids <- unique(merged_correctly$nid)

missing_nids <- topic_nids[(topic_nids %in% geo_nids) & !(topic_nids %in% merged_nids)]
if (length(missing_nids) > 0){
  message(paste("Writing csv of the", length(missing_nids), "surveys that are not properly merging"))
  merge_issues <- all[nid %in% missing_nids, .(nid, iso3, survey_name)] %>% distinct
  merge_issues <- merge(merge_issues, stages, by.x="iso3", by.y="alpha.3", all.x=T)
  write.csv(merge_issues, paste0(folder_out, "/merge_issues.csv"), na="", row.names=F)
} else{
  message("All nids merged correctly. You are so thorough.")
  #Once R can handle unicode please add the clap emoji to this message.
}

#####################################################################
######################### MAKE year_experiment COLUMN ###############
#####################################################################

message("Adding year_experiment column")

all[, year_experiment := round(mean(year_start, na.rm=T)), by=.(nid, iso3)]

all[(!is.na(int_year) & int_year <= year_start+5 & int_year >= year_start), year_experiment := round(mean(int_year, na.rm=T)), by=c("nid", "iso3")]

#make point-level clusters annually representative of themselves
all[!is.na(lat) & !is.na(long) & !is.na(int_year) & int_year <= year_start+5 & int_year >= year_start, year_experiment := round(mean(x=int_year, na.rm=T)), by=.(nid, iso3, lat, long)]

bad_year_nids <- unique(all[is.na(year_experiment), nid])
for (bad_nid in bad_year_nids){
  message(bad_nid)
  only_year <- unique(all[nid==bad_nid, year_experiment])
  only_year <- only_year[!is.na(only_year)]
  all[nid == bad_nid, year_experiment := only_year]
}
message("if a table longer than 0 rows appears here diagnose issues with year_experiment")
unique(all[is.na(year_experiment), .(nid, iso3)])
message("end of table")

#####################################################################
######################### TOPIC-SPECIFIC CODE #######################
#####################################################################

if (topic == "hap"){
  message("HAP-specific Fixes")
  #accomodating my file structure
  #TODO make more flexible
  if (h %like% 'jfrostad') {
    
    file.path(h, "_code/lbd", "hap/extract/2a_hap_custom_postextract.R") %>% source
    
  } else file.path(h, "hap/extract/2a_hap_custom_postextract.R") %>% source
}
#File path where this is located in your repo.

#####################################################################
######################### CLEAN UP & SAVE ###########################
#####################################################################

##Fill in lat & long from ubCov extracts, if present
#all[!is.na(latitude) & is.na(lat), lat := latitude]
#all[!is.na(longitude) & is.na(long), long := longitude]

#Save
message("Saving as .Rdata")
save(all, file=paste0(folder_out, "/", today, ".Rdata"))
#message("Saving as .csv")
#write.csv(all, file=paste0(folder_out, "/", today, ".csv"))

#Create & export a list of all surveys that have not yet been matched & added to the geo codebooks
message("Exporting a list of surveys that need to be geo matched")
gnid <- unique(geo$nid)
fix <- subset(all, !(all$nid %in% gnid))
fix_collapse <- distinct(fix[,c("nid", "iso3", "year_start", "survey_name"), with=T])
fix_collapse <- merge(fix_collapse, stages, by.x="iso3", by.y="alpha.3", all.x=T)
fix_outpath <- paste0(l, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/", topic, "/new_geographies_to_match.csv")
write.csv(fix_collapse, fix_outpath, row.names=F, na="")
