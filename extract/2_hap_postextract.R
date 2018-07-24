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
#####################################################################

#####################################################################
############################## SETUP ################################
#####################################################################
rm(list=ls())

#Define values
topic <- "hap"
cluster <- TRUE #running on cluster true/false
geos <- FALSE #running on geos nodes true/false
cores <- 10
#FOR THE CLUSTER:
#qlogin -now n -pe multi_slot 5 -P proj_geospatial -l geos_node=TRUE
#source('/snfs2/HOME/gmanny/backups/Documents/Repos/geospatial-data-prep/common/post_extraction_3.R')

#Setup
j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")
h <- ifelse(Sys.info()[1]=="Windows", "H:/", "/homes/jfrostad/")
folder_in <- paste0(j, "LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/", topic) #where your extractions are stored
folder_out <- paste0(j, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/", topic) #where you want to save the big csv of all your extractions together

####### YOU SHOULDN'T NEED TO CHANGE ANYTHING BELOW THIS LINE. SORRY IF YOU DO ##################################################

today <- Sys.Date() %>% gsub("-", "_", .)
stages <- read.csv(paste0(j, "temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F)
package_lib    <- sprintf('%s_code/_lib/pkg', h)
## Load libraries and  MBG project functions.
.libPaths(package_lib)

#Load packages
packages <- c('haven', 'stringr', 'data.table', 'dplyr', 'magrittr', 'parallel', 'doParallel')
pacman::p_load(packages)
packages <- lapply(packages, library, character.only=T)
#haven to read in stata
#data.table for speed & syntax
#dplyr for distinct (strange behavior from data.table unique)
#parallel for mclapply
#doParallel for parallelized foreach


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
#Generate list of extraction filepaths
extractions <- list.files(folder_in, full.names=T, pattern = ".dta", ignore.case=T, recursive = F)
extractions <- grep("IPUMS_CENSUS", extractions, invert=T, value = T) #IPUMS is handled separately
extractions <- grep("234353|233917", extractions, invert=T, value=T)
#234353 is a massive India dataset that slows everything down and gets us killed on the cluster. It is handled separately.
#233917 is another IND survey that isn't quite as large but it also has to be loaded and collapsed separately.

#append all ubcov extracts together
if(cluster == TRUE) {
  message("Make cluster")
  cl <- makeCluster(cores)
  message("Register cluster")
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  registerDoParallel(cl)
  message("Start foreach")
  #Read in each .dta file in parallel - returns a list of data frames
  top <- foreach(i=1:length(extractions), .packages="haven") %dopar% {
    dta <- read_dta(extractions[i], encoding = 'latin1')
  }
  message("Foreach finished")
  message("Closing cluster")
  stopCluster(cl)
} else if(cluster == FALSE) {
  top <- foreach(i=1:length(extractions)) %do% {
    message(paste0("Reading in: ", extractions[i]))
    dta <- read_dta(extractions[i])
    return(dta)
  }
}

message("rbindlist all extractions together")
topics <- rbindlist(top, fill=T, use.names=T)
rm(top)

##Save raw data file, if desired
#save(topics, file=paste0(folder_out, "/topics_no_geogs_", today, ".Rdata"))

#####################################################################
######################## PULL IN GEO CODEBOOKS ######################
#####################################################################
#Get all geog codebooks and package them together
message("Retrieve geo codebook filepaths")
files <- list.files(paste0(j, "WORK/11_geospatial/05_survey shapefile library/codebooks"), pattern=".csv$", ignore.case = T, full.names = T)
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

all[, start_year := year_start]
all[, year_dummy := start_year]
all[, year_experiment := year_dummy]

all[, year_experiment := round(mean(x=year_dummy, na.rm=T)), by=.(nid, iso3)]

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

if (topic == "wash"){
  message("WaSH-specific Fixes")
  file.path(h, "_code/lbd/hap/extract/hap_custom_postextract.R") %>% source
}

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
fix_outpath <- paste0(j, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/", topic, "/new_geographies_to_match.csv")
write.csv(fix_collapse, fix_outpath, row.names=F, na="")
