#source("/snfs2/HOME/gmanny/backups/Documents/Repos/geospatial-data-prep/common/wash_specific_post_extract.R")

#replace missing pweights with hhweight
all[is.na(hhweight) & !is.na(pweight), hhweight := pweight]

#drop useless vars
drop <- c("line_id", "sex_id", "age_year", "age_month", "pweight", "latitude", "longitude")
all <- all[, (drop):= NULL]

#cleanup women module
message("dropping duplicate women in single household (so household sizes aren't duplicated)")
wn <- all[survey_module == "WN", ]
wn_key <- c("psu", "hh_id")
wn <- distinct(wn, psu, hh_id, .keep_all=T)
all <- all[survey_module != "WN", ]
all <- rbind(all, wn, fill=T, use.names=T)

message("drop duplicate HH entries and cleanup hh_sizes")
####
# 0. Set hh_size values to NA for nid 157397 7438
#TODO look further into this and why just these NIDs
nids_without_unique_hh_ids <- c(157397, 7438, 24915)
all[nid %in% nids_without_unique_hh_ids, hh_size := NA]
# 1. separate NA hh_size values from dataset

#drop data that doesn't need a hh_size crosswalk and that has NA hh_sizes
#all <- all[!is.na(hh_size) & !is.na(t_type) & !is.na(w_source_drink) & !(nid %in% nids_that_need_hh_size_crosswalk), ]

#create indicator for hh_size missingness
all[, missingHHsize := sum(is.na(hh_size)), by=nid]
all[, obs := .N, by=nid]
all[, pct_miss_hh_size := 100 * missingHHsize / obs]
all[, is_hh := pct_miss_hh_size > 0]

#subset cases where all hh_sizes are present. in these, each row is a HH
has_hh_size <- all[pct_miss_hh_size > 0, ]
has_hh_size[, uq_id := paste(nid, psu, hh_id, year_start, lat, long, shapefile, location_code, sep="_")] #includes space-time
has_hh_size[, prev_uq_id := paste(nid, psu, hh_id, sep="_")]
diff <- length(unique(has_hh_size$uq_id)) - length(unique(has_hh_size$prev_uq_id))
message(paste("There are", diff, "more unique households from including spacetime than excluding."))
hhhs <- distinct(has_hh_size, uq_id, .keep_all=T)

#subset cases where all hh_sizes are missing. these are HHM
missing_hh_size <- all[pct_miss_hh_size <= 0, ]
missing_hh_size[, hh_size := 1]

packaged <- rbind(hhhs, missing_hh_size, fill=T)

nids_that_need_hh_size_crosswalk <- c(20998, #MACRO_DHS_IN UGA 1995 WN
                                      32144, 32138, 1301, 1308, 1322, #BOL/INTEGRATED_HH_SURVEY_EIH
                                      7375) # KEN 2007 Household Health Expenditure Utilization Survey KEN/HH_HEALTH_EXPENDITURE_UTILIZATION_SURVEY
packaged[nid %in% nids_that_need_hh_size_crosswalk, hh_size := NA]

excluded_surveys <- c(8556, #dropping MEX/NATIONAL_HEALTH_SURVEY_ENSA due to bad weighting
                      261889, 261887) #MAL_ED due to non-representative sample from hospital visits
packaged <- packaged[!(nid %in% excluded_surveys),]

message("saving points")
pt_collapse <- packaged[!is.na(lat) & !is.na(long), ]
#set start_year to int_year for point data
pt_collapse[, year_experiment := year_start]
pt_collapse[!is.na(int_year), year_experiment := int_year]
save(pt_collapse, file=paste0(folder_out, "/points_", today, ".Rdata"))


message("saving polygons")
poly_collapse <- packaged[(is.na(lat) | is.na(long)) & !is.na(shapefile) & !is.na(location_code), ]
#set polygon years to a weighted mean
poly_collapse[, year_experiment := weighted.mean(int_year, weight=hhweight, na.rm=T), by=c("nid")]
save(poly_collapse, file=paste0(folder_out, "/poly_", today, ".Rdata"))


library(feather)
message("Point Feather")
write_feather(pt_collapse, path=paste0(folder_out, "/points_", today, ".feather"))
message("Poly Feather")
write_feather(poly_collapse, path=paste0(folder_out, "/poly_", today, ".feather"))
