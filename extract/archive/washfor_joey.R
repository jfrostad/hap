#source("/snfs2/HOME/gmanny/backups/Documents/Repos/geospatial-data-prep/common/wash_specific_post_extract.R")
message("dropping duplicate women in single household (so household sizes aren't duplicated)")
all[is.na(hhweight) & !is.na(pweight), hhweight := pweight]
#"hw_soap1", "hw_soap2", "hw_soap3"
#custom fix for mics handwash soap
all[hw_soap1 == 0, hw_soap := 0]
all[hw_soap2 == 0, hw_soap := 0]
all[hw_soap3 == 0, hw_soap := 0]
all[hw_soap1 == 1, hw_soap := 1]
all[hw_soap2 == 1, hw_soap := 1]
all[hw_soap3 == 1, hw_soap := 1]

drop <- c("line_id", "sex_id", "age_year", "age_month", "age_day", "pweight", "cooking_fuel_mapped", "w_treat", "w_filter", "w_boil", "w_bleach", "nid_n", "year", "t_type_multi", "w_solar", "w_cloth", "w_settle", "hw_soap1", "hw_soap2", "hw_soap3")
all <- all[, (drop):= NULL]
wn <- all[survey_module == "WN", ]
wn_key <- c("psu", "hh_id")
wn <- distinct(wn, psu, hh_id, .keep_all=T)
all <- all[survey_module != "WN", ]
all <- rbind(all, wn, fill=T, use.names=T)

message("custom wash fixes")
drop <- c("latitude", "longitude")
all <- all[, (drop):=NULL]

#fix pma water and soap obs custom var
all <- all[is.na(hw_water), hw_water := hw_water_pma]
all <- all[is.na(hw_soap), hw_soap := hw_soap_pma]

#reduce number of time and distance to water variables
#set correct data types
all <- all[, mins_ws := as.numeric(mins_ws)]
all <- all[, mins_queue_plus_trip := as.numeric(mins_queue_plus_trip)]
all <- all[, mins_queue_ws := as.numeric(mins_queue_ws)]

#set NA queue times to 0
all <- all[!is.na(mins_ws) & is.na(mins_queue_ws), mins_queue_ws := 0]
#set NA time to water units to correct unit
all <- all[is.na(mins_ws_unit), mins_ws_unit := mins_ws_manual_unit]
#double one-way trip times
all <- all[!is.na(mins_ws) & mins_ways == 1, mins_ws := 2*mins_ws]
all <- all[!is.na(mins_queue_plus_trip) & mins_ways == 1, mins_queue_plus_trip := 2*mins_queue_plus_trip]
all <- all[!is.na(mins_ws) & mins_ways == 1, mins_ways := 2]
all <- all[!is.na(mins_queue_plus_trip) & mins_ways == 1, mins_ways := 2]
#fix known unit differences
all <- all[!is.na(mins_queue_unit) & !is.na(mins_ws_unit) & mins_ws_unit != mins_queue_unit & mins_ws_unit == "Hour", mins_ws := 60*mins_ws]
all <- all[!is.na(mins_queue_unit) & !is.na(mins_ws_unit) & mins_ws_unit != mins_queue_unit & mins_ws_unit == "Hour", mins_ws_unit := "Minute"]
all <- all[!is.na(mins_queue_unit) & !is.na(mins_ws_unit) & mins_ws_unit != mins_queue_unit & mins_queue_unit == "Hour", mins_queue_ws := 60*mins_queue_ws]
all <- all[!is.na(mins_queue_unit) & !is.na(mins_ws_unit) & mins_ws_unit != mins_queue_unit & mins_queue_unit == "Hour", mins_queue_unit := "Minute"]

#document known issues with trip and queue time
unit_problems <- all[!is.na(mins_queue_unit) & !is.na(mins_ws_unit) & mins_ws_unit != mins_queue_unit, list(nid, mins_queue_unit, mins_ws_unit)]
if (nrow(unit_problems) > 0){
  #alert if temporal units are different
  message(paste("there are time-to-water issues with nids", unique(unit_problems$nid), collapse=" "))
  write.csv(unit_problems, "/snfs1/temp/gmanny/wash_water_trip_unit_issues.csv", row.names=F)
}

make_negtive <- function(number){
  if (number < 0){
    return(number)
  } else{
    return(number*-1)
  }
}

#when units are equal, sum queue and trip time together
all <- all[, time_to_source := mins_queue_plus_trip]
#sum positive numbers together
all <- all[!is.na(mins_ws) & !is.na(mins_ws_unit) & (mins_ws_unit == mins_queue_unit | is.na(mins_ws_unit) | is.na(mins_queue_unit)) & mins_ws > 0 & mins_queue_ws > 0, time_to_source := mins_ws + mins_queue_ws]
#sub negative numbers together
all <- all[!is.na(mins_ws) & !is.na(mins_ws_unit) & mins_ws_unit == mins_queue_unit & mins_ws < 0 | mins_queue_ws < 0, time_to_source := make_negative(mins_ws) + make_negative(mins_queue_ws)]

all <- all[!is.na(mins_ws) & !is.na(mins_queue_ws) & is.na(mins_ws_unit), time_to_source := mins_ws + mins_queue_ws]

#fix missing PMA minutes to water source values
all[grepl("PMA2020", survey_name) & (mins_ws == -88 | mins_ws == -99), mins_ws := NA]
all[grepl("PMA2020", survey_name) & (mins_queue_plus_trip == -88 | mins_queue_plus_trip == -99), mins_queue_plus_trip := NA]

#recalc distances to water

message("drop duplicate HH entries and cleanup hh_sizes")
####
# 0. Set hh_size values to NA for nid 157397 7438
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
pt_collapse[, year_experiment := start_year]
pt_collapse[!is.na(int_year), year_experiment := int_year]
save(pt_collapse, file=paste0(folder_out, "/points_", today, ".Rdata"))


message("saving polygons")
poly_collapse <- packaged[(is.na(lat) | is.na(long)) & !is.na(shapefile) & !is.na(location_code), ]
#set polygon years to a weighted mean
poly_collapse[, year_experiment := start_year]
poly_collapse[, year_experiment := weighted.mean(int_year, weight=hhweight, na.rm=T), by=c("nid")]
save(poly_collapse, file=paste0(folder_out, "/poly_", today, ".Rdata"))


library(feather)
message("Point Feather")
write_feather(pt_collapse, path=paste0(folder_out, "/points_", today, ".feather"))
message("Poly Feather")
write_feather(poly_collapse, path=paste0(folder_out, "/poly_", today, ".feather"))
