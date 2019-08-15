// *********************************************************************************************************************************************************************
// *********************************************************************************************************************************************************************
// Author: 		Joey Frostad
// Date: 			Dec 3rd, 2013
// Modified:		--
// Project:		RISK
// Purpose:		Compile all blood lead data to send to Annette
// Source: 		do /homes/jfrostad/_code/risks/envir_lead_blood/exp/02_split.do

** **************************************************************************
** RUNTIME CONFIGURATION
** **************************************************************************
// Set preferences for STATA
	// Clear memory and set memory and variable limits
		clear all
		set maxvar 32000
	// Set to run all selected code without pausing
		set more off
	// Set graph output color scheme
		set scheme s1color
	// Remove previous restores
		capture restore, not
	// Define J drive (data) for cluster (UNIX) and Windows (Windows)
		if c(os) == "Unix" {
			global prefix "/home/j"
			set odbcmgr unixodbc
		}
		else if c(os) == "Windows" {
			global prefix "J:"
		}
		
// Close previous logs
	cap log close
	
// Create timestamp for logs
	local c_date = c(current_date)
	local c_time = c(current_time)
	local c_time_date = "`c_date'"+"_" +"`c_time'"
	display "`c_time_date'"
	local time_string = subinstr("`c_time_date'", ":", "_", .)
	local timestamp = subinstr("`time_string'", " ", "_", .)
	display "`timestamp'"
	
// Settings
local central_root "$prefix/WORK/01_covariates/common/ubcov_central"
local topics "hap"
 
// Load functions
cd "`central_root'"
do "`central_root'/modules/extract/core/load.do"
	
// Set to log
	log using "`logs'/`iso3'_age_sex_split_`timestamp'.log", replace
	
** **************************************************************************
** CREATE A DATASET TO INFORM AGE-TREND IN BRADMOD
** **************************************************************************	

// Initialize the system
/*
    Brings in the databases, after which you can run
    extraction or sourcing functions like: new_topic_rows
 
    You can view each of the loaded databases by running: get, *db* (eg. get, codebook)
*/
 
init, topics(`topics')

// Run extraction
/* Launches extract
 
    Arguments:
        - ubcov_id: The id of the codebook row
    Optional:
        // General
        - keep: Keeps all original survey variables (useful for checking)
        - bypass: Skips the extraction check
        - run_all: Loops through all ubcov_ids for the topic
        // Value Mapping
        - bypass_map: Skips the subnational/value mapping process
        - store_vals: Stores unmapped values into a file on J:/WORK/01_covariates/common/ubcov_library/extract/topics/value_maps
        // Collapsing
        - collapse_config: Path to config file for collapse
*/
//local ubcov_ids 9 10 24 25 26 27 28 29 30 31
local ubcov_ids 5858

run_extract `ubcov_ids'
