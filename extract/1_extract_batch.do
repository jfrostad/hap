/***********************************************************************************************************
 Author: Patrick Liu (pyliu@uw.edu) edited by Manny Garcia (gmanny@uw.edu)                                                                  
 Date: 7/13/2015
 Project: ubCov
 Purpose: Run Script
***********************************************************************************************************/
// do /homes/jfrostad/_code/lbd/housing/extract/1_extract_batch.do

//////////////////////////////////
// Setup
//////////////////////////////////

if c(os) == "Unix" {
    local j "/home/j"
    set odbcmgr unixodbc
}
else if c(os) == "Windows" {
    local j "J:"
}

clear all
set more off
set obs 1

// Settings
local central_root "`j'/WORK/01_covariates/common/ubcov_central"
// local central_root "/homes/jfrostad/_code/ubcov_central"
local topics hap //ENTER YOUR TOPIC HERE
local username qnguyen1

// Load functions
cd "`central_root'"
do "`central_root'/modules/extract/core/load.do"

// Load the base code for ubCov
local paths  `central_root'/modules/extract/core/ `central_root'/modules/extract/core/addons/
foreach path in `paths' {
    local files : dir "`path'" files "*.do"
    foreach file in `files' {
        if "`file'" != "run.do" do "`path'/`file'"
    }
}

// Make sure you're in central
cd `central_root'

// Initialize the system
/* 
    Brings in the databases, after which you can run
    extraction or sourcing functions like: new_topic_rows

    You can view each of the loaded databases by running: get, *db* (eg. get, codebook)
*/

ubcov_path
init, topics(`topics')
// Run extraction
/* Launches extract
    Arguments:
        - ubcov_id: The id of the codebook row
    Optional:
        - keep: Keeps 
        - bypass: Skips the extraction check
        - run_all: Loops through all ubcov_ids in the codebook.
*/
//Enter path where you'd like your extracts saved below between the quotes
//DO NOT EXTRACT DATA FROM LIMITED USE TO THE J DRIVE. YOU CAN GET IN BIG TROUBLE. If you need to extract from LIMITED_USE contact IT and your PO to organize a workaround. 
local outpath = "`L'/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/hap/batch"
local thisvar = "cooking_fuel_mapped"

   get, vars
    levelsof var_name if topic_name == "`topics'", l(vars) clean
    local n : list sizeof vars
    get, codebook
    egen keep = rowmiss(`vars')
    keep if keep < `n'

     batch_extract, topics(`topics') ubcov_ids(`ubcov_ids') /// 
                 central_root(`central_root') ///
                 cluster_project(proj_geo_nodes) ///
                 output_path("`outpath'") ///
                 store_vals_path("/share/temp/sgeoutput/`c(username)'") ///
                 logs_path("/share/temp/sgeoutput/`c(username)'") ///
                 run_log_path("/share/temp/sgeoutput/`c(username)'") ///
                 db_path("/share/temp/sgeoutput/`c(username)'") 
