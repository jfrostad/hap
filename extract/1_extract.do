/***********************************************************************************************************
 Author: Patrick Liu (pyliu@uw.edu) edited by Manny Garcia (gmanny@uw.edu)                                                                  
 Date: 7/13/2015
 Project: ubCov
 Purpose: Run Script
***********************************************************************************************************/
// do /homes/jfrostad/_code/lbd/hap/extract/1_extract.do

//////////////////////////////////
// Setup
//////////////////////////////////

if c(os) == "Unix" {
    local j "/home/j"
    local l "/share/limited_use/LIMITED_USE"
    set odbcmgr unixodbc
}
else if c(os) == "Windows" {
    local j "J:"
    local l "L:/LIMITED_USE"
}

clear all
set more off
set obs 1

// Settings
local central_root "`j'/WORK/01_covariates/common/ubcov_central"
local topics hap //ENTER YOUR TOPIC HERE

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
local outpath = "`l'/LU_GEOSPATIAL/ubCov_extractions/hap/"

//Troublemakers
local array 4514 4664 4962 5801 4549 //come back to this one: error=variables ihme_loc_id admin_1 do not uniquely identify observations in the using data 
local array 5110 // come back to this one: error=variable hv109 not found
local array 4723 // come back to this one: error=Indicator Check || Requisite input vars (sweight) do not exist in dataset || design || pweight_admin_1 (cont)
local array 4651 // come back to this one: error=Indicator Check || Requisite input vars (hv239) do not exist in dataset || hap || cooking_type 

//Enter the ubCov ID of your survey after "array"
//local array 4468 4489 4475 4494 4462 4484 4500 4504 4479 5844 5341 5629 5965 5718 4542 5972 4948 5425 5725 6042 4710 5075 5491 5979 4830 4955 5293 5581 5787 6049 6284 4622 4837 5083 5498 5851 4582 4870 4877 5090 6056 4670 5097 5588 6063 5794 6204 5636 6070 4518 4635 4905 5348 5595 5912 4969 6116 4523 4677 4976 5300 5458  
//local array 6211 4545 4586 4717 4911 5354 5505 5643 5858 6290 4915 5463 5360 5649 5986 5365 6123 4593 4843 5103 5511 5864 6296 5307 5656 6130 6218 4922  6303 5918 5663 6077 4884 5372 5669 6137 4554 4683 4891 5023 5469 5808 6143 5166 5676 5030 5476 5815 5925 6150 4928 5314 4598 4850 5250 5518 5871 6309 5036 6156 5379 5683 6000 6316 4527 5732 6225 4558 5601 5931 6323 4562 4778 5525 5690 4784 5042 5531 5878 5938 4565 4572 4934 5431 5739 6163 5833 5048 5538 6084 5384 4790 5391 5608 6007 5836 4797 5398 5746 6232 4803 5259 5753 6170 4647 5546 5885 6239 5055 5438 4983 5444 5760 6091
//local array 5767 6177 4531 4690 4989 5405 5554 5945 6015 6100 6184 4857 5266 5567 5892 6253 4658 4810 5411 5697 5821 6021 6338 4628 4538 4817 5062 5320 5704 6028 6192 6345 6373 5898 6259 4510 5905 5773 4996 5615 6352 4603 5272 6268 4577 6198 5951 4581 4608 4864 5279 5574 4696 5003 5327 5622 5958 6380 4613 4941 5418 5780 6109 5828 5010 5069 5482 4704 6272 5286 4825 5016 5451 5837 6278 4618 4898 5334 5711 6035 6387 6360 6250 6396 6397 6403 6439 6410 6452 6457 6430 6447 6404 6443 6445 6419 6468 6495 6517 6528 6473 6500 6503 6514 6536 6532 6508 6521 6489 6511 6464 6476 6483 6524
//local array 7887 7663 7502 7671 7979 7506 7510 7675 7738 7741 7743 8074 7515 7747 7982 7679 8078 7751 7988 7519 8083 7891 7527 7755 7895 7531 7760 7535 7764 7667 7900 7539 7992 7768 7904 7772 7543 8186 8088 8129 7683 7777 7847 7908 7997 7547 7687 7551 7782 8189 7555 7559 7786 8196 8003 8004 7567 7791 12063 10638 10123 5996 4643 6096 6426 6432 10471

local array 9400 9832 9354 13157 10174


//fill me in with the ubcov_ids you want to run
foreach number in `array'{
    local i `number'
    run_extract `i', store_vals bypass
    tostring year_start, gen(year)
    tostring year_end, gen(end_year)
    tostring nid, gen(nid_n)
    local filename = ihme_loc_id + "_" + survey_name + "_" + year + "_" + end_year + "_" + nid_n
    local filename = subinstr("`filename'", "/", "_",.)
    drop year end_year nid_n
    cd  `outpath'
    outsheet using "`filename'.csv", comma names replace
}

