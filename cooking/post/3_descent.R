# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 11/01/2019
# Purpose: Calculate TAP PAFs
#source('/homes/jfrostad/_code/lbd/hap/cooking/post/3_descent.R') 
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j/"
  h_root <- file.path("/ihme/homes", Sys.info()["user"])
  
  package_lib    <- file.path(h_root, '_code/_lib/pkg')
  ## Load libraries and  MBG project functions.
  .libPaths(package_lib)
  
  # necessary to set this option in order to read in a non-english character shapefile on a linux system (cluster)
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
} else {
  j_root <- "J:"
  h_root <- "H:"
}

#load external packages
#TODO request adds to lbd singularity
pacman::p_load(assertthat, ccaPP, data.table, dplyr, fst, mgsub, sf, stringr, magrittr)

#detect if running interactively
interactive <- F  %>% #manual override
  ifelse(., T, !length(commandArgs())>2) %>%  #check length of arguments being passed in
  ifelse(., T, !(is.na(Sys.getenv("RSTUDIO", unset = NA)))) #check if IDE

if (interactive) {
  
  ## Set repo location, indicator group, and some arguments
  user <- 'jfrostad'
  core_repo <- "/homes/jfrostad/_code/lbd/hap"
  indicator_group <- 'cooking'
  indicator <- 'cooking_fuel_solid'
  config_par   <- 'hap_standard'
  holdout <- 0
  age <- 0
  run_date <- '2020_05_12_13_59_51'
  run_date <- '2020_05_06_22_40_43'
  measure <- 'prev'
  region <- 'soas'
  region <- 'THA'
  cov_par <- paste(indicator_group, region, sep='_')
  my_repo <- "/homes/jfrostad/_code/lbd/hap"
  
} else {
  
  ## Set repo location, indicator group, and some arguments
  user            <- commandArgs()[4]
  core_repo       <- commandArgs()[5]
  indicator_group <- commandArgs()[6]
  indicator       <- commandArgs()[7]
  config_par      <- commandArgs()[8]
  cov_par         <- commandArgs()[9]
  region          <- commandArgs()[10]
  run_date        <- commandArgs()[11]
  measure         <- commandArgs()[12]
  holdout         <- as.numeric(commandArgs()[13])
  my_repo         <- commandArgs()[14]
  age             <- 0
  
}

# collect date
today <- Sys.Date()
#***********************************************************************************************************************

# ---OPTIONS------------------------------------------------------------------------------------------------------------
## set arguments
format <- T #set T if needing to reformat the cellpreds to long data.table
prep_gbd_files <- F #set T if change on GBD side requires reprep of GBD info

#countries we want to process by state 
#TODO-set based on nrow or ndistrict?
big_countries <- c(
  143, #Mexico
  44, #China
  47, #DRC
  65, #Algeria
  138, #Libya
  193, #Sudan
  33, #Brazil
  108, #Iran
  150, #Mongolia
  105 #India
)

#mbg options
rk                       <- (indicator=='cooking_fuel_solid')
suffix                   <- '_eb_bin0_0'
covs                     <- c('ihmepm25')
cov_measures             <- c('mean')

#TODO pass from 2_entry.R?
# config_par   <- 'hap_standard'
# cov_par <- paste(indicator_groups[['hap']], region, sep='_')

# print out session info so we have it on record
sessionInfo()
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#load gbd fxs
gbd.shared.function.dir <- '/ihme/cc_resources/libraries/current/r/'
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source

# load MBG packages
#core_repo <- file.path(h_root, '_code/lbd/lbd_core/')
core_repo <- file.path(h_root, '_code/lbd/hap/')
my_repo <- file.path(h_root, '_code/lbd/hap/')
package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

# load post functions
file.path(my_repo, '_lib', 'post', 'format_cell_preds.R') %>% source
file.path(my_repo, '_lib', 'post', 'risk_calculations.R') %>% source
file.path(my_repo, '_lib', 'post', 'make_projections.R') %>% source

#load erf custom fx
file.path(h_root, '_code/risks/erf/air/_lib/misc.R') %>% source
file.path(h_root, '_code/risks/erf/air/paf/_lib/paf_helpers.R') %>% source

#use your own diacritics fx, due to inscrutable error
#note: requires mgsub pkg
#TODO submit PR
fix_diacritics <<- function(x) {
  
  #first define replacement patterns as a named list
  defs <-
    list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 
         'Ç'='C', 'È'='E', 'É'='E','Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 
         'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U','Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 
         'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c','è'='e', 'é'='e', 'ê'='e', 
         'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
         'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y', 'ß'='Ss')
  
  #then force conversion to UTF-8 and replace with non-diacritic character
  enc2utf8(x) %>% 
    mgsub(., pattern=enc2utf8(names(defs)), replacement = defs) %>% 
    return
  
}

#custom function for melting long on draws and then saving a country level fst file
meltAndSave <- function(country, type, dt, debug=F) {
  
  if (debug) browser()
  
  out.path <- sprintf('%s/%s_%s.fst', paste0(outputdir, '/tmp'), country, type)
  
  #write fst file for country if data available for this indicator
  if (dt[ADM0_CODE==country, .N]>0)  {
    
    message('melting adm0=', country, ' dt long on draw')
    #reshape long draws and key on the pixel ID/draw/year
    out <- melt(dt[ADM0_CODE==country],
                measure = patterns("V"),
                variable.name = "draw",
                value.name = type) %>% 
      #toconvert draw col to int instead of V1-250 as a factor
      #.[, draw := substring(as.character(draw), 2) %>% as.integer] %>% #TODO probably can be done in the reshape?
      setkey(., pixel_id, draw, year) %>% 
      write.fst(., path = out.path) #write file in fst format
    
    message('saved as \n', out.path)
    
  } else message('no ', type, ' data present for #', country)
  
  return(NULL)
  
}

#***********************************************************************************************************************

# ---MBG PREP-----------------------------------------------------------------------------------------------------------
#Prep MBG inputs/Load Data
PID <- Sys.getpid()
tic("Entire script") # Start master timer

## Set seed for reproducibility
message('Setting seed 98118 for reproducibility')
set.seed(98118)

## Read config file and save all parameters in memory
config_filepath <- 'cooking/model/configs/'
config <- set_up_config(repo            = my_repo,
                        indicator_group = indicator_group,
                        indicator       = indicator,
                        config_name     = paste0('/model/configs/config_', config_par),
                        covs_name       = paste0('/model/configs/covs_', cov_par),
                        run_tests       = F,
                        post_est_only   = T,
                        )

# Get the necessary variables out from the config object into global env
#TODO move all to config, some are currently defaulting
rake_countries <- eval(parse(text = config[V1 == 'rake_countries', V2]))
rake_subnational <- eval(parse(text = config[V1 == 'subnational_raking', V2]))
modeling_shapefile_version <- config[V1 == 'modeling_shapefile_version', V2]
raking_shapefile_version <- config[V1 == 'raking_shapefile_version', V2]
countries_not_to_rake <- config[V1 == 'countries_not_to_rake', V2]
countries_not_to_subnat_rake <- config[V1 == 'countries_not_to_subnat_rake', V2]
year_list <- eval(parse(text = config[V1 == 'year_list', V2]))
metric_space <- config[V1 == 'metric_space', V2]
summstats <- eval(parse(text = config[V1 == 'summstats', V2]))

#***********************************************************************************************************************

# ---IN/OUT-------------------------------------------------------------------------------------------------------------
#input dirs
rr.dir <- file.path('/ihme/erf/GBD2019/air_pmhap/rr/model/')
global_link_dir <- file.path('/home/j/WORK/11_geospatial/admin_shapefiles', modeling_shapefile_version) #TODO make official

#intermediate data
data.dir <- file.path('/share/geospatial/mbg/', indicator_group, 'data')

# create outputdir
outputdir <- paste0('/share/geospatial/mbg/', indicator_group, '/pafs/', run_date)
  dir.create(paste0(outputdir, '/tmp/'), recursive = T)
  
# prep GBD files and values ------------------------------------------------------------
##define share directory
share_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date, '/')

## start master timer
tic('Master timer')


#***********************************************************************************************************************

# ---DATA PREP----------------------------------------------------------------------------------------------------------
if(prep_gbd_files) {

  tic('Processing GBD datasets')
  # load the gbd location hierarchy
  # note that these IDs are the reporting hierarchy for GBD2019
  locs <- get_location_metadata(location_set_id = 35, gbd_round_id = 6) %>% 
    .[, .(location_id, iso3=ihme_loc_id)] #subset to relevant columns
  
  # load and format the GBD results
  gbd.hap.version <- '092419'
  gbd.hap.pm <- file.path('/ihme/erf/GBD2019/air_hap/map/results/summary', gbd.hap.version, 
                          paste0('lm_map_', gbd.hap.version, '.csv')) %>% 
    fread %>% 
    .[grouping!='indoor', .(location_id, year=year_id, hap_excess_pm25=mean, grouping)] %>% # use the mean, (or median?? bc logspace)
    merge(., locs, by='location_id', all.x=T) %>% 
    .[nchar(iso3)<4] %>% #TODO, for now do not use subnationals as they do not merge to adm0 codes
    .[, ADM0_CODE := get_adm0_codes(iso3), by=iso3] #merge on ad0 code
  
  #choose ages to merge for RRs
  gbd.hap.pm[grouping=='child', `:=` (age_group_id=2, sex_id=1)]
  gbd.hap.pm[grouping=='female', `:=` (age_group_id=10, sex_id=2)]
  gbd.hap.pm[grouping=='male', `:=` (age_group_id=10, sex_id=1)]
  
  #load the rr max for SEV
  rr.max <- fread('/ihme/erf/GBD2019/air_hap/rr_max/air_hap.csv') %>% 
    setnames(., 'rr', 'rr_max') %>% 
    .[, draw := paste0("V", draw)] %>% 
    .[, rei := NULL] %>% #to save space
    setnames('cause_id', 'cause')
  
  #replace the cause ids with their code
  cause.codes <- c('cvd_ihd',
                   "cvd_stroke_isch",
                   "cvd_stroke_intracerebral",
                   "cvd_stroke_subarachnoid",
                   "lri",
                   'neo_lung',
                   'resp_copd',
                   't2_dm')
  
  cause.ids <- c(493,
                 495,
                 496,
                 497,
                 322,
                 426,
                 509,
                 976)
  
  # then pass to your custom function
  rr.max <- findAndReplace(rr.max,
                           cause.ids,
                           cause.codes,
                           "cause", 
                           'cause')
  
  #keep only one type of stroke so as not to triple weight this in the SEV collapse
  rr.max <- rr.max[!(cause %in% c('cvd_stroke_intracerebral', 'cvd_stroke_subarachnoid'))]
  rr.max[cause %like% 'stroke', cause := 'cvd_stroke']
  
  rr.dt <- 
    list.files(rr.dir) %>% 
    lapply(formatRR, dir=rr.dir) %>% 
    rbindlist(use.names=T, fill=T)
  
  #remove the RRs that are mediated through shifts
  #these include low birthweight and gestational age
  #we will use the GBD pafs for these instead
  rr.dt <- rr.dt[!(cause %in%  c('bw', 'ga', 'lbw', 'ptb'))]
  
  #expand to the appropriate age groups using a custom function
  rr.dt <- unique(rr.dt$cause) %>% 
    lapply(causeExpansion, input.table=rr.dt) %>% 
    rbindlist(use.names=T, fill=T)
  
  #now subset to the age/sex combos that you have specific hap pm data for so as not to create a massive join
  #these are children, women, and men
  rr.dt <- rr.dt[(age_group_id==2 & sex_id==1) | age_group_id==10]
  
  #prep for merge with hap data
  setnames(rr.dt, 'exposure_spline', 'tap_pc')
  
  #combine necessary GBD info
  gbd_data <- list(
    'pm'=gbd.hap.pm,
    'rr'=rr.dt,
    'rr_max'=rr.max
  )

  #save 
  saveRDS(gbd_data,
          file=file.path(data.dir, 'gbd_data.RDS'))
  
  #cleanup
  rm(gbd.hap.pm, 
     rr.dt, 
     rr.max)

} else gbd_data <- file.path(data.dir, 'gbd_data.RDS') %>% readRDS

#read in link_table
global_link_table <- file.path(global_link_dir, "lbd_full_link.rds") %>% readRDS %>% as.data.table
adm_links <- global_link_table[, .(ADM0_NAME, ADM0_CODE, ADM1_NAME, ADM1_CODE, ADM2_NAME, ADM2_CODE)] %>% unique

## find the adm0s/iso3s of countries in this region
adm0s <- get_adm0_codes(region, shapefile_version = modeling_shapefile_version)
countries <- pull_custom_modeling_regions(region) %>% unlist %>% str_split(., pattern='\\+', simplify = T)

#***********************************************************************************************************************

# ---FORMAT PREDS-------------------------------------------------------------------------------------------------------
#format cell preds into long DTs
if(format) {
  
  ## create the long data.tables and format them for the TAP calculations
  tic('Make table(s) for HAP')

  hap.dt <- link_cell_pred(ind_gp = indicator_group,
                           ind = indicator,
                           rd = run_date,
                           reg = region,
                           measure = measure,
                           pop_measure = 'hap', #men/women/children
                           covs = covs,
                           n_draws = 50, #reduce the # of draws to speed up processing
                           year_start = 2000,
                           year_end = 2017,
                           rk = rk,
                           shapefile_version = modeling_shapefile_version,
                           coastal_fix = T,
                           debug=F)
  toc(log = TRUE)
  
  tic('Saving results at country level with .fst')
  lapply(adm0s, meltAndSave, type='hap', dt=hap.dt)
  rm(hap.dt) #save memory
  toc(log=TRUE)
    
} else message('skipping format stage as results are preformatted')

#***********************************************************************************************************************

# ---TAP/RISK CALC------------------------------------------------------------------------------------------------------
#custom function to calculate the ad0/2 level TAP results
#produces the TAP PAF for LRI, the LRI rate attributable to TAP, and the LRI counts attributable to TAP
calcTAP <- function(country, dir, rr_data=rr.dt,
                    adm_info=adm_links,
                    hap_data=gbd_data,
                    debug=F) {
  
  if(debug) browser()
  
  #makes sure file exists
  country_file <- sprintf('%s/tmp/%s_hap.fst', dir, country)
  existance <- file.exists(country_file)
           
  if (existance) {
    
    #read in the long data.tables for the appropriate country
    message('\n********\nstarting process for ', 
            adm_info[ADM0_CODE==country, ADM0_NAME %>% unique], '\n********\n')
    message('reading data from\n')
    message(country_file)
    country_dt <- read_fst(country_file, as.data.table = T) %>% .[, ID := .I]
    states <- unique(country_dt$ADM1_CODE)
    
    #define relevant col vectors
    geo_cols <- c('iso3', 'location_id', 'ADM0_CODE', 'ADM1_CODE', 'ADM2_CODE', 'area_fraction')
    pop_cols <- names(country_dt) %>% .[. %like% 'pop']
    
    #everything is collapsed by cause but everything except PAFs are only unique to loc/year/grouping
    ind_cols <- c('hap_pct', 'tap_paf', 'tap_pm', 'hap', 'hap_sev', 'cor') 
    id_cols <- c('pixel_id', 'year', 'grouping', 'cause')
    
    #for larger countries, we will process them by state
    stateLoop <- function(adm1) {
      
      message('processing draws for ', 
              ifelse(length(adm1)>1, 'all states', paste0('state=', adm1)))
      
      #subset to working adm1
      dt <- country_dt[ADM1_CODE %in% adm1]
      
      #merge on the HAP excess PM2.5 values for each ad0
      #note that this expands the dt to have man/woman/child values for each pixel
      message('merging PM2.5 data')
      dt <- merge(dt, hap_data$pm, by=c('ADM0_CODE', 'year', 'grouping'), all.x=T, allow.cartesian=T)
      
      #calculate the TAP PM2.5/capita values
      message('calculating TAP PM2.5/capita')
      dt[, aap_pm := pop * ihmepm25]
      dt[, hap_pm := pop * hap * hap_excess_pm25]
      dt[, tap_pm := (aap_pm + hap_pm)]
      dt[, tap_pc := tap_pm / pop]
      dt[pop==0, tap_pc := 0] #must assume no exposure in zero population cells
      
      #calculate the hap proportion and other key hap indicators
      dt[, hap_pct := hap_pm/(aap_pm+hap_pm)]
      
      #calculate correlations between aap and hap
      message('making correlation calculations')
      dt[, cor := corSpearman(aap_pm, hap_pm), by=.(pixel_id, draw)]
  
      #calculate the TAP RR for LRI
      message('merging risk files and expanding dt')
      #first merge on the RR.max to expand the causes for our cellpred dt
      dt <- merge(dt, hap_data$rr_max, by=c('draw', 'age_group_id', 'sex_id'), allow.cartesian=T)
      
      #merge on the mrBRT RR predictions using the nearest spline cutpoint
      message('merging mrBRT predictions in order to estimate RR')
      dt <- hap_data$rr[dt, on=.(draw, age_group_id, sex_id, cause, tap_pc), roll='nearest'] 
      
      #calculate the TAP PAF for each cause
      message('making PAF calculations')
      dt[, tap_paf := (rr-1)/rr]
  
      #calculate the SEV by cause using the HAP PAF and then average across causes
      message('making SEV calculations')
      dt[, hap_sev := (tap_paf*hap_pct/(1-tap_paf*hap_pct))/(rr_max-1)]
      dt[, hap_sev := mean(hap_sev), by=.(pixel_id, year, draw, grouping)]
      
      #population weight using the age/sex distribution per pixel to collapse over causes
      dt[, hap_sev := sum(hap_sev*pop)/sum(pop), by=.(pixel_id, year, draw)]
      
      #we will output SEVs at the cell_pred/pixel level in order to forecast
      message('producing SEV cell_pred')
      #TODO note that the V1,V10, etc cols get reordered - shouldnt matter?
      sev_cell_pred <- dt[, .(pixel_id, cell_pred_id, year, draw, hap_sev, area_fraction)] %>% 
        unique(., by=c('pixel_id', 'year', 'draw')) %>% 
        dcast(cell_pred_id+pixel_id+area_fraction+year~draw, value.var='hap_sev') 
      
      #calculate the LRI attrib to TAP
      # dt[, tap_lri_r := tap_paf * lri] #rate
      # dt[, tap_lri_c := tap_paf * lri * pop] #count
  
      message('collapsing draws')

      #first remove any unnecessary cols
      null_cols <- c('ihmepm25', 'hap_excess_pm25', 'aap_pm', 'hap_pm', 'tap_pc',
                     'rr', 'rr_max', 'ID', 'age_group_id', 'sex_id')
      dt <- dt[, (null_cols) := NULL]
  
      #collapse over the draws for all indicators and return the mean values
      agg <- dt %>% 
        copy %>%
        setkeyv(., id_cols) %>% 
        .[, (ind_cols) := lapply(.SD, mean, na.rm=T), .SDcols=ind_cols, by=key(.)] %>% 
        .[, c(geo_cols, id_cols, ind_cols, pop_cols), with=F] %>% 
        unique(., by=key(.))
      
      #since only PAFs are unique by cause, copy all other indicators into 'all' cause bracket
      agg <- agg %>% 
        copy %>% 
        .[cause=='lri'] %>% 
        .[, cause := 'all'] %>%
        #PAFs are not valid at this level without some kind of outcome weighting which we dont have
        #TODO could use GBD loc-year outcome rates in order to weight
        .[, tap_paf := NA] %>% 
        list(agg, .) %>% rbindlist
      
      list('sev_cell_pred'=sev_cell_pred,
           'agg'=agg)
      
    }  
    
    if (country %in% big_countries) {
      
      #TODO make this piece more memory efficient
      country_cores <- ifelse(country==33, 1, 3) #brazil states too large to run multicore
      out_states <- mclapply(states, stateLoop, mc.cores=country_cores)
      
      agg <- lapply(out_states, function(x) x[['agg']]) %>% rbindlist
      sev_cell_pred <- lapply(out_states, function(x) x[['sev_cell_pred']]) %>% rbindlist
      
      rm(out_states) #cleanup
    
    } else {
      
      out_states <- stateLoop(states)
      agg <- out_states[['agg']]
      sev_cell_pred <- out_states[['sev_cell_pred']]
      
    }

    #TODO move to function part of script
    #custom function for aggregating columns to ad0/1/2
    aggResults <- function(dt, by_cols, agg_cols) {
      
      # aggregate to ad2
      message('Aggregating at the level of ', paste(by_cols, collapse=' / '))
      
      #distinguish that count columns should be a weighted sum instead of a weighted mean
      sum_cols <- agg_cols %>% .[. %like% '_c$|_pm$|_pc$'] %T>% 
        message('--Using a weighted sum to aggregate: ', paste(., collapse=' / ')) 
      mean_cols <- agg_cols %>% .[!(. %in% sum_cols)] %T>% 
        message('--Using a weighted mean to aggregate: ', paste(., collapse=' / ')) 
      
      #which columns will no longer be relevant after this collapse?
      null_cols <- c('pixel_id', 'area_fraction', 'tap_pm', 
                     names(dt) %>% .[(. %like% 'ADM')] %>% .[!(. %in% by_cols)])
      
      #check which pop columns were returned (pop_total is produced if using a non-total pop to aggregate)
      #append them to sum_cols, they will be likewise summed in the aggregation step
      pop_cols <- names(dt) %>% .[(. %like% 'pop')]
      sum_cols <- c(sum_cols, pop_cols)

      #aggregate and return dt
      copy(dt) %>% 
        setkeyv(., by_cols) %>% 
        #fractional aggregation
        .[, (mean_cols) := lapply(.SD, weighted.mean, w=pop*area_fraction, na.rm=T), 
          .SDcols=mean_cols, by=key(.)] %>% 
        .[, (sum_cols) := lapply(.SD, function(x, w) sum(x*w, na.rm=T), w=area_fraction), 
          .SDcols=sum_cols, by=key(.)] %>% 
        .[, c(by_cols, mean_cols, sum_cols), with=F] %>%  #keep only necessary columns
        .[, tap_pc := tap_pm / pop] %>% #generate aggregated pm per capita 
        .[, tap_pm := NULL] %>% #no longer useful
        unique(., by=key(.)) %>% 
        return
      
    }
    
    #agg ad0/2
    ad0 <- aggResults(agg, 
                      by_cols=c('ADM0_CODE', 'year', 'grouping', 'cause'), 
                      agg_cols=ind_cols)
    ad2 <- aggResults(agg, 
                      by_cols=c('ADM0_CODE', 'ADM2_CODE', 'year', 'grouping', 'cause'), 
                      agg_cols=ind_cols)

    list('ad0'=ad0,
         'ad2'=ad2,
         'cell_pred'=sev_cell_pred) %>% 
      return
      
  } else {message(country_files[!existance], ' does not exist..skipping!'); return(NULL)}
  
}  

#loop over countries and produce ad0/2 results for TAP
tic('Calculating TAP for each country')
out <- lapply(adm0s, calcTAP, dir=outputdir, debug=F)
toc(log=TRUE)

#bind results
tic('Extracting results from lists')
out_ad0 <- lapply(out, function(x) x[['ad0']]) %>% 
  .[!sapply(., is.null)] %>% #remove the null tables (missing raster values)
  rbindlist(use.names=T, fill=T)
out_ad2 <- lapply(out, function(x) x[['ad2']]) %>% 
  .[!sapply(., is.null)] %>% #remove the null tables (missing raster values)
  rbindlist(use.names=T, fill=T)
out_sev <- lapply(out, function(x) x[['cell_pred']]) %>% 
  .[!sapply(., is.null)] %>% #remove the null tables (missing raster values)
  rbindlist(use.names=T, fill=T)
toc(log=TRUE)

#recombine SEVs into a deduped cell_pred (linking process generates duplicates)
#TODO setup to check dims against original cellpred?
# rdata_file <- paste0('/share/geospatial/mbg/cooking/cooking_fuel_solid/output/', run_dates$hap, '/',
#                      'cooking_fuel_solid_raked_prev_cell_draws_eb_bin0_', region, "_0.RData")
# load(rdata_file)
#cell_pred_dims <- dim(raked_cell_pred)
cell_pred <- names(out_sev) %>% .[. %like% 'V'] %>% 
  unlink_cell_pred(out_sev, cols=.)

# finish up and save
tic('Saving ad0')
out_path <- file.path(outputdir, paste0(region, '_ad0_tap_results.csv'))
message('-> finished calculating TAP for ad0 level, now saving as \n...', out_path)
write.csv(out_ad0, file = out_path, row.names = F)
toc(log = TRUE)

tic('Saving ad2')
out_path <- file.path(outputdir, paste0(region, '_ad2_tap_results.csv'))
message('-> finished calculating TAP for ad2 level, now saving as \n...', out_path)
write.csv(out_ad2, file = out_path, row.names = F)
toc(log = TRUE)

tic('Saving SEV')
sev_file <- paste0('/share/geospatial/mbg/cooking/cooking_fuel_solid/output/', run_date, '/',
                   'cooking_fuel_solid_sev_cell_draws_eb_bin0_', region, "_0.RData")
save(cell_pred, file=sev_file)

#***********************************************************************************************************************

# ---SDG PROJECTIONS----------------------------------------------------------------------------------------------------
# Define goals: start by initializing goal object
goals <- add_goal(target_year = 2030, 
                  target = 0.01,
                  target_type = "less",
                  abs_rel = "absolute",
                  pred_type = c("cell"))

goals <- add_goal(target_year = 2030, 
                  target = 0.05,
                  target_type = "less",
                  abs_rel = "absolute",
                  pred_type = c("cell"))

#make AROC predictions
make_aroc(          
  ind_gp = indicator_group,
  ind = indicator,
  rd = run_date,
  regions = region,
  inputs = out,
  #inputs = list('cell_pred'=cell_pred, 'admin_2'=out_ad2),
  type = c("cell"),
  admin_types = 2, 
  measure = "sev",
  year_list = year_list,
  uselogit = TRUE,
  raked = TRUE,
  weighting_res = 'domain',
  weighting_type = 'exponential',
  pow = 1,
  debug=T
)

make_proj(
  ind_gp = indicator_group,
  ind = indicator,
  rd = run_date,
  regions = region,
  type = c("cell"),
  admin_types=2,
  proj_years = seq(2020, 2030, 5),
  measure = "sev",
  year_list = year_list,
  uselogit = TRUE,
  raked = TRUE,
) 



#run comparisons
compare_to_target(  
  ind_gp = indicator_group,
  ind = indicator,
  rd = run_date,
  regions = region,
  goal_obj = goals,
  measure = "sev", 
  year_list = year_list,
  uselogit = TRUE,
  raked = TRUE
)



toc() # End master timer
#*********************************************************************************************************************** 
