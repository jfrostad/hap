# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 11/01/2019
# Purpose: Calculate TAP PAFs
#source('/homes/jfrostad/_code/lbd/hap/post_estimation/calc_tap.R') 
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory

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
pacman::p_load(ccaPP, fst, mgsub, sf, stringr)

#detect if running in rstudio IDE
debug <- T
interactive <- ifelse(debug, T, !(is.na(Sys.getenv("RSTUDIO", unset = NA))))

## if running interactively, set arguments
if (interactive) {
  warning('interactive is set to TRUE - if you did not mean to run MBG interactively then kill the model and set interactive to FALSE in parallel script')
  
  ## set arguments
  # TODO currently cannot work with regions that having differing speceification across models
  # region                      <- 'dia_s_america-GUF'
  # region                      <- 'dia_name-ESH'
  region <- 'dia_s_america-GUF'

} else {
  
  ## otherwise, grab arguments from qsub
  ## note this requires a shell script with "<$1 --no-save $@", because its starting at 4
  region                      <- as.character(commandArgs()[4])

} 

#analysis options
# bwga.ier.version <- 38
# ier.version <- "33power2_simsd_source_priors"
rr.data.version <- "33" #rr data version
rr.model.version <-"power2_simsd_source_priors" #rr model version
rr.functional.form <- "power2" #rr functional form
format <- T #set T if needing to reformat the cellpreds to long data.table

# collect date
today <- Sys.Date()
today <- '2019-11-21' #if using old run
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#load gbd fxs
gbd.shared.function.dir <- '/ihme/cc_resources/libraries/current/r/'
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source

# load MBG packages
core_repo <- file.path(h_root, '_code/lbd/hap/')
package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

#load erf custom fx
file.path(h_root, '_code/risks/erf/air_pm/rr/_lib/functional_forms.R') %>% source
  fobject <- get(rr.functional.form)
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

# Helper function to turn a tif raster file into a dt
raster_to_dt <- function(the_raster,
                         simple_polygon,
                         simple_raster,
                         year_list,
                         interval_mo,
                         outputdir,
                         pixel_id) { #TODO will pull names from raster by default add user pass in functionality
  
  #some rasters are badly named with multiple slotnames, subset to first name only
  raster_name <- names(the_raster)[1]
  
  message(paste0("Prepping the ", raster_name, " raster for this region"))
  ## extend and crop pop raster to ensure it matches the simple raster #not convinced this is totally needed
  out  <- extend(the_raster, simple_raster, values = NA)
  out  <- crop(out, extent(simple_raster))
  out  <- setExtent(out, simple_raster)
  out  <- raster::mask(out, simple_raster)
  
  ## check to ensure the pop raster matches the simple raster in extent and resolution
  if (extent(out) != extent(simple_raster)) {
    stop("raster extent does not match simple raster")
  }
  if (any(res(out) != res(simple_raster))) {
    stop("raster resolution does not match simple raster")
  }
  
  #ensure the dimensions are the same
  stopifnot(dim(out)[1:2] == dim(simple_raster)[1:2])
  
  message("converting the raster into a data.table")
  #convert to datables, reshape and stuff
  brick_to_dt = function(bbb, pixel_id = pixel_id){
    dt = setDT(as.data.frame(bbb))
    dt[, pxid := .I] #probably uncessary
    
    #drop rows now in cellIdx
    dt = dt[pixel_id,]
    
    dt = melt(dt, id.vars = 'pxid', variable.factor = F)
    dt = dt[,.(value)]
    return(dt)
  }

  dt <- brick_to_dt(bbb = out, pixel_id = pixel_id) %>% 
    setnames(., raster_name)
  
  # Add pixel_id, but make sure that its recycled explicitly as per data.table 1.12.2 guidelines
  dt[, pixel_id := rep(pixel_id, times = nrow(dt) / length(pixel_id))]
  
  #add year to covdt
  yyy = as.vector(unlist(lapply(min(year_list):max(year_list), function(x) rep.int(x, times = length(pixel_id)))))
  dt[,year := yyy]
  
  return(dt)
  
}


#TODO write documentation
link_cell_pred <- function(ind_gp,
                           ind,
                           rd,
                           reg,
                           measure,
                           pop_measure,
                           year_start,
                           year_end,
                           var_names = ind, # name using ind by default, but can pass custom name
                           n_draws = 250, #TODO automatically check the config file for 'samples' value?
                           #matrix_pred_name = NULL,
                           skip_cols = NULL,
                           rk = T,
                           coastal_fix = T, # if model wasn't run w new coastal rasterization, force old simple raster process 
                           rake_subnational = rk, # TODO is this correct? might need to be defined custom by country
                           shapefile_version = 'current',
                           covs=NA, #added fx to bring in covariates and merge
                           debug=F) {
  
  message('Beginning formatting for ', reg)
  message('loading simple raster & populations')
  
  ## Load simple polygon template to model over
  gaul_list <- get_adm0_codes(reg, shapefile_version = shapefile_version)
  simple_polygon_list <- load_simple_polygon(
    gaul_list = gaul_list, buffer = 1, tolerance = 0.4,
    shapefile_version = shapefile_version
  )
  subset_shape <- simple_polygon_list[[1]]
  simple_polygon <- simple_polygon_list[[2]]
  
  ## Load list of raster inputs (pop and simple)
  if (coastal_fix) raster_list <- build_simple_raster_pop(subset_shape, link_table = shapefile_version) #uses new simple_raster 
  else raster_list <- build_simple_raster_pop(subset_shape, link_table = NULL) #uses old rasterize
  
  simple_raster <- raster_list[["simple_raster"]]
  pop_raster <- raster_list[["pop_raster"]]
  pixel_id <- seegSDM:::notMissingIdx(simple_raster)
  
  ## get number of years
  year_list <- c(year_start:year_end)
  num_yrs <- length(year_list)
  
  message('loading links')
  #####################################################################
  # load the cell id to admin units link
  link_table <- get_link_table(simple_raster, shapefile_version = shapefile_version)
  
  #####################################################################
  # collect and load the population data from the WorldPop rasters
  covdt <- load_populations_cov(reg, pop_measure=pop_measure, measure = measure, simple_polygon, 
                                simple_raster, year_list, interval_mo=12, pixel_id = pixel_id)
  
  #TODO add covariates functionality
  if(debug) browser()
  
  #####################################################################
  # Prepping the cell_pred and link table to be linked by making sure they have the appropriate identifiers.  Also performs a
  # zippering at the region boundary where cells that have a portion of their area outside of the modeling region are reintegrated
  # as a whole cell and considered to be only in one region.  This works becasue a given cell is only modeled in one region.
  link <- prep_link_table(
    link_table = link_table,
    simple_raster = simple_raster,
    pixel_id = pixel_id
  )
  
  # getting the connector for sub-national or national raking, This connector gets the IHME location_code for our
  # gbd targets and connects that to the ADM0_CODE or ADM1_CODE as nessecary
  connector <- get_gbd_locs(
    rake_subnational, 
    reg = reg,
    shapefile_version = shapefile_version
  )
  
  # merge the connector on to the link table by making sure that each cell fragment gets connected to the appropriate
  # raking geography
  link <- sub_nat_link_merge(
    rake_subnational,
    link,
    connector
  )
  
  #helper function to load a list of cell preds and then merge them together
  loadCellPreds <- function(i, 
                            max_n=n_draws) { 
    
    message('~>loading cell pred for: ', ind[[i]])
    
    # Load the relevant pred object - loads an object named cell_pred
    rdata_file <- paste0('/share/geospatial/mbg/', ind_gp[[i]], '/', ind[[i]], '/output/', rd[[i]], '/',
                         ind[[i]], 
                         ifelse(rk, "_raked_cell_draws_eb_bin0_","_cell_draws_eb_bin0_"), 
                         reg, "_0.RData")
    
    #TODO improve logic
    if (rk) {
      if (file.exists(rdata_file)) {
        load(rdata_file)
        cell_pred <- raked_cell_pred
        rm(raked_cell_pred)
      }
    } else {
      if (file.exists(rdata_file)) {
        load(rdata_file)
      }
    }
    
    # Check to make sure loaded correctly
    if (!exists("cell_pred")) stop("Unable to load raked cell pred object!")
    
    # If extra columns at front of cell_pred, can skip here
    #TODO what is this for
    if(!(is.null(skip_cols))) cell_pred <- as.matric(cell_pred[, (skip_cols+1):ncol(cell_pred)])
    
    # Verify alignment  
    if(nrow(cell_pred)!=nrow(covdt)) stop('Dimensional mismatch between cell pred and simple raster!!')
    
    # Enforce max # of draws
    message('...subsetting to #', max_n, ' draws!')
    cell_pred <- cell_pred[, (1:max_n)]

    # set cell pred as a data table, and rename things
    prep_cell_pred(
      cell_pred = cell_pred,
      cell_ids = link_table[[2]],
      pixel_id = pixel_id,
      covdt = covdt
    ) %>% 
      return

  }
  
  #load/format all the cell preds and then merge them together
  cell_pred <- lapply(1:length(ind), loadCellPreds) %>% 
    Reduce(function(...) merge(..., all = TRUE), .)
  
  #TODO should add a test here to verify that the resulting nrow = original cell_pred/num_yrs/250
  #tested it now and it looks OK but this is important to make more robust
  
  # merge cell_pred on the link
  # TODO note that pixel_id col in link dt is a duplicate of ID, and causes a merge issue (pixel_id.x|pixel_id.y)
  # eventually should fix this issue upstream but for now removing it pre-merge is sufficient
  cell_pred <- merge(link[, -c('pixel_id')], cell_pred, by.x = "ID", by.y = "cell_id", allow.cartesian = TRUE)
  
  #also load/merge on any user-provided covariates
  # TODO note that we need to do this at the end due to using cbind instead of merge for prep_cell_pred
  #merging before prep_cell_pred will resort the pixel_id/year and then mess up the cbind
  if (!is.null(covs)) {
    message('adding requested covariates, ', covs)
    
    #load the covariates as a raster
    cell_pred <- load_and_crop_covariates_annual(covs = covs,
                                                 measures = cov_measures,
                                                 simple_polygon = simple_polygon,
                                                 start_year  = min(year_list),
                                                 end_year    = max(year_list),
                                                 interval_mo = 12,
                                                 agebin = 1) %>% 
      #convert to DT and combine
      lapply(., raster_to_dt, simple_polygon, simple_raster, year_list, interval_mo=12, pixel_id = pixel_id) %>% 
      Reduce(function(...) merge(..., all = TRUE), .) %>% 
      #force names, auto-extracting from raster can be variable depending on the upload name of the cell pred obj
      setnames(., c(covs, 'pixel_id', 'year')) %>% 
      #merge to the input rasters DT
      merge(cell_pred, ., by=c('pixel_id', 'year'), all.x=T) #TODO is it possible to have missing covariate values?
    
  }

  #subset to relevant columns and return
  keep_vars <- c('ADM0_CODE', 'ADM1_CODE', 'ADM2_CODE', 
                 'pixel_id', 'year', 'pop', 'area_fraction', unlist(covs), paste0('V', 1:n_draws))
  cell_pred[, (keep_vars), with=F] %>% 
    return
  
}

#custom function for calculating the LRI RR based on PM2.5
calcRR <- function(col.n, ratio, exp, exp.cols, curve, curve.n) {
  
  crude <- fobject$eval(exp[, get(exp.cols[col.n])], curve[[curve.n]][col.n, ])
  out <- ratio * crude - ratio + 1
  
  return(out)
  
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

# ---OPTIONS------------------------------------------------------------------------------------------------------------
## indicate whether running interactively
interactive <- TRUE

## if running interactively, set arguments
if (interactive) {
  warning('interactive is set to TRUE - if you did not mean to run MBG interactively then kill the model and set interactive to FALSE in parallel script')
  
  ## set arguments
  indicator_groups         <- list(hap='cooking', 
                                   lri='lri')
  indicators               <- list(hap='cooking_fuel_solid', 
                                   lri='has_lri')
  run_dates                <- list(hap='2019_09_16_23_24_14', 
                                   lri='2019_10_23_16_13_17')
  measures                 <- list(hap='count',
                                   lri='count')
  suffixes                 <- list(hap='_eb_bin0_0', 
                                   lri='_eb_bin0_0')
  rks                      <- list(hap=F, 
                                   lri=F)
  shapefile                <- '2019_09_10' #NEEDS TO MATCH
  covs                        <- c('ihmepm25')
  cov_measures                <- c('mean')
  
  #TODO janky fix
  if (region=='dia_name-ESH') {
    hap.region <- 'dia_name'
  } else if (region=='dia_s_america-GUF') {
    hap.region <- 'dia_s_america'
  } else hap.region <- region


} else {
  
  ## otherwise, grab arguments from qsub
  ## note this requires a shell script with "<$1 --no-save $@", because its starting at 4
  #TODO

} 

# print out session info so we have it on record
sessionInfo()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~ Prep MBG inputs/Load Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PID <- Sys.getpid()
tic("Entire script") # Start master timer

## Set seed for reproducibility
message('Setting seed 98118 for reproducibility')
set.seed(98118)

# Setup -------------------------------------------------------------------------

#input dirs
j.home.dir <- file.path(j_root,"WORK/05_risk/risks/air_pm")
rr.dir <- file.path(j.home.dir, 'data/rr/output', paste0(rr.data.version, rr.model.version))

# create outputdir
outputdir <- paste0('/share/geospatial/mbg/', indicator_groups[[1]], '/pafs/tap_', today, '/')
  dir.create(paste0(outputdir, '/tmp/'), recursive = T)

# # Make correlation objects ------------------------------------------------------------
##define share directory
share_dir <- paste0('/share/geospatial/mbg/', indicator_groups[[2]], '/', indicators[[2]], '/output/', run_dates[[2]], '/')

## start master timer
tic('Master timer')

tic('Loading external datasets')
# load the gbd location hierarchy
# note that these IDs are the reporting hierarchy for GBD2019
locs <- get_location_metadata(location_set_id = 35, gbd_round_id = 6) %>% 
  .[, .(location_id, iso3=ihme_loc_id)] #subset to relevant columns

# load and format the GBD results
gbd.hap.version <- '092419'
gbd.hap.pm <- file.path('/ihme/erf/GBD2019/air_hap/map/results/summary', gbd.hap.version, 
                        paste0('lm_map_', gbd.hap.version, '.csv')) %>% 
  fread %>% 
  .[grouping=='child', .(location_id, year=year_id, hap_excess_pm25=mean)] %>% #subset to the under5 results and use the mean, (or median?? bc logspace)
  merge(., locs, by='location_id', all.x=T) %>% 
  .[nchar(iso3)<4] %>% #TODO, for now do not use subnationals as they do not merge to adm0 codes
  .[, ADM0_CODE := get_adm0_codes(iso3), by=iso3] #merge on ad0 code

#load and format the gbd RR objects
age.cause <- ageCauseLister(cause.code='lri', gbd.version='GBD2019', lri.version='single', full.age.range = F)
# Prep the RR curves into a single object, so that we can loop through different years without hitting the files extra times.
all.rr <- lapply(1:nrow(age.cause), prepRR, rr.dir=rr.dir) #9 = LRI 
lri.rr <- all.rr[[9]] %>% 
  .[, draw := paste0('V', draws)]
toc(log=TRUE)

## find the adm0s/iso3s of countries in this region
adm0s <- get_adm0_codes(region, shapefile_version = shapefile)
countries <- pull_custom_modeling_regions(region) %>% unlist %>% str_split(., pattern='\\+', simplify = T)

if(format) {
  
  ## create the long data.tables and format them for the TAP calculations
  tic('Make table(s) for HAP')
  
  if (length(hap.region)==1) {
    
    hap.dt <- link_cell_pred(ind_gp = indicator_groups[['hap']],
                             ind = indicators[['hap']],
                             rd = run_dates[['hap']],
                             reg = hap.region,
                             measure = measures[['hap']],
                             pop_measure = 'a0004t', #children <5
                             covs = covs,
                             n_draws = 100, #reduce the # of draws to speed up processing
                             year_start = 2000,
                             year_end = 2017,
                             rk = rks[['hap']],
                             shapefile_version = shapefile,
                             coastal_fix = T,
                             debug=F)
    toc(log = TRUE)
    
    tic('Saving results at country level with .fst')
    lapply(adm0s, meltAndSave, type='hap', dt=hap.dt)
    rm(hap.dt) #save memory
    toc(log=TRUE)
    
  } else {
  #note that this logic is to accomodate HAP's use of single country models
    
    for (subregion in hap.region) {
      
      hap.dt <- link_cell_pred(ind_gp = indicator_groups[['hap']],
                               ind = indicators[['hap']],
                               rd = run_dates[['hap']],
                               reg = subregion,
                               measure = measures[['hap']],
                               pop_measure = 'a0004t', #children <5
                               covs = covs,
                               n_draws = 100, #reduce the # of draws to speed up processing
                               year_start = 2000,
                               year_end = 2017,
                               rk = rks[['hap']],
                               shapefile_version = shapefile,
                               coastal_fix = T,
                               debug=F)
      toc(log = TRUE)
      
      tic('Saving results at country level with .fst')
      lapply(adm0s, meltAndSave, type='hap', dt=hap.dt)
      rm(hap.dt) #save memory
      toc(log=TRUE)
      
    }
    
  }
  
  tic('Make table for LRI')
  lri.dt <- link_cell_pred(ind_gp = indicator_groups[['lri']],
                           ind = indicators[['lri']],
                           rd = run_dates[['lri']],
                           reg = region,
                           measure = measures[['lri']],
                           pop_measure = 'a0004t', #children <5
                           covs = covs,
                           n_draws = 100, #reduce the # of draws to speed up processing
                           year_start = 2000,
                           year_end = 2017,
                           rk = rks[['lri']],
                           shapefile_version = shapefile,
                           coastal_fix = T,
                           debug=F)
  toc(log = TRUE)
  
  tic('Saving results at country level with .fst')
  lapply(adm0s, meltAndSave, type='lri', dt=lri.dt)
  rm(lri.dt) #save memory
  toc(log=TRUE)

} else message('skipping format stage as results are preformatted')

#custom function to calculate the ad0/2 level TAP results
#produces the TAP PAF for LRI, the LRI rate attributable to TAP, and the LRI counts attributable to TAP
calcTAP <- function(country, dir, 
                    hap_data=gbd.hap.pm,
                    debug=F) {
  
  if(debug) browser()
  
  #makes sure file exists
  country_files <- list(hap=sprintf('%s/tmp/%s_hap.fst', dir, country),
                        lri=sprintf('%s/tmp/%s_lri.fst', dir, country))
  existance <- sapply(country_files, file.exists)
           
  if (existance %>% all) {
  
    #read in the long data.tables for the appropriate country
    message('reading data from: ', country_files)
    dt <- read_fst(country_files[['hap']], as.data.table = T) %>% .[, ID := .I]
    lri.dt <- read_fst(country_files[['lri']], as.data.table = T)  %>% .[, ID := .I]
    
    ##TESTS##
    #make sure dimensions match
    test_result <- (dim(dt)!=dim(lri.dt)) %>% any
    if(test_result) { 
      error <- data.table('hap_n'=dt[draw=='V1' & year=='2000', .N], 'lri_n'=lri.dt[draw=='V1' & year=='2000', .N])
    } else {
      #make sure tables have not been re-sorted
      #sample 50 random rows and compare them. they should match in all rows except pixel_id and the indicator
      sampled_ids <- sample(dt$ID, 50)
      test_result <- all.equal(lri.dt[ID %in% sampled_ids, -c('pixel_id', 'lri'), with=F], 
                               dt[ID %in% sampled_ids, -c('pixel_id', 'hap'), with=F])
      if(test_result %>% is.character) error <- test_result
    }
    
    #if errrors are found, record in output folder and return NULL
    if(exists('error')) {
      
      message(country, ' encountered error: ', error)
      write.csv(error, file = paste0(dir, '/', country, '_failure.csv'))
      return(NULL)
    
    } else {  
      #if no errors, process country  
      #merge tables
      dt[, lri := lri.dt[, lri]] %>% 
        .[, ID := NULL] #no longer needed
        rm(lri.dt) #cleanup
      
    
      #merge on the child HAP excess PM2.5 values for each ad0
      dt <- merge(dt, hap_data, by=c('ADM0_CODE', 'year'), all.x=T)
      
      #calculate the TAP PM2.5/capita values
      message('calculating TAP PM2.5/capita')
      dt[, aap_pm := pop * ihmepm25]
      dt[, hap_pm := pop * hap * hap_excess_pm25]
      dt[, tap_pm_capita := (aap_pm + hap_pm) / pop]
      dt[pop==0, tap_pm_capita := 0] #must assume no exposure in zero population cells
      
      #calculate the hap proportion
      dt[, hap_pct := hap_pm/(aap_pm+hap_pm)]
      
      #calculate the TAP RR for LRI
      message('calculating RISK')
      dt <- merge(dt, lri.rr, by='draw') #merge on the draws of IER parameters
      dt[tap_pm_capita<=tmrel, tap_rr := 1] #only calculate RR if the exp>tmrel
      dt[tap_pm_capita>tmrel, tap_rr := 1 + (alpha * (1 - exp(-beta * (tap_pm_capita - tmrel)^gamma)))] #power2 functional form
      
      #calculate the TAP PAF for LRI
      dt[, tap_paf := (tap_rr-1)/tap_rr]
      
      #calculate the LRI attrib to TAP
      dt[, tap_lri_r := tap_paf * lri] #rate
      dt[, tap_lri_c := tap_paf * lri * pop] #count
      
      
      message('collapsing draws')
      #define relevant col vectors
      out_cols <- c('hap_pct', 'tap_paf', 'tap_lri_r', 'tap_lri_c')
      null_cols <- c(indicators, 'ihmepm25', 'hap_excess_pm25', 'aap_pm', 'hap_pm', 'tap_pm_capita', 'tap_rr',
                     names(lri.rr)) %>% unlist
      
      #collapse over the draws for TAP PAF / LRI attrib and return the mean values
      agg <- dt %>% 
        copy %>%
        setkey(., pixel_id, year) %>% 
        .[, (out_cols) := lapply(.SD, mean, na.rm=T), .SDcols=out_cols, by=key(.)] %>% 
        .[, (null_cols) := NULL] %>%  #remove unecessary columns
        unique(., by=key(.))
    
      #TODO move to function part of script
      #custom function for aggregating columns to ad0/1/2
      aggResults <- function(dt, by_cols, agg_cols) {
        
        # aggregate to ad2
        message('Aggregating at the level of ', by_cols)
        
        #distinguish that count columns should be a weighted sum instead of a weighted mean
        sum_cols <- agg_cols %>% .[. %like% '_c'] %T>% 
          message('Using a weighted sum to aggregate: ', .) 
        mean_cols <- agg_cols %>% .[!(. %in% sum_cols)] %T>% 
          message('Using a weighted mean to aggregate: ', .) 
        
        #which columns will no longer be relevant after this collapse?
        null_cols <- c('pixel_id', 'area_fraction', 
                       names(agg) %>% .[(. %like% 'ADM')] %>% .[!(. %in% by_cols)])
        
        #aggregate and return dt
        copy(agg) %>% 
          setkeyv(., by_cols) %>% 
          #fractional aggregation
          .[, (mean_cols) := lapply(.SD, weighted.mean, w=pop*area_fraction, na.rm=T), .SDcols=mean_cols, by=key(.)] %>% 
          .[, (sum_cols) := lapply(.SD, function(x, w) sum(x*w, na.rm=T), w=area_fraction), .SDcols=sum_cols, by=key(.)] %>% 
          .[, pop := sum(pop*area_fraction), by=key(.)] %>% #aggregate pop as well
          .[, (null_cols) := NULL] %>%  #remove unecessary columns
          unique(., by=key(.)) %>% 
          return
        
      }
      
      #agg ad0/2
      ad0 <- aggResults(agg, by_cols=c('ADM0_CODE', 'year'), agg_cols=out_cols)
      ad2 <- aggResults(agg, by_cols=c('ADM0_CODE', 'ADM2_CODE', 'year'), agg_cols=out_cols)
    
      list('ad0'=ad0,
           'ad2'=ad2) %>% 
        return
    }
      
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
toc(log=TRUE)

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

toc() # End master timer
#*********************************************************************************************************************** 
