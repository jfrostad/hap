# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 04/29/2019
# Purpose: Run correlation calculations vs various covariates for TIF files that bobby is using
# source('/homes/jfrostad/_code/lbd/hap/post_estimation/test/bobby_tifs.R') 
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
pacman::p_load(ccaPP, fst, mgsub, wCorr)
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
# load MBG packages
core_repo <- file.path(h_root, '_code/lbd/ort/')
package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

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

#use the custom link table that Tim Whitson built for stage3
get_link_table <<- function(simple_raster, shapefile_version) {
  
  require(raster)
  
  #for custom stage3 links
  global_link_dir <- '/share/scratch/tmp/fwlt/' #TODO make official

  #read in link_table
  global_link <- file.path(global_link_dir, "link_table_full_world.rds") %>% readRDS %>%  as.data.table
  #read in id_raster
  global_raster <- file.path(global_link_dir, 'id_raster_full_world.rds') %>% readRDS

  #extract the pixel IDs using the cropped and masked global ID raster
  pixel_ids <- 
    crop(global_raster, simple_raster) %>% #crop id_raster to simple_raster
    mask(., simple_raster) %>% #mask id_raster with simple_raster
    raster::extract(., extent(.), na.rm = T) %>% #extract id_raster
    na.omit
  
  #subset link table by pixel ids from extract
  link_table <- global_link[ID %in% pixel_ids,]

  #TODO remove when working properly
  #offical link
  old_global_link <- data.table(readRDS(sprintf("/snfs1/WORK/11_geospatial/admin_shapefiles/%s/lbd_standard_link.rds", shapefile_version)))
  #read in id_raster
  old_global_raster <- readRDS(sprintf("/snfs1/WORK/11_geospatial/admin_shapefiles/%s/lbd_standard_id_raster.rds", shapefile_version))
  
  browser()
  
  #extract the pixel IDs using the cropped and masked global ID raster
  old_pixel_ids <- 
    crop(old_global_raster, simple_raster) %>%  #crop id_raster to simple_raster
    mask(., simple_raster) %>%  #mask id_raster with simple_raster
    raster::extract(., extent(.), na.rm = T) %>% #extract id_raster
    na.omit
  
  #subset link table by pixel ids from extract
  old_link_table <- old_global_link[ID %in% old_pixel_ids,]
  
  #make sure list of pixel ids is same number of nonna cells in simple raster
  if (length(pixel_ids) != length(which(!is.na(getValues(simple_raster))))) {
    stop("The number of pixel_ids does not match the number of non-na values in simple raster. 
         \nPart of the simple raster may be outside the extent of the global id raster.")
  }
  
  #return subsetted link table and list of pixel ids
  list("link_table" = link_table, "pixel_ids" = pixel_ids,
       "old_link" = old_link_table, "old_pixel_ids" = old_pixel_ids) %>% 
    return
  
}

# -------------------------------------------------------------------
# Helper function to turn a tif raster file into a dt
raster_to_dt <- function(the_raster,
                         simple_polygon,
                         simple_raster,
                         year_list,
                         interval_mo,
                         outputdir,
                         pixel_id) { #will pull names from raster by default but user can pass in
  
  
  message(paste0("Prepping the ", names(the_raster), " raster for this region"))
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
    setnames(., names(the_raster))
  
  # Add pixel_id, but make sure that its recycled explicitly as per data.table 1.12.2 guidelines
  dt[, pixel_id := rep(pixel_id, times = nrow(dt) / length(pixel_id))]
  
  #add year to covdt
  yyy = as.vector(unlist(lapply(min(year_list):max(year_list), function(x) rep.int(x, times = length(pixel_id)))))
  dt[,year := yyy]
  
  return(dt)
  
}

#TODO write documentation
format_rasters <- function(these_rasters,
                           reg,
                           measure,
                           pop_measure,
                           covs = NULL,
                           cov_measures = NULL,
                           year_start,
                           year_end,
                           var_names = sapply(these_rasters, names), # name using rasters by default, but can pass custom name
                           matrix_pred_name = NULL,
                           skip_cols = NULL,
                           rk = T,
                           coastal_fix = T, # if model wasn't run w new coastal rasterization, force old simple raster process 
                           rake_subnational = rk, # TODO is this correct? might need to be defined custom by country
                           shapefile_version = 'current') {
  
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
  browser()
  link_table <- get_link_table(simple_raster, shapefile_version = shapefile_version)
  browser()
  
  #####################################################################
  #turn the rasters into a DT and merge them together
  dt <- lapply(these_rasters, raster_to_dt, 
               simple_polygon, simple_raster, year_list, interval_mo=12, pixel_id = pixel_id) %>% 
    Reduce(function(...) merge(..., all = TRUE), .) %>% 
    #force names, auto-extracting from raster can be variable depending on the upload name of the cell pred obj
    setnames(., c('pixel_id', 'year', var_names))
  
  #also merge on population
  dt <- load_populations_cov(reg, pop_measure=pop_measure, measure=measure, simple_polygon, 
                             simple_raster, year_list, interval_mo=12, pixel_id = pixel_id) %>% 
    merge(dt, ., by=c('pixel_id', 'year'), all.x=T) #TODO is it possible to have missing pop values?

  #also load/merge on any user-provided covariates
  if (!is.null(covs)) {
    
    #load the covariates as a raster
    dt <- load_and_crop_covariates_annual(covs = covs,
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
      setnames(., c('pixel_id', 'year', covs)) %>% 
      #merge to the input rasters DT
      merge(dt, ., by=c('pixel_id', 'year'), all.x=T) #TODO is it possible to have missing covariate values?
    
  }
  
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
  
  #now create the requisite IDs for merging
  dt[, cell_pred_id := .I] #cell_pred object ID
  dt[,cell_id := rep(link_table[[2]], times = nrow(dt) / length(link_table[[2]]))]  #cell id references the africa map
  dt[,pixel_id := rep(pixel_id, times = nrow(dt) / length(pixel_id))] #pixel id references the regional map  
  
  #TODO should add a test here to verify that the resulting nrow = original cell_pred/num_yrs/250
  #tested it now and it looks OK but this is important to make more robust
  
  # merge cell_pred on the link
  # TODO note that pixel_id col in link dt is a duplicate of ID, and causes a merge issue (pixel_id.x|pixel_id.y)
  # eventually should fix this issue upstream but for now removing it pre-merge is sufficient
  out <- merge(link[, -c('pixel_id')], dt, by.x = "ID", by.y = "cell_id", allow.cartesian = TRUE)
  
  #subset to relevant columns and return
  #note that this is why we needed to force the cov names
  keep_vars <- c('ADM0_CODE', 'ADM1_CODE', 'ADM2_CODE', 
                 'pixel_id', 'year', 'pop', 'area_fraction', unlist(var_names), covs)
  out[, (keep_vars), with=F] %>% 
    return
  
}

#***********************************************************************************************************************

# ---OPTIONS------------------------------------------------------------------------------------------------------------
## indicate whether running interactively
interactive <- TRUE

## if running interactively, set arguments
if (interactive) {
  warning('interactive is set to TRUE - if you did not mean to run MBG interactively then kill the model and set interactive to FALSE in parallel script')
  
  ## set arguments
  shapefile                   <- '2019_05_06'
  modeling_shapefile_version <- shapefile
  covs                        <- c('access2', 
                                   'diarrhea_prev',
                                   'edu_mean_raked',
                                   'u5m')
  cov_measures                <- c('mean',
                                   'mean',
                                   'median',
                                   'mean')


} else {
  
  ## otherwise, grab arguments from qsub
  ## note this requires a shell script with "<$1 --no-save $@", because its starting at 4
  #TODO

} 

# print out session info so we have it on record
sessionInfo()

## Set seed for reproducibility
message('Setting seed 98118 for reproducibility')
set.seed(98118)
#***********************************************************************************************************************

# ---IN/OUT-------------------------------------------------------------------------------------------------------------
#define dirs
tmp_dir <- '/home/j/temp/jfrostad/'
data_dir <- file.path(tmp_dir, 'data', 'bobby_tifs')
out_dir <- file.path(tmp_dir, 'output', 'bobby_tifs')

## load in region list
region_list <- file.path(j_root, 'WORK/11_geospatial/10_mbg/stage_master_list.csv') %>% 
  fread %>% 
  .[, ADM0_CODE := get_adm0_codes(iso3), by=iso3] #merge on ad0 code
regions <- region_list[Stage %in% c('1', '2a'), unique(mbg_reg)]  #if only using LMICs
regions <- NULL #TODO remove
regions <- c(regions, region_list[mbg_reg=='', unique(iso3)]) #if using all

## load in bobby's tifs and work on them
var_names <- c('distance', 'median', 'ratio')
these_rasters <- list.files(data_dir, full.names = T, pattern='.tif') %>% 
  lapply(., raster)

## load in the provided country info and merge on the ad0 code
country_info <- file.path(data_dir, 'country_info.csv') %>% 
  fread %>% 
  merge(., region_list[, .(country=location_name, iso3, ADM0_CODE)], by='country', all.y=T) #merge on iso3

#***********************************************************************************************************************

# ---CALCULATE----------------------------------------------------------------------------------------------------------
tic("Entire script") # Start master time

#function to loop over regions in lapply
regLoop <- function(region, build=T) {
  
  message('beginning correlation calculations for: ', region)
  
  #setup path to read/write
  fst_path <- sprintf('%s/%s.fst', out_dir, region)

  if (build) { #set TRUE if need to rebuild the table with new covs (longest part of process)
    
    tic('Make table')
    dt <- format_rasters(these_rasters,
                         reg = region,
                         measure = 'count',
                         pop_measure = 'total',
                         covs = covs,
                         cov_measures = cov_measures,
                         year_start = 2010,
                         year_end = 2010,
                         var_names = var_names, #simplify raster names
                         rk = FALSE,
                         shapefile_version = shapefile,
                         coastal_fix = T)

    #reset key (to take correlation over country for each covariate combination)
    setkey(dt, ADM0_CODE, year)
    toc(log = TRUE)
    
    # finish up and save
    tic('Saving raw table as fst')
    message('-> finished making tables. now saving as \n...', fst_path)
    write.fst(dt, path = fst_path)
    toc(log = TRUE)
    
  } else dt <- read.fst(fst_path, as.data.table = T) #else, read from disk

  # aggregate to ad2
  tic('Aggregation - AD2')
  agg_cols <- c(var_names, covs)
  agg_dt <- copy(dt) %>% 
    setkey(., ADM0_CODE, ADM2_CODE, year) %>% 
    #fractional aggregation
    .[, (agg_cols) := lapply(.SD, weighted.mean, w=pop*area_fraction, na.rm=T), .SDcols=agg_cols, by=key(.)] %>% 
    .[, pop := sum(pop*area_fraction), by=key(.)] %>% #aggregate pop as well
    unique(., by=key(.))
  toc()
  
  #internal function to calculate weighted Spearman correlations between a given cov y and a list of x vars
  wCorrCovs <- function(input_dt, cov, var_list, by_vars, wt_var, collapse) {
    
    #copy to avoid global assignments
    dt <- copy(input_dt) %>% 
      na.omit(., cols=cov) #remove any rows that are missing the cov (see above limitation of weightedCorr)
    
    if (nrow(dt)==0) {
      
      message('no data available for ', cov, '!\n|~>skipping')
      return(NULL)
      
    } else {
      
      #create the new column names
      new_cols <- paste0('corr_', cov, '_X_', var_list) 
      setnames(dt, c(cov, wt_var), c('y_var', 'wt')) #rename to simplify function
      
      #we will drop inputs if collapsing as they are no longer meaningful
      if (collapse) keep_cols <- c(key(dt), new_cols)
      else keep_cols <- c(key(dt), new_cols, cov, var_list)
      
      message('calculating correlation for ', new_cols %>% list)
      message('|~>calculation is at the level of...', by_vars %>% list) 

      #calculate correlation
      out <- dt[, (new_cols) := lapply(.SD, weightedCorr, y=y_var, weights=wt, method='Spearman'), #note: cov is x
                by=by_vars, .SDcols=var_list] %>% 
        unique(., by=key(.)) %>% 
        { if (collapse) . else .[, (cov) := y_var] } %>%  #rename y_var if you want to keep it
        .[,  ..keep_cols] %>% #cleanup
        return
      
    }
    
  }
  
  #internal function to apply wCorrCovs on dt with given level of aggregation and compile results
  #note that the key needs to be SET to specify level of aggregation
  calcCorrs <- function(dt, by_vars, wt_var, collapse) {

    tmp <-
    na.omit(dt, cols=var_names) %>% #remove NA values as it causes weightedCorr to fail (na.rm not implemented)
      lapply(covs, wCorrCovs, input_dt=., var_list=var_names, #calculate for each covariate
             by_vars=by_vars, wt_var=wt_var, collapse=collapse) %>%  #grab opts from external fx
      .[!sapply(., is.null)] %>% #remove the null tables (missing covariate values)
      { if (collapse) . else lapply(., function(x) setkeyv(x, cols=c(key(x), var_names))) } %>% #req to avoid dupes in next step
      Reduce(function(...) merge(..., all = TRUE), .) %>%  #merge output
      merge(country_info, ., by='ADM0_CODE', all.y=T) %>% #add on the country info
      return

  }
  
  #calculate spearmans correlation over years by country using pixel level and ad2 level results
  tic('Correlation calculations')
  message('Calculating by pixel')
  out <- calcCorrs(dt, by_vars=c('ADM0_CODE', 'year'), wt_var='pop', collapse=T)
  message('Calculating by ad2')
  agg_out <- calcCorrs(agg_dt, by_vars=c('ADM0_CODE', 'year'), wt_var='pop', collapse=F)
  toc(log = TRUE)

  #export named list
  list('ad0'=out, 'ad2'=agg_out) %>% 
    return
  
}

#loop over all regions
out <- lapply(regions, regLoop, build=T) 

#bind results
out_ad0 <- lapply(out, function(x) x[['ad0']]) %>% rbindlist(use.names=T, fill=T)
out_ad2 <- lapply(out, function(x) x[['ad2']]) %>% rbindlist(use.names=T, fill=T)

# finish up and save
tic('Saving ad0')
out_path <- file.path(out_dir, 'ad0_covariate_correlations.csv')
message('-> finished calculating all correlations. now saving as \n...', out_path)
write.csv(out_ad0, file = out_path, row.names = F)
toc(log = TRUE)

tic('Saving ad2')
out_path <- file.path(out_dir, 'ad2_covariate_correlations.csv')
message('-> finished calculating all correlations. now saving as \n...', out_path)
write.csv(out_ad2, file = out_path, row.names = F)
toc(log = TRUE)

toc() # End master timer
 
#*********************************************************************************************************************** 