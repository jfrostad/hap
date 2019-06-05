# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 04/29/2019
# Purpose: Run correlation for ORS and demo the cell pred formatting fx
#source('/homes/jfrostad/_code/lbd/hap/post_estimation/correlation_child.R') 
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
pacman::p_load(ccaPP, fst, mgsub)
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

#TODO write documentation
prep_rasters <- function(these_rasters,
                         reg,
                         measure,
                         pop_measure,
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
  link_table <- get_link_table(simple_raster, shapefile_version = shapefile_version)
  
  #####################################################################
  #turn the rasters into a data.table and merge them together
  dt <- lapply(these_rasters, prep_raster, 
               simple_polygon, simple_raster, year_list, interval_mo=12, pixel_id = pixel_id) %>% 
    Reduce(function(...) merge(..., all = TRUE), .)
  
  #also merge on population
  pop <- load_populations_cov(reg, pop_measure=pop_measure, measure = measure, simple_polygon, 
                              simple_raster, year_list, interval_mo=12, pixel_id = pixel_id)
  dt <- merge(pop, dt, by=c('pixel_id', 'year'))
  
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
  
  # space
  link <- NULL
  
  #subset to relevant columns and return
  keep_vars <- c('ADM0_CODE', 'ADM1_CODE', 'ADM2_CODE', 
                 'pixel_id', 'year', 'pop', 'area_fraction', unlist(var_names))
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
  indicator_groups         <- list('ort', 'ort')
  indicators               <- list('rhf', 'ors')
  run_dates                <- list('2019_04_01_full_OOS', '2019_04_01_full_OOS')
  shapefile               <- '2019_02_27'


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

# create outputdir
outputdir <- paste0('/share/geospatial/mbg/', indicator_groups[[1]], '/exploratory_analysis/output/correlation_', Sys.Date(), '/')
dir.create(paste0(outputdir, '/results_maps/'))

# # Make correlation objects ------------------------------------------------------------
##define share directory
share_dir <- paste0('/share/geospatial/mbg/', indicator_groups[[1]], '/', indicators[[1]], '/output/', run_dates[[1]], '/')

## load in regions used in this model and run_date
regions <- get_output_regions(in_dir = share_dir)

## load in bobby's tifs and work on them
these_rasters <- list.files('/homes/jfrostad/temp/lbd', full.names = T) %>% 
  lapply(., raster)

for (region in regions) {
  
  message('beginning correlation calculations for: ', region)

  tic('Make table')
    dt <- prep_rasters(these_rasters,
                       reg = region,
                       measure = 'count',
                       pop_measure = 'total',
                       year_start = 2010,
                       year_end = 2010,
                       rk = FALSE,
                       shapefile_version = shapefile,
                       coastal_fix = F)
  toc(log = TRUE)
    
  #reset key (to take correlation over year for each pixel/draw)
  # setkey(dt, pixel_id, draw)
  # toc(log = TRUE)
  # 
  # #calculate spearmans correlation over years by cell ID
  # #note corSpearman from ccaPP, it is vectorized so should be faster and more suitable for DT
  # tic('Long calculation')
  # dt <- dt[, cor := corSpearman(a, b), by=key(dt)]
  # dt[, `:=`(a=NULL, b=NULL)] #remove indicators to save memory, no longer needed
  # toc(log = TRUE)
  # 
  # #reshape wide
  # tic('Reshaping back to wide')
  # dt <- dcast(dt, ... ~ draw, value.var= 'cor')
  # toc(log = TRUE)
  # 
  # # finish up and save
  # tic('Saving')
  # message(sprintf('TESTING: Percent of NA rows per column is: %f%%', mean(is.na(dt[, V1]))))
  # 
  # out.path <- sprintf('%s/%s_%s_%s_corr_cell_draws.fst',
  #                     share_dir, region, indicators[[1]], indicators[[2]])
  # 
  # message('-- finished making correlation across draws. now saving as \n', out.path)
  # 
  # write.fst(dt, path = out.path)
  # toc(log = TRUE)
  # 
  # rm(dt) #prepare for next round (save memory)
  
}
 
#*********************************************************************************************************************** 
