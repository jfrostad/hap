# ---------------------------------------------------------------------------------------------
# Functions useful for exploratory analysis of ORS, RHF, ORT, and diarrhea
#
# Written by Kirsten Wiens
# 2019-01-25
# ---------------------------------------------------------------------------------------------


# -------------------------------------------------------------------
# Read in and clean data

get_data <- function(indi, run_date, indicator_group = 'ort', ad = 'admin_0') {
  
  # load data
  mydat <- fread(paste0('/share/geospatial/mbg/', indicator_group, '/', indi, '/output/', run_date, '/pred_derivatives/admin_summaries/', 
                        indi, '_', ad, '_', ifelse(indi == 'had_diarrhea', 'raked_prevalence', 'unraked'), '_summary.csv'))
  
  # clean data
  mydat <- mydat[!is.na(mean)]
  mydat[, cirange := NULL]
  mydat[, indicator := indi]
  
  # end function
  return(mydat)
}
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Calculate fold difference between max and min admin unit

min_max_diff <- function(x) {
  
  # calculate fold difference
  diff <- max(x)/min(x)
  
  # end function
  return(diff)
}
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Calculate relative inequity

rel_dev <- function(x, y) {
  
  # calculate fold difference
  ineq <- (x - y) / y
  
  # end function
  return(ineq)
}
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Calculate absolute inequity

abs_dev <- function(x, y) {
  
  # calculate fold difference
  ineq <- x - y
  
  # end function
  return(ineq)
}
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Correlation coefficient function

# get mean spearman statistic
get_cor_coef <- function(x, y) {
  
  if (!is.na(mean(x)) | !is.na(mean(y))) { 
    out <- cor.test(x, y, method = 'spearman')[['estimate']][['rho']]
  } else {
    out <- NA
  }
  
  #force to numeric to prevent errors in data.table group assignments
  out %>% as.numeric %>% return
  
}
# -------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# General deaths averted for comparative risk assessment

# get deaths averted
get_deaths_averted <- function(observed_rate, counterfactual_rate, observed_deaths, rr, 
                               counterfactual_is_worse = TRUE) {
  
  paf_obs <- (observed_rate*(rr - 1))/(observed_rate*(rr - 1) + 1)
  
  paf_ctf <- (counterfactual_rate*(rr - 1))/(counterfactual_rate*(rr - 1) + 1)
  
  if (counterfactual_is_worse == TRUE) averted <- observed_deaths*paf_ctf - observed_deaths*paf_obs
  if (counterfactual_is_worse == FALSE) averted <- observed_deaths*paf_obs - observed_deaths*paf_ctf
  
  return(averted)
}
# -------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# General number of deaths averted by to a given risk factor per 1,000 (rate)

# get ratio averted
get_rate_averted <- function(deaths_averted, population) {
  
  rate <- deaths_averted/population*1000
  
  if (rate < 0) rate <- 0
  
  return(rate)
  
}
# -------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# General ratio of change in deaths attributable to a given risk factor

# get ratio averted
get_ratio_averted <- function(deaths_averted, change_in_deaths) {
  
  ratio <- max(0, deaths_averted)/change_in_deaths
    
  if (ratio < 0) ratio <- 0
  
  return(ratio)

}

# -------------------------------------------------------------------------------------------

## format_cell_pred ################################################
#TODO write documentation

format_cell_pred <- function(ind_gp,
                             ind,
                             rd,
                             reg,
                             measure,
                             pop_measure,
                             year_start,
                             year_end,
                             var_names = ind, # name using ind by default, but can pass custom name
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
  # collect and load the population data from the WorldPop rasters
  covdt <- load_populations_cov(reg, pop_measure=pop_measure, measure = measure, simple_polygon, 
                                simple_raster, year_list, interval_mo=12, pixel_id = pixel_id)
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
  loadCellPreds <- function(i) { 
  
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
    
    # set cell pred as a data table, and rename things
    cell_pred <- prep_cell_pred(
      cell_pred = cell_pred,
      cell_ids = link_table[[2]],
      pixel_id = pixel_id,
      covdt = covdt
    )
    
    message('~~>reshape long and formatting')
    #reshape long draws and key on the pixel ID/draw/year
    dt <- melt(cell_pred,
               measure = patterns("V"),
               variable.name = "draw",
               value.name = var_names[[i]]) %>% 
      #convert draw col to int instead of V1-250 as a factor
      .[, draw := substring(as.character(draw), 2) %>% as.integer] %>% #TODO probably can be done in the reshape?
      setkey(., pixel_id, draw, year, cell_pred_id, cell_id, pop) %>% #TODO could we get rid of cell_pred_id earlier?
      return
    
  }
  
  #load/format all the cell preds and then merge them together
  cell_pred <- lapply(1:length(ind), loadCellPreds) %>% 
    Reduce(function(...) merge(..., all = TRUE), .)
  
  #TODO should add a test here to verify that the resulting nrow = original cell_pred/num_yrs/250
  #tested it now and it looks OK but this is important to make more robust

  # merge cell_pred on the link
  # TODO note that pixel_id col in link dt is a duplicate of ID, and causes a merge issue (pixel_id.x % pixel_id.y)
  # eventually should fix this issue upstream but for now removing it pre-merge is sufficient
  cell_pred <- merge(link[, -c('pixel_id')], cell_pred, by.x = "ID", by.y = "cell_id", allow.cartesian = TRUE)
  
  # space
  link <- NULL

  #convert to fractional population 
  #TODO for now i will leave this turned off so user can do this adjustment explicitly
  #cell_pred = cell_pred[,pop := pop * area_fraction]

  #subset to relevant columns and return
  keep_vars <- c('ADM0_CODE', 'ADM1_CODE', 'ADM2_CODE', 
                 'pixel_id', 'year', 'pop', 'area_fraction', 'draw', unlist(var_names))
  cell_pred[, (keep_vars), with=F] %>% 
    return
  
}

# -------------------------------------------------------------------
# Crop rasters

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
  dt <- lapply(these_rasters, raster_to_dt, 
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

raster_to_dt <- function(the_raster,
                        simple_polygon,
                        simple_raster,
                        year_list,
                        interval_mo,
                        outputdir,
                        pixel_id){
  
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

message("converting the raster in to a data table")
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
