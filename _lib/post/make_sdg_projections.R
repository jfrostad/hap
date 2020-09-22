## get_cell_pred_for_aroc ################################################

#' Pull cell pred object and format for aroc or projection calculations
#'
#' \code{get_cell_pred_for_aroc()} is a helper function that pulls a
#' cell pred object and then formats it for use in other functions
#'
#' @param ind_gp indicator group
#' @param ind indicator
#' @param rd run_date
#' @param reg region
#' @param measure prevalence, incidence, mortality, etc
#' @param matrix_pred_name In \code{sprintf} notation. The one object passed into
#'   the string should will be a region name. this allows different regions to be
#'   passed to different named matrix_preds (pixel level, ad0, ad1, ad2, ...)
#'   e.g. 'had_diarrhea_cell_draws_eb_bin0_%s_diarrhea2_0.RData' which
#'   will be passed to sprintf('had_diarrhea_cell_draws_eb_bin0_%s_0.RData', reg)
#' @param skip_cols columns to skip when reading in the cell preds
#'   For example, if the first two columns store non-pred information in your
#'   file format, \code{skip_cols = 2} will read in all columns from 3 onwards
#' @param rk raked? (logical)
#' @param shapefile_version string which shapefile version to use for indexing cell_preds
#' @return a formated \code{cell_pred} object
#' @examples
#'

get_cell_pred_for_aroc <- function(ind_gp,
                                   ind,
                                   rd,
                                   reg,
                                   measure,
                                   obj = NULL,
                                   matrix_pred_name = NULL,
                                   skip_cols = NULL,
                                   rk = T,
                                   shapefile_version = 'current') {
  
  #if the cell pred object is already in memory, you can can skip loading
  
  if(obj %>% is.null) {
  
    # Load the relevant pred object - loads an object named cell_pred
    # Try both rds file and rdata file until we standardize this
    rds_file <- sprintf(paste0('#REDACTED',
                               matrix_pred_name), ind_gp, ind, rd, reg, measure)
    
    if (rk) {
      rdata_file <- paste0('#REDACTED/', ind_gp, '/', ind, '/output/', rd, '/',
                           ind, "_raked_cell_draws_eb_bin0_", reg, "_0.RData")
    } else {
      rdata_file <- paste0('#REDACTED/', ind_gp, '/', ind, '/output/', rd, '/',
                           ind, "_cell_draws_eb_bin0_", reg, "_0.RData")
    }
    
    if (!is.null(matrix_pred_name) & file.exists(rds_file)) {
      cell_pred <- readRDS(rds_file)
    }
    
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
    if(!(is.null(skip_cols))) cell_pred <- as.matric(cell_pred[, (skip_cols+1):ncol(cell_pred)])
  
  } else cell_pred <- obj  
    
  ## then we need to load the simple raster to determine the
  ## indexing between pixel-years, and matrix rows
  message('-- making simple raster')
  if (reg %>% is.character) gaul_list <- get_adm0_codes(reg, shapefile_version = shapefile_version)
  else gaul_list <- reg #check if the gaul code was passed in, in this case using country-specific calcs
  simple_polygon_list <- load_simple_polygon(gaul_list = gaul_list, buffer = 0.4, subset_only = T,
                                             shapefile_version = shapefile_version)
  subset_shape   <- simple_polygon_list[[1]]
  raster_list    <- build_simple_raster_pop(subset_shape)
  simple_raster  <- raster_list[['simple_raster']] ## this is what we really need
  
  ## then we add two columns to the cell_preds. an spatial index
  ## and a year col
  cell_idx <- seegSDM:::notMissingIdx(simple_raster)
  num_yrs  <- nrow(cell_pred) / length(cell_idx)
  ## this should be an integer year. if it isn't, something isn't
  ## synced correctly between preds and simple_rasters
  if(round(num_yrs, 0) != num_yrs){
    print(sprintf("Error! The number of rows in the matrix_pred object for region %s is not divisible by ",
                  "the number of modeling cells in the associated simple raster object that was loaded for ",
                  "the region. Some dimension mismatch has occured and needs to be investigated.",
                  reg))
    break
  }
  
  new_cols <- cbind(rep(1:num_yrs, each = length(cell_idx)),
                    rep(cell_idx, num_yrs))
  colnames(new_cols) <- c('year', 'idx')
  cell_pred <- cbind(new_cols, cell_pred)
  
  return(cell_pred)
  
}

## make_aroc #############################################################
#' Generates a set of aroc objects from cell_preds or admin_preds, for use in
#' projections or in other analyses that require AROC
#'
#' @param ind_gp indicator group
#' @param ind indicator
#' @param rd run_date
#' @param matrix_pred_name In \code{sprintf} notation. The one object passed into
#'   the string should will be a region name. this allows different regions to be
#'   passed to different named matrix_preds (pixel level, ad0, ad1, ad2, ...)
#'   e.g. 'had_diarrhea_cell_draws_eb_bin0_%s_diarrhea2_0.RData' which
#'   will be passed to sprintf('had_diarrhea_cell_draws_eb_bin0_%s_0.RData', reg)
#' @param type Type of aroc to create. Options include \code{cell}, \code{admin},
#'   or \code{c('cell', 'admin')} for both.
#' @param measure prevalence, incidence, mortality, etc
#' @param skip_cols columns to skip when reading in the cell preds
#'   For example, if the first two columns store non-pred information in your
#'   file format, \code{skip_cols = 2} will read in all columns from 3 onwards
#' @param year_list Vector (integer) of years included in the model run / cell pred object
#' @param uselogit Should this be done in logit space?
#' @param raked Should we do this with raked cell/admin preds?
#' @param weighting_res Spatial resolution to apply weights. Can be done by either:
#'    'domain' (e.g. Africa),
#'    'country'.
#'    In the future this could be expanded to adm* levels. Currently this only affects
#'    weighting if you also select weighting_type==empirical
#' @param weighting_type Method to generate the weighting used in the weighted-mean of the
#'    rate of changes calculated between all adjacent pairs of years in year_list.
#'    Can be: 'exponential' (see pow argument for details) or 'empirical'.
#'    Empirical generates weights proportional to the amount of data in each year.
#'      - The total sample size across all years divided by num_years is added to every year
#'        to ensure no pairs of years get zero weight
#'      - If empirical and country are both selected and there are countries with no data then
#'        exponential weights are used
#'      - Must supply input_data
#' @param pow Power used in year-weighting scheme:
#'    exponential weight used in determining year_wt_i:
#'         (yr_i - yr_1)/(yr_n - yr_i))^pow
#'    if pow==0, get uniform wts
#'    if pow==1, get linear wts
#'    if pow > 1, get exponential wts, etc.
#' @param input_data Required to derive emprical weights. A set of cleaned and collapsed micro
#'    data (e.g. input to MBG models) that will be used to derive the emprical weights.
#'    Required Columns:
#'       - country (iso3)
#'       - N
#'       - year
#' @param mult_emp_exp if TRUE and you are calculating emprical weights, the sample-size
#'    driven empirical weights are also multipled by the exponential weighting scheme such
#'    that both the exponential weight and the empirical weight contribute to the final weights
#' @param extra_file_tag Appended at the end of all files generated from this run of the function.
#' Useful if you're comparing different weights/resolutions to calcualte AROC and project
#' @param shapefile_version string specifying which version of shapefiles to pull
#' @return writes cell- and admin- level aroc objects to standard directories and formats
#'   in the 'pred_derivatives' folder of the model run.  cell-level objects are in the
#'   cell_pred indexed format. Both cell- and admin- aroc objects are matrices wide by draw.
#' NOTE: admin_preds are sorted such that the order of the rows is by year and then admin_code
#' @examples
#' make_aroc(ind_gp = indicator_group,
#'           ind = indicator,
#'           rd = run_date,
#'           matrix_pred_name = NULL,
#'           type = c("cell", "admin"),
#'           measure = "prevalence",
#'           year_list = c(2000:2015),
#'           uselogit = TRUE,
#'           raked = TRUE,
#'           weighting_res = 'country',
#'           weighting_type = 'empirical',
#'           pow = 1,
#'           input_data = clean_collapsed_micro_data,
#'           mult_emp_exp = TRUE)


make_aroc <- function(ind_gp, ind, rd,
                      regions=NULL,
                      matrix_pred_name = NULL,
                      inputs = list('cell_pred'=NULL, 'admin_2'=NULL),
                      admin_types = 0:2,
                      type,
                      measure="mortality",
                      skip_cols = NULL,
                      year_list = c(2000:2015),
                      uselogit = FALSE,
                      raked,
                      weighting_res = 'domain',
                      weighting_type = 'exponential',
                      pow = 1,
                      input_data = NULL,
                      mult_emp_exp = FALSE,
                      extra_file_tag = '',
                      shapefile_version = 'current',
                      debug=F
) {
  
  if(weighting_type == 'emprical' & is.null(input_data)){
    warning("You must supply input_data if using 'emprical' weighting.")
    stop()
  }
  
  if (debug) browser()
  
  # define directories
  share_dir <- paste0('#REDACTED', ind_gp, '/', ind, '/output/', rd, '/')
  output_dir <- paste0(share_dir, "/pred_derivatives/aroc/")
  dir.create(output_dir, showWarnings=F, recursive = T)
  
  ## load in regions used in this model and run_date
  if (regions %>% is.null) regions <- get_output_regions(in_dir = share_dir)

  ## get weights to use in either cell or adm aroc calculations
  aroc_weights <- make_aroc_weights(weighting_res = weighting_res,
                                    weighting_type = weighting_type,
                                    pow = pow,
                                    year_list = year_list,
                                    input_data = input_data,
                                    mult_emp_exp = mult_emp_exp)
  
  ## we also get all the gaul codes that came back from
  ## make_arox_weights() if we're using country resolution
  if(weighting_res == 'country'){
    aroc_weight_gauls <- gaul_convert(aroc_weights$country, from = 'iso3', shapefile_version = shapefile_version)
  }
  
  ## we also get the exponential weights which we may need in the
  ## empirical setting if there's no data in a predicted country
  exp_weights <- make_aroc_weights(weighting_res = 'domain',
                                   weighting_type = 'exponential',
                                   pow = pow,
                                   year_list = year_list,
                                   input_data = NULL,
                                   mult_emp_exp = FALSE)
  
  ## get number of years
  num_yrs <- length(year_list)
  
  if('cell' %in% type){
    message('Working on CELL level')
    for(ii in 1:length(regions)){
      
      
      message(sprintf('- On region: %s', regions[ii]))
      
      cell_pred <- get_cell_pred_for_aroc(ind_gp,
                                          ind,
                                          rd,
                                          regions[ii],
                                          measure,
                                          matrix_pred_name,
                                          inputs[['cell_pred']], 
                                          skip_cols, rk = raked,
                                          shapefile_version = shapefile_version)
      
      if(weighting_res == 'country'){
        ## we'll need to match pixels to countries so we load the region simple raster
        message('-- making simple raster')
        if (reg %>% is.character) gaul_list <- get_adm0_codes(regions[ii], shapefile_version = shapefile_version)
        else gaul_list <- reg #check if the gaul code was passed in, in this case using country-specific calcs
        simple_polygon_list <- load_simple_polygon(gaul_list = gaul_list, buffer = 0.4, subset_only = T)
        subset_shape   <- simple_polygon_list[[1]]
        raster_list    <- build_simple_raster_pop(subset_shape)
        simple_raster  <- raster_list[['simple_raster']] ## this is what we really need
        
        ## pull out gaul codes and match to cell preds via cell_idx
        cell_idx <- seegSDM:::notMissingIdx(simple_raster)
        ctry_vec <- values(simple_raster)[cell_idx]
        all_ctrys <- sort(unique(ctry_vec))
      }
      
      ## now we have some year column and some id column and the rest are draw columns.
      ## we can now calculate AROC by idx across time
      num_draws <- ncol(cell_pred) - 2
      num_idx <- length(unique(cell_pred[, 2])) ## second col is 'idx'
      
      ## for each draw, calculate the rates of change between years and
      ## then the weighted total AROC across all years
      message('-- making AROC between years for each draw')
      aroc_draws <- matrix(ncol = num_draws, nrow = num_idx)
      #goal_draws <- copy(aroc_draws)
      if(uselogit){
        cell_pred_logit <- cell_pred
        cell_pred_logit[, 3:ncol(cell_pred)] <- log(cell_pred[, 3:ncol(cell_pred)] / (1 - cell_pred[, 3:ncol(cell_pred)]))
      }
      for(dd in 1:(ncol(cell_pred) - 2)){
        if(dd %% 50 == 1) message(sprintf('---- on draw %i out of %i', dd, num_draws))
        aroc_mat <- matrix(ncol = num_yrs - 1, nrow = num_idx)
        for(yy in 1:(num_yrs - 1)){
          if(uselogit){
            aroc_mat[, yy] <- cell_pred_logit[1:num_idx + (yy) * num_idx, dd + 2] -  cell_pred_logit[1:num_idx + (yy - 1) * num_idx, dd + 2]
          }else{
            aroc_mat[, yy] <- log(cell_pred[1:num_idx + (yy) * num_idx, dd + 2]) -  log(cell_pred[1:num_idx + (yy - 1) * num_idx, dd + 2])
          }
        }
        
        if(weighting_res == 'domain'){
          ## then there is only a single weight vector for all locs
          aroc_vec <- aroc_mat %*% aroc_weights
        }
        if(weighting_res == 'country'){
          ## we have a different set of weights for each country and must match accordingly
          aroc_vec <- rep(0.0, nrow(aroc_mat))
          for(cc in 1:length(all_ctrys)){
            ctry_gaul <- all_ctrys[cc]
            ctry_idx <- which(ctry_vec == ctry_gaul)
            if(ctry_gaul %in% aroc_weight_gauls){ ## use empirical weight if possible
              ctry_wt <- as.numeric(aroc_weights[which(aroc_weight_gauls == ctry_gaul), 2:ncol(aroc_weights)])
            }else{ ## otherwise use exponential weight
              ctry_wt <- exp_weights
            }
            aroc_vec[ctry_idx] <- aroc_mat[ctry_idx, ] %*% as.vector(ctry_wt)
          }
          
        }
        
        aroc_draws[, dd] <- aroc_vec
        #goal_draws[, dd] <- ifelse(aroc_vec < aroc_goal, 1, 0)
      }
      # short_goal <- ifelse(aroc_draws < aroc_goal_2015, 1, 0)
      # goal_draws[is.na(goal_draws)] <- 0
      # short_goal[is.na(short_goal)] <- 0
      # relative_goal_prob <- rowMeans(goal_draws)
      # achieved_relative_prob <- rowMeans(short_goal)
      message(sprintf('TESTING: Percent of NA rows per column is: %f%%', mean(is.na(aroc_draws[, 1]))))
      message('-- finished making AROC across draws. now saving')
      saveRDS(object = aroc_draws,
              file = sprintf('%s/%s_%s_aroc_cell_draw_matrix_%s%s%s.RDs',
                             output_dir, ind, measure, ifelse(uselogit, "logit_", ""), regions[ii],
                             extra_file_tag))
    } # Close region loop
  } # if ('cell' %in% type)
  
  if('admin' %in% type){

    message('Working on ADMIN level')
      ## load the admin objects
      ## try two different locations until we standardize
      
      if (inputs$admin_2 %>% is.null) {
        file_1 <- sprintf('#REDACTED', 
                          ifelse(raked, "raked", "unraked"), '.Rdata',
                          ind_gp, ind, rd, ind, measure)
        file_2 <- paste0('#REDACTED', ind_gp, '/', ind, '/output/', rd, '/',
                         ind, '_', ifelse(raked, "raked", "unraked"),
                         '_admin_draws_eb_bin0_0.RData')
        
        if (file.exists(file_1)) {
          load(file_1)
        } else if (file.exists(file_2)) {
          load(file_2)
        } else {
          stop("Cannot load admin pred object!")
        }
      
      } else admin_2 <- inputs[['admin_2']]
    
    ## load spatial admin hierarchy
    admins <- get_sp_hierarchy(shapefile_version = shapefile_version)
    
    ## this contains admin_0, admin_1, and admin_2 matrix_draw objects
    ## col1 is year, col2 is ADM*_CODE, final column is pop. all other columns are draws
    
    for(aa in admin_types) { ## for each admin type
      message(sprintf('- On admin: %i', aa))
      
      cell_pred <- get(sprintf('admin_%i', aa))
      cell_pred <- as.data.table(cell_pred)
      
      ## format this object to look like the one we used for cell level (easier to copy code from above)
      ## revised now to work with a bigger array of cell pred objects
      str_match <- stringr::str_match
      draw_cols <- names(cell_pred)[grep("V[0-9]*", names(cell_pred))]
      keep_cols <- c("year", paste0("ADM", aa, "_CODE"), draw_cols)
      cell_pred <- subset(cell_pred, select = keep_cols)
      setnames(cell_pred, paste0("ADM", aa, "_CODE"), "idx")
      cell_pred <- as.matrix(cell_pred)
      
      ## it needs to be ordered like the regular cell_pred object too!
      ## all admin_idx for a single year show up, then repeat
      cell_pred <- cell_pred[order(cell_pred[,1], cell_pred[,2]), ]
      
      num_draws <- ncol(cell_pred) - 2
      
      # Redid the below line to warn if mismatches in case of non-unique admin codes - JM
      num_idx <- nrow(cell_pred) / length(year_list)
      if (num_idx != length(unique(cell_pred[,2]))) {
        warning(paste0("The number of unique cell_pred indices does not equal the number of ",
                       "rows of cell_pred divided by the number of years.",
                       "\nYou may have duplicate admin codes at the admin ", aa,
                       " level - check your aggregated objects!"))
      }
      num_yrs <- length(unique(cell_pred[, 1]))
      idx <- cell_pred[which(cell_pred[, 1] == min(cell_pred[, 1])), 2] ## idx in year1
      
      if(weighting_res == 'country'){
        if(aa == 0){
          ctry_vec <- idx
        }else{
          sp_aa <- admins[[sprintf('ADM%i', aa)]]
          aa_vec <- cell_pred[, sprintf('ADM%i_CODE', aa), with = F]
          aa_vec[, ind := 1:nrow(aa_vec)]
          ctry_vec <- merge(aa_vec, sp_aa, all.y = FALSE)
          ctry_vec <- ctry_vec[order(ind), ADM0_CODE]
          ctry_vec <- ctry_vec[1:num_idx]
        }
        ## get all the countries in the pred object
        all_ctrys <- sort(unique(ctry_vec))
      }
      
      ## for each draw, calculate the rates of change between years and
      ## then the weighted total AROC across all years
      message('-- making AROC between years for each draw')
      aroc_draws <- matrix(ncol = num_draws, nrow = num_idx)
      
      if(uselogit){
        cell_pred_logit <- cell_pred
        cell_pred_logit[, 3:ncol(cell_pred)] <- log(cell_pred[, 3:ncol(cell_pred)] / (1 - cell_pred[, 3:ncol(cell_pred)]))
      }
      
      for(dd in 1:(ncol(cell_pred) - 2)){
        if(dd %% 50 == 1) message(sprintf('---- on draw %i out of %i', dd, num_draws))
        aroc_mat <- matrix(ncol = num_yrs - 1, nrow = num_idx)
        for(yy in 1:(num_yrs - 1)){
          if(uselogit){
            aroc_mat[, yy] <- cell_pred_logit[1:num_idx + (yy) * num_idx, dd + 2] -  cell_pred_logit[1:num_idx + (yy - 1) * num_idx, dd + 2]
          }else{
            aroc_mat[, yy] <- log(cell_pred[1:num_idx + (yy) * num_idx, dd + 2]) -  log(cell_pred[1:num_idx + (yy - 1) * num_idx, dd + 2])
          }
        }
        
        if(weighting_res == 'domain'){
          ## then there is only a single weight vector for all locs
          aroc_vec <- aroc_mat %*% aroc_weights
        }
        if(weighting_res == 'country'){
          ## we have a different set of weights for each country and must match accordingly
          aroc_vec <- rep(0.0, nrow(aroc_mat))
          for(cc in 1:length(all_ctrys)){
            ctry_gaul <- all_ctrys[cc]
            ctry_idx <- which(ctry_vec == ctry_gaul)
            if(ctry_gaul %in% aroc_weight_gauls){ ## use empirical weight if possible
              ctry_wt <- as.numeric(aroc_weights[which(aroc_weight_gauls == ctry_gaul), 2:ncol(aroc_weights)])
            }else{ ## otherwise use exponential weight
              ctry_wt <- exp_weights
            }
            aroc_vec[ctry_idx] <- aroc_mat[ctry_idx, ] %*% as.vector(ctry_wt)
          }
          
        }
        
        aroc_draws[, dd] <- aroc_vec
      }
      
      message(sprintf('TESTING: Percent of NA rows per column is: %f%%', mean(is.na(aroc_draws[, 1]))))
      message('-- finished making AROC across draws. now saving')
      final_aroc <- cbind(idx, aroc_draws)
      colnames(final_aroc)[1] <- sprintf('ADM%i_CODE', aa)
      
      saveRDS(object = final_aroc,
              file = sprintf('%s/%s_%s_aroc_adm%i_draw_matrix%s%s.RDs',
                             output_dir, ind, measure, aa, ifelse(uselogit, "_logit", ""),
                             extra_file_tag))
      
    } # For aa in admin
  } # if ('admin' %in% type)...
}

## make_proj #############################################################

#' Generates a set of draw-level projection objects from aroc objects for
#' a given set of target years
#'
#' @param ind_gp indicator group
#' @param ind indicator
#' @param rd run_date
#' @param type Type of aroc to create. Options include \code{cell}, \code{admin},
#'   or \code{c('cell', 'admin')} for both.
#' @param proj_years Vector (integer) of years that you want to project to.  Note
#'   that this is different from \code{year_list}, which is the list of years that
#'   were included in the model run / are included in the aroc object.
#' @param measure prevalence, incidence, mortality, etc
#' @param skip_cols columns to skip when reading in the cell preds
#'   For example, if the first two columns store non-pred information in your
#'   file format, \code{skip_cols = 2} will read in all columns from 3 onwards
#' @param raked Should we do this with raked cell/admin preds?
#' @param matrix_pred_name In \code{sprintf} notation. The one object passed into
#'   the string should will be a region name. this allows different regions to be
#'   passed to different named matrix_preds (pixel level, ad0, ad1, ad2, ...)
#'   e.g. 'had_diarrhea_cell_draws_eb_bin0_%s_diarrhea2_0.RData' which
#'   will be passed to sprintf('had_diarrhea_cell_draws_eb_bin0_%s_0.RData', reg)
#' @param year_list Vector (integer) of years included in the model run / cell pred object
#' @param uselogit Should this be done in logit space?
#' @param extra_file_tag Appended at the end of all files generated from this run of the function. Useful if you're comparing different weights/resolutions to calcualte AROC and project
#' @return writes cell- and admin- level projection objects to standard directories and formats
#'   in the 'pred_derivatives' folder of the model run.  cell-level objects are in the
#'   cell_pred indexed format. Both cell- and admin- projection objects are matrices wide by draw.
#' NOTE: admin_preds are sorted such that the order of the rows is by year and then admin_code
#' @examples
#' make_proj(ind_gp = indicator_group,
#'           ind = indicator,
#'           rd = run_date,
#'           type = c("cell", "admin"),
#'           proj_years = c(2020, 2025, 2030),
#'           measure = "prevalence",
#'           skip_cols = NULL,
#'           year_list = c(2000:2015),
#'           uselogit = TRUE)

make_proj <- function(ind_gp, ind, rd,
                      regions=NULL,
                      type,
                      admin_types=0:2,
                      proj_years = c(2020, 2025),
                      measure="mortality",
                      skip_cols = NULL,
                      raked, 
                      matrix_pred_name = NULL,
                      year_list = c(2000:2015),
                      uselogit = FALSE,
                      extra_file_tag = '',
                      shapefile_version = 'current'
) {
  
  if (regions %>% is.null) {
    ## load in regions used in this model and run_date
    regions <- get_output_regions(in_dir = paste0('/share/geospatial/mbg/',
                                                  ind_gp, '/',
                                                  ind, '/output/',
                                                  rd))
 }
  
  
  # define directories
  share_dir <- paste0('#REDACTED', ind_gp, '/', ind, '/output/', rd, '/')
  aroc_dir <- paste0(share_dir, "/pred_derivatives/aroc/")
  output_dir <- paste0(share_dir, "/pred_derivatives/proj/")
  
  dir.create(output_dir, recursive = T, showWarnings = F)
  
  ## make projections at the cell level
  if ('cell' %in% type) {
    message('Working on CELL level')
    for(ii in 1:length(regions)){
      message(sprintf('- On region: %s', regions[ii]))
      
      # Pull cell pred
      cell_pred <- get_cell_pred_for_aroc(ind_gp,
                                          ind,
                                          rd,
                                          regions[ii],
                                          measure,
                                          matrix_pred_name,
                                          skip_cols,
                                          rk = raked, 
                                          shapefile_version = shapefile_version)
      
      # Load aroc object
      message('-- loading aroc')
      
      aroc_draws <- readRDS(sprintf('%s/%s_%s_aroc_cell_draw_matrix_%s%s%s.RDs',
                                    aroc_dir, ind, measure, ifelse(uselogit, "logit_", ""),
                                    regions[ii], extra_file_tag))
      
      # Get number of draws
      num_draws <- ncol(aroc_draws)
      if (ncol(aroc_draws) != ncol(cell_pred) - 2) stop("cell_pred & aroc draw #s do not match!")
      
      # Make projections -----------------------------------
      ## also make projections and save them
      message('-- making projections')
      
      ## grab last year of modeled estiamtes
      final_year <- cell_pred[which(cell_pred[, 1] == max(cell_pred[, 1])), ] ## first col is year
      last_year <- final_year[, -(1:2)]
      
      ## grab idx
      idx <- final_year[,2]
      
      ## unlist all draws into vector, apply forecast, and convert back to matrix
      final_year <- as.vector(final_year[, -(1:2)]) ## unlist only the draw columns
      
      ## unlist aroc draw matrix
      aroc_draws <- as.vector(aroc_draws)
      
      ## set up a list to capture the projection output
      proj_draws_list <- list()
      
      for (yr in proj_years) {
        
        message(paste0('--- year: ', yr))
        
        ## figure out how many years to project
        proj_dur <- as.numeric(yr) - max(year_list)
        
        ## make projection from final_yr out proj_dur years
        if(uselogit){
          proj_draws_logit <- log(final_year / (1 - final_year)) + (aroc_draws * proj_dur)
          proj_draws <- exp(proj_draws_logit) / (1 + exp(proj_draws_logit))
        }else{
          proj_draws <- final_year * exp(aroc_draws * proj_dur)
        }
        
        ## convert back to matrix
        proj_draws <- matrix(proj_draws, ncol = num_draws)
        
        ## insert into list
        proj_draws_list[[as.character(yr)]] <- proj_draws
        rm(proj_draws)
        
      }
      
      ## save
      message('-- saving projections')
      
      lapply(1:length(proj_draws_list), function(i) {
        saveRDS(object = proj_draws_list[[i]],
                file = sprintf('%s/%s_%s_%s_projections_cell_draw_matrix_%s%s.RDs',
                               output_dir, ind, measure, names(proj_draws_list)[i],
                               ifelse(uselogit, "logit_", ""), regions[ii]))
      })
    } # close regions loop
  } # if ('cell' %in% type)
  
  # make projections as the admin level
  if ('admin' %in% type) {
    
    message('Working on ADMIN level')
    ## load the admin objects
    ## try two different locations until we standardize
    message('- loading admin objects')
    file_1 <- sprintf('#REDACTED', 
                      ifelse(raked, "raked", "unraked"), '.Rdata',
                      ind_gp, ind, rd, ind, measure)
    file_2 <- paste0('#REDACTED', ind_gp, '/', ind, '/output/', rd, '/',
                     ind, '_', ifelse(raked, "raked", "unraked"), 
                     '_admin_draws_eb_bin0_0.RData')
    
    if (file.exists(file_1)) {
      load(file_1)
    } else if (file.exists(file_2)) {
      load(file_2)
    } else {
      stop("Cannot load admin pred object!")
    }
    
    for(aa in admin_types){ ## for each admin type
      
      message(paste0("- admin level: ", aa))
      
      # create pseudo cell-pred object
      cell_pred <- get(sprintf('admin_%i', aa))
      cell_pred <- as.data.table(cell_pred)
      
      ## format this object to look like the one we used for cell level (easier to copy code from above)
      ## revised now to work with a bigger array of cell pred objects
      str_match <- stringr::str_match
      draw_cols <- names(cell_pred)[grep("V[0-9]*", names(cell_pred))]
      keep_cols <- c("year", paste0("ADM", aa, "_CODE"), draw_cols)
      cell_pred <- subset(cell_pred, select = keep_cols)
      setnames(cell_pred, paste0("ADM", aa, "_CODE"), "idx")
      cell_pred <- as.matrix(cell_pred)
      
      ## it needs to be ordered like the regular cell_pred object too!
      ## all admin_idx for a single year show up, then repeat
      cell_pred <- cell_pred[order(cell_pred[,1], cell_pred[,2]), ]
      
      # Redid the below line to warn if mismatches in case of non-unique admin codes - JM
      num_idx <- nrow(cell_pred) / length(year_list)
      if (num_idx != length(unique(cell_pred[,2]))) {
        warning(paste0("The number of unique cell_pred indices does not equal the number of ",
                       "rows of cell_pred divided by the number of years.",
                       "\nYou may have duplicate admin codes at the admin ", aa,
                       " level - check your aggregated objects!"))
      }
      num_yrs <- length(unique(cell_pred[, 1]))
      idx <- cell_pred[which(cell_pred[, 1] == min(cell_pred[, 1])), 2] ## idx in year1
      
      # load aroc
      message("-- loading aroc")
      aroc_draws <- readRDS(sprintf('%s/%s_%s_aroc_adm%i_draw_matrix%s%s.RDs',
                                    aroc_dir, ind, measure, aa, ifelse(uselogit, "_logit", ""),
                                    extra_file_tag))
      
      # remove first column (spatial index) and store separately
      spatial_idx <- aroc_draws[, 1]
      aroc_draws <- aroc_draws[, 2:ncol(aroc_draws)]
      
      # Get number of draws
      num_draws <- ncol(aroc_draws)
      if (ncol(aroc_draws) != ncol(cell_pred) - 2) stop("cell_pred & aroc draw #s do not match!")
      
      # make projections
      message('-- making projections')
      
      ## grab last year of modeled estiamtes
      final_year <- cell_pred[which(cell_pred[, 1] == max(cell_pred[, 1])), ] ## first col is year
      last_year <- final_year[, -(1:2)]
      
      ## grab idx
      idx <- final_year[,2]
      
      ## unlist all draws into vector, apply forecast, and convert back to matrix
      final_year <- as.vector(final_year[, -(1:2)]) ## unlist only the draw columns
      
      ## unlist aroc draw matrix
      aroc_draws <- as.vector(aroc_draws)
      
      proj_draws_list <- list()
      
      ## make projection from final_yr out proj_dur years
      for (yr in proj_years) {
        
        message(paste0('--- year: ', yr))
        
        ## figure out how many years to project
        proj_dur <- as.numeric(yr) - max(year_list)
        
        if(uselogit){
          proj_draws_logit <- log(final_year / (1 - final_year)) + (aroc_draws * proj_dur)
          proj_draws <- exp(proj_draws_logit) / (1 + exp(proj_draws_logit))
        }else{
          proj_draws <- final_year * exp(aroc_draws * proj_dur)
        }
        
        ## convert back to matrix
        proj_draws <- matrix(proj_draws, ncol = num_draws)
        
        ## append spatial index
        proj_draws <- cbind(spatial_idx, proj_draws)
        
        ## insert into list
        proj_draws_list[[as.character(yr)]] <- proj_draws
        rm(proj_draws)
        
      }
      
      ## save
      message('-- saving projections')
      lapply(1:length(proj_draws_list), function(i) {
        saveRDS(object = proj_draws_list[[i]],
                file = sprintf('%s/%s_%s_%s_projections_adm%i_draw_matrix%s.RDs',
                               output_dir, ind, measure, names(proj_draws_list)[i],
                               aa, ifelse(uselogit, "_logit", "")))
      })
    } # close admin levels loop
  } # if ('admin' %in% type)
  
}

## compare_to_target #####################################################

#' Runs comparison of projected years against a set of goals defined in a goal object
#'
#' @param ind_gp indicator group
#' @param ind indicator
#' @param rd run_date
#' @param goal_obj An existing goal object made with \code{add_goal()}
#' @param measure prevalence, incidence, mortality, etc
#' @param year_list Vector (integer) of years included in the model run / cell pred object
#' @param uselogit Should this be done in logit space?
#' @param skip_cols columns to skip when reading in the cell preds
#'   For example, if the first two columns store non-pred information in your
#'   file format, \code{skip_cols = 2} will read in all columns from 3 onwards
#' @param matrix_pred_name In \code{sprintf} notation. The one object passed into
#'   the string should will be a region name. this allows different regions to be
#'   passed to different named matrix_preds (pixel level, ad0, ad1, ad2, ...)
#'   e.g. 'had_diarrhea_cell_draws_eb_bin0_%s_diarrhea2_0.RData' which
#'   will be passed to sprintf('had_diarrhea_cell_draws_eb_bin0_%s_0.RData', reg)
#' @param shapefile_version character string indicating version of shapefile to pull
#' @return generates cell- and admin- level probabilities of meeting the specified goal,
#'   according to what is specified in \code{goal_obj}.  Objects are written to
#'   standard directories and formats in the 'pred_derivatives' folder of the model run.
#'   cell-level objects are in the cell_pred indexed format, but are no longer
#'   wide by draw (just a single column as they are probabilities).  admin objects are
#'   saved both as rds files and as .csv files with admin hierarchy appended.
#' @examples
#' # Define goals: start by initializing goal object
#' goals <- add_goal(target_year = 2030,
#'                   target = 0.8,
#'                   target_type = "greater",
#'                   abs_rel = "absolute",
#'                   pred_type = c("cell", "admin"))
#'
#' # Run comparison to target
#' compare_to_target(ind_gp = indicator_group,
#'                   ind = indicator,
#'                   rd = run_date,
#'                   goal_obj = goals,
#'                   measure = "prevalence",
#'                   year_list = c(2000:2015),
#'                   uselogit = T)

compare_to_target <- function(ind_gp,
                              ind,
                              rd,
                              regions=NULL,
                              goal_obj,
                              measure,
                              year_list = c(2000:2015),
                              uselogit,
                              raked, 
                              skip_cols = NULL,
                              matrix_pred_name = NULL,
                              shapefile_version = 'current'){
  
  ## load in regions used in this model and run_date
  if (regions %>% is.null) {
    regions <- get_output_regions(in_dir = paste0('#REDACTED',
                                                  ind_gp, '/',
                                                  ind, '/output/',
                                                  rd))
  }
  
  # define directories
  share_dir <- paste0('#REDACTED', ind_gp, '/', ind, '/output/', rd, '/')
  aroc_dir <- paste0(share_dir, "/pred_derivatives/aroc/")
  proj_dir <- paste0(share_dir, "/pred_derivatives/proj/")
  output_dir <- paste0(share_dir, "/pred_derivatives/target_probs/")
  
  dir.create(output_dir, showWarnings = F, recursive = T)
  
  
  for (n in 1:nrow(goal_obj)) {
    
    # Load parameters for this row
    target_year <- as.integer(goal_obj[n, target_year])
    target <- as.numeric(goal_obj[n, target])
    target_type <- as.character(goal_obj[n, target_type])
    abs_rel <- as.character(goal_obj[n, abs_rel])
    if (abs_rel == "relative") baseline_year <- as.integer(goal_obj[n,baseline_year])
    proj <- as.logical(goal_obj[n, proj])
    pred_type <- as.character(goal_obj[n, pred_type])
    goal_type <- as.character(goal_obj[n, target_type])
    message(paste0("\n------------------------------------------------",
                   "\nWorking on target comparison for:",
                   "\n  target_year: ", target_year,
                   "\n  target: ", target,
                   "\n  target_type: ", target_type,
                   "\n  abs_rel: ", abs_rel,
                   ifelse(abs_rel == "relative", paste0("\n  baseline_year: ",baseline_year), ""),
                   "\n  proj: ", proj,
                   "\n  pred_type: ", goal_obj[n, pred_type]), "\n") # character version for pred_type
    
    # TODO: build in ability to use cell_pred objects too
    if(proj == F) stop("For now, can only use projected values")
    
    if (grepl('cell', pred_type, fixed=TRUE)) {
      message('Working on CELL level')
      
      for(ii in 1:length(regions)){
        message(sprintf('- On region: %s', regions[ii]))
        
        # Load proj_draws object
        message("-- Loading proj_draws object...")

        proj_draws <- readRDS(sprintf('%s/%s_%s_%s_projections_cell_draw_matrix%s_%s.RDs',
                                      proj_dir, ind, measure, target_year,
                                      ifelse(uselogit, "_logit",""), regions[ii]))
        
        # Generate probabilities
        if (abs_rel == "absolute") {
          
          # Calculate cell-wise probability of meeting the goal
          if (target_type == "greater") {
            absolute_goal_draws <- ifelse(proj_draws >= target, 1, 0)
          } else if (target_type == "less") {
            absolute_goal_draws <- ifelse(proj_draws <= target, 1, 0)
          }
          
          absolute_goal_draws[is.na(absolute_goal_draws)] <- 0
          absolute_goal_prob <- rowMeans(absolute_goal_draws)
          
          # Save
          message("-- Saving probabilities...")
          saveRDS(object = absolute_goal_prob,
                  file = paste0(output_dir, ind, "_", measure, "_", target_year, "_", abs_rel, "_",
                                target_type, '_', target, '_cell_target_probs_', regions[ii], '.RDs'))
          
          rm(absolute_goal_prob, absolute_goal_draws)
          
        } else if (abs_rel == "relative") {
          
          # Need to load the pred files for this one...
          # Pull cell pred
          cell_pred <- get_cell_pred_for_aroc(ind_gp,
                                              ind,
                                              rd,
                                              regions[ii],
                                              measure,
                                              rk = raked, 
                                              matrix_pred_name,
                                              skip_cols,
                                              shapefile_version = shapefile_version)
          
          ## grab baseline year preds
          year_idx = which(year_list == baseline_year)
          baseline_year_draws <- cell_pred[which(cell_pred[, 1] == year_idx), ] ## first col is year
          baseline_year_draws <- baseline_year_draws[, -(1:2)]
          
          # Now do the comparisons
          if (target_type == "greater") {
            relative_proj_draws <- ifelse(proj_draws / baseline_year_draws >= 1 - target, 1, 0)
          } else if (target_type == "less") {
            relative_proj_draws <- ifelse(proj_draws / baseline_year_draws <= 1 - target, 1, 0)
          }
          relative_proj_draws[is.na(relative_proj_draws)] <- 0
          relative_goal_prob <- rowMeans(relative_proj_draws)
          
          # Save
          message("-- Saving probabilities...")
          saveRDS(object = relative_goal_prob,
                  file = paste0(output_dir, ind, "_", measure, "_", target_year, "_vs_",
                                baseline_year, "_", abs_rel, "_", target_type, '_', target,
                                '_cell_target_probs_', regions[ii], '.RDs'))
          
          rm(relative_goal_prob, relative_proj_draws)
          
        } # close abs/relative if/then/else
      } #close regions loop
    } # close cell loop
    
    if (grepl('admin', pred_type, fixed=TRUE)) {
      message('Working on ADMIN level')
      
      # Load sp hierarchy
      sp_h <- get_sp_hierarchy(shapefile_version = shapefile_version)
      
      for(aa in 0:2) { ## for each admin type
        
        # Load proj_draws object
        message("-- Loading proj_draws object...")
        proj_draws <- readRDS(sprintf('%s/%s_%s_%s_projections_adm%i_draw_matrix%s.RDs',
                                      proj_dir, ind, measure, target_year, aa,
                                      ifelse(uselogit, "_logit","")))
        
        # Split off spatial index
        spatial_idx <- proj_draws[,1]
        proj_draws <- proj_draws[, 2:ncol(proj_draws)]
        
        # Generate probabilities
        if (abs_rel == "absolute") {
          if (target_type == "greater") {
            absolute_goal_draws <- ifelse(proj_draws >= target, 1, 0)
          } else if (target_type == "less") {
            absolute_goal_draws <- ifelse(proj_draws <= target, 1, 0)
          }
          
          absolute_goal_draws[is.na(absolute_goal_draws)] <- 0
          absolute_goal_prob <- rowMeans(absolute_goal_draws)
          
          # Add spatial index
          absolute_goal_prob <- cbind(spatial_idx, absolute_goal_prob)
          
          # Save
          message("-- Saving probabilities...")
          
          # RDS
          saveRDS(object = absolute_goal_prob,
                  file = paste0(output_dir, ind, "_", measure, "_", target_year, "_", abs_rel, "_",
                                target_type, '_', target, '_adm_', aa, '_target_probs.RDs'))
          
          # CSV
          absolute_goal_prob <- merge_sp_hierarchy(df = absolute_goal_prob,
                                                   admin_level = aa,
                                                   idx_col = "spatial_idx",
                                                   sp_h = sp_h)
          
          write.csv(absolute_goal_prob,
                    file = paste0(output_dir, ind, "_", measure, "_", target_year, "_", abs_rel, "_",
                                  target_type, '_', target, '_adm_', aa, '_target_probs.csv'),
                    row.names = F)
          
        }
        
        ## Did it meet relative goal?
        if (abs_rel == "relative") {
          ## load the admin objects
          ## try two different locations until we standardize
          message('- loading admin objects')
          file_1 <- sprintf('#REDACTED',
                            ind_gp, ind, rd, ind, measure)
          file_2 <- paste0('#REDACTED', ind_gp, '/', ind, '/output/', rd, '/',
                           ind, '_raked_admin_draws_eb_bin0_0.RData')
          
          if (file.exists(file_1)) {
            load(file_1)
          } else if (file.exists(file_2)) {
            load(file_2)
          } else {
            stop("Cannot load admin pred object!")
          }
          
          # Need to load "pseudo cell_pred" admin object to get baseline year
          cell_pred <- get(sprintf('admin_%i', aa))
          cell_pred <- as.data.table(cell_pred)
          
          ## format this object to look like the one we used for cell level (easier to copy code from above)
          ## revised now to work with a bigger array of cell pred objects
          str_match <- stringr::str_match
          draw_cols <- names(cell_pred)[grep("V[0-9]*", names(cell_pred))]
          keep_cols <- c("year", paste0("ADM", aa, "_CODE"), draw_cols)
          cell_pred <- subset(cell_pred, select = keep_cols)
          setnames(cell_pred, paste0("ADM", aa, "_CODE"), "idx")
          cell_pred <- as.matrix(cell_pred)
          
          num_draws <- ncol(cell_pred) - 2
          
          # Redid the below line to warn if mismatches in case of non-unique admin codes - JM
          num_idx <- nrow(cell_pred) / length(year_list)
          if (num_idx != length(unique(cell_pred[,2]))) {
            warning(paste0("The number of unique cell_pred indices does not equal the number of ",
                           "rows of cell_pred divided by the number of years.",
                           "\nYou may have duplicate admin codes at the admin ", aa,
                           " level - check your aggregated objects!"))
          }
          
          # Assess relative goals
          if (target_type == "greater") {
            relative_proj_draws <- ifelse(proj_draws / baseline_year_draws >= 1 - relative_goal, 1, 0)
          } else if (target_type == "less") {
            relative_proj_draws <- ifelse(proj_draws / baseline_year_draws <= 1 - relative_goal, 1, 0)
          }
          relative_proj_draws[is.na(relative_proj_draws)] <- 0
          relative_goal_prob <- rowMeans(relative_proj_draws)
          
          # Add spatial index
          relative_goal_prob <- cbind(spatial_idx, relative_goal_prob)
          
          message("-- Saving probabilities...")
          saveRDS(object = relative_goal_prob,
                  file = paste0(output_dir, ind, "_", measure, "_", target_year, "_vs_",
                                baseline_year, "_", abs_rel, "_", target_type, '_', target,
                                '_adm_', aa, '_target_probs.RDs'))
          
          # CSV
          relative_goal_prob <- merge_sp_hierarchy(df = relative_goal_prob,
                                                   admin_level = aa,
                                                   idx_col = "spatial_idx",
                                                   sp_h = sp_h)
          
          write.csv(relative_goal_prob,
                    file = paste0(output_dir, ind, "_", measure, "_", target_year, "_vs_",
                                  baseline_year, "_", abs_rel, "_", target_type, '_', target,
                                  '_adm_', aa, '_target_probs.csv'),
                    row.names = F)
          
        } # close relative loop
      } # close admin loop
    } # close if_admin section
  } # close goal_obj row loop
}


## Description:   Prepare rasters for aroc, proj and target_probs 
##
## Inputs:        .RDs objects (/share/geospatial/mbg/[indicator_group]/[indicator]/output/[run_date]/pred_derivatives/[pred_deriv_name])
##
## Output:        .tif rasters (/share/geospatial/mbg/[indicator_group]/[indicator]/output/[run_date]/[indicator_raster_name].tif)
## 
####################################################################################################

##' @param pred_deriv indicator of which object to prepare the raster for. Options:  "aroc" / "proj" / "target_probs"
##' @param raking raked object or not? Options: "raked" / "unraked"
##' @param abs_rel absolute or relative target_probs? Options: "absolute" / "relative" 
##'                If none provided, default value is assigned
##' @param target_yr specify target year (for example, GNT 2025 target) 
##'                If none provided, default value is assigned
##' @param target specify target (for example, 0.5 -- GNT 2025 target for EBF)
##' @param target_type indicator of target type. Options: "greater", "less"
##' @param baseline_year specify baseline year you are comparing to (for example, 2017, the last year in modeling)
##'                If none provided, default value is assigned
##' @param shapefile_version specify which shapefile version should be used
##'                If none provided, default value 'current' is assigned

prepare_aroc_proj_rasters <- function(regions='all',
                                      pred_deriv, 
                                      raking, 
                                      uselogit,
                                      indicator_group,
                                      indicator,
                                      measure,
                                      run_date,
                                      abs_rel = NULL,
                                      target_yr = NULL,
                                      target,
                                      target_type, 
                                      baseline_year = NULL, 
                                      shapefile_version = 'current',
                                      debug=F){
  
  if(debug) browser()
  
  # Set up default values for arguments if not specified
  if (is.null(abs_rel)) abs_rel <- "absolute"
  if (is.null(target_yr)) target_yr <- 2025
  if (is.null(baseline_year)) baseline_year <- 2018
  
  # Define dir to search
  input_dir <- file.path('#REDACTED', indicator_group, indicator,
                         'output', run_date, 'pred_derivatives', pred_deriv, '/')
  
  message('searching for results in\n', input_dir)

  #helper function to loop over each region and create the projection raster
  regLoop <- function(reg, dir=input_dir) {
    
    message('working on ', reg)

    ## Load simple polygon template to model over
    gaul_list <- get_adm0_codes(reg, shapefile_version = shapefile_version)
    simple_polygon_list <- load_simple_polygon(
      gaul_list = gaul_list, buffer = 1, tolerance = 0.4,
      shapefile_version = shapefile_version
    )
    subset_shape <- simple_polygon_list[[1]]
    simple_polygon <- simple_polygon_list[[2]]
    
    ## Load list of raster inputs (pop and simple)
    raster_list <- build_simple_raster_pop(subset_shape, link_table = shapefile_version) #uses new simple_raster 
    
    simple_raster <- raster_list[["simple_raster"]]
    pop_raster <- raster_list[["pop_raster"]]
    pixel_id <- seegSDM:::notMissingIdx(simple_raster)
    
    message('loading links')
    #####################################################################
    # load the cell id to admin units link
    link_table <- get_link_table(simple_raster, shapefile_version = shapefile_version)
    
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
      rake_subnational=T, 
      reg = reg,
      shapefile_version = shapefile_version
    )
    
    # merge the connector on to the link table by making sure that each cell fragment gets connected to the appropriate
    # raking geography
    link <- sub_nat_link_merge(
      rake_subnational=T, #TODO pass in?
      link,
      connector
    )
    
    #read in file based on pred type
    if(pred_deriv == "aroc"){
      cell_deriv <- readRDS(paste0(dir, 
                                   indicator, '_', measure, '_',  pred_deriv,  
                                   "_cell_draw_matrix_", ifelse(uselogit, "logit_", ""), reg, '.RDs'))
      
      ## summarize raked predictions for each cell
      mean_deriv_raster <- make_cell_pred_summary(draw_level_cell_pred = cell_deriv,
                                                  mask                 = simple_raster,
                                                  return_as_raster     = TRUE,
                                                  summary_stat         = 'mean')
    }
    
    if(pred_deriv == "proj"){
      cell_deriv <- readRDS(paste0(dir,
                                   indicator, '_', measure, '_',  target_yr, "_projections_cell_draw_matrix_", 
                                   ifelse(uselogit, "logit_", ""), reg, '.RDs'))
      
      ## summarize raked predictions for each cell
      mean_deriv_raster <- make_cell_pred_summary(draw_level_cell_pred = cell_deriv,
                                                  mask                 = simple_raster,
                                                  return_as_raster     = TRUE,
                                                  summary_stat         = 'mean')
    }
    
    if(pred_deriv == "target_probs"){
      if(abs_rel == "absolute"){
        cell_deriv <- readRDS(paste0(dir,
                                     indicator, '_', measure, '_',  target_yr, "_", abs_rel,"_", target_type, "_", 
                                     target, "_cell_target_probs_", reg, '.RDs'))
        
        yrs = length(cell_deriv)/length(cellIdx(simple_raster))
        message(sprintf('Making a RasterBrick with %i layers', yrs))
        mean_deriv_raster <- insertRaster(simple_raster,  matrix(cell_deriv,  ncol = yrs))
        
      }
      else{
        cell_deriv <- readRDS(paste0(dir, 
                                     indicator, '_', measure, '_', target_yr, "_vs_", baseline_year, "_", abs_rel,
                                     "_", target_type, "_", target, "_cell_target_probs_", reg, '.RDs'))
      }
    }

    #set cell pred as a data table, and rename things
    cell_pred <- matrix(cell_deriv,  ncol = yrs) %>% 
      as.data.table %>% 
      setnames('V1', 'mean')
    
    cell_pred[, cell_pred_id := .I] #cell_pred object ID
    cell_pred[, cell_id := rep(link_table[[2]], times = nrow(cell_pred) / length(link_table[[2]]))]  #cell id references the africa map
    cell_pred[, pixel_id := rep(pixel_id, times = nrow(cell_pred) / length(pixel_id))] #pixel id references the regional map  
    
    #generate year variable in order to merge on pops
    cell_pred[, year := 1:.N, by='pixel_id']
    #TODO set up to use multiple years
    cell_pred[, year := target_yr] #shift from idx to the actual year using min year in the provided list

    # merge cell_pred on the link
    # TODO note that pixel_id col in link dt is a duplicate of ID, and causes a merge issue (pixel_id.x|pixel_id.y)
    # eventually should fix this issue upstream but for now removing it pre-merge is sufficient
    cell_pred <- merge(link[, -c('pixel_id')], cell_pred, by.x = "ID", by.y = "cell_id", allow.cartesian = TRUE)
    
    #add total population info in order to weight ad2s
    cell_pred <- load_populations_cov(reg, pop_measure='total', measure = 'count', simple_polygon, 
                                      simple_raster, year_list=target_yr, interval_mo=12, pixel_id = pixel_id) %>% 
      merge(., cell_pred, by=c('pixel_id', 'year'))
    
    #helper function to collapse
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
        unique(., by=key(.)) %>% 
        return
      
    }
    
    #agg ad0/2
    ind_cols <- 'mean'
    ad0 <- aggResults(cell_pred, 
                      by_cols=c('ADM0_CODE', 'year'), 
                      agg_cols=ind_cols)
    ad2 <- aggResults(cell_pred, 
                      by_cols=c('ADM0_CODE', 'ADM2_CODE', 'year'), 
                      agg_cols=ind_cols)

    list('raster'=mean_deriv_raster,
         'ad0'=ad0,
         'ad2'=ad2) %>% 
      return

  }

  if (regions=='all') regions <- list.files(input_dir) %>% stringr::str_match(., ".*_([^\\.]*)\\.RDs*") %>% .[,2]
  if (length(regions)>1) {
    
    out <- mclapply(regions, regLoop, mc.cores=3)
  
  } else out <- regLoop(regions) 
  
  return(out)
  
}
