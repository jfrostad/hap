## summarize_admins ################################################

#' Function to summarize admin_pred objects
#'
#' This is a wrapper for `make_admin_pred_summary()`
#'
#' @param ind indicator
#' @param ig indicator_group
#' @param summstats Summary statistics (functions) to compute.
#'                  Order will be the order they are written in csv
#'                  This is passed to `make_admin_pred_summary()`
#' @param raked Raked (T), unraked (F), or both (`c(T,F)`)?
#' @param ad_levels Admin levels to summarize (0, 1, and 2 by default)
#' @return Writes csv files to `sharedir/pred_derivatives/admin_summaries/`
#' @examples
#' summarize_admins(summstats = c("mean", "lower", "upper", "cirange"),
#'                  ad_levels = c(0,1,2),
#'                  raked     = c(T,F))

summarize_admins <- function(ind = indicator,
                             ig = indicator_group,
                             summstats = c("mean", "lower", "upper", "cirange"),
                             raked = c(T,F),
                             ad_levels = c(0,1,2),
                             file_addin = NULL,
                             measure = 'prevalence',
                             metrics = c('rates', 'counts'),
                             ...) {
  
  sharedir       <- sprintf('/share/geospatial/mbg/%s/%s',ig,ind)
  input_dir <- paste0(sharedir, "/output/", run_date, "/")
  output_dir <- paste0(input_dir, "/pred_derivatives/admin_summaries/")
  dir.create(output_dir, recursive = T, showWarnings = F)
  
  # Convert raked to character
  rr <- character()
  if (T %in% raked) rr <- c(rr, paste0("raked_", measure))
  if (F %in% raked) rr <- c(rr, "unraked")
  
  # If file_addin present, use it
  if (!is.null(file_addin)) file_addin <- paste0("_", file_addin)
  if (is.null(file_addin)) file_addin <- ""
  
  # Summarize and save admin preds
  for (metric in metrics) {
    for (rake in rr) {
      load(paste0(input_dir, ind, "_", rake, ifelse(metric == "counts", "_c", ""), "_admin_draws_eb_bin0_0.RData"))
      sp_hierarchy_list <- mutate_if(sp_hierarchy_list, is.factor, as.character)
      sp_hierarchy_list <- mutate_at(sp_hierarchy_list, grep('_CODE', names(sp_hierarchy_list), value = T), as.numeric)
      
      for (ad in ad_levels) {
        message(paste0("Summarizing ", ind, ": admin ", ad, " (", rake, ")"))
        ad_summary_table <- make_admin_pred_summary(admin_pred = get(paste0("admin_", ad)),
                                                    sp_hierarchy_list,
                                                    summary_stats = summstats,
                                                    ...)
        fwrite(ad_summary_table,
               file = paste0(output_dir, ind, ifelse(metric == "counts", "_c", ""), "_admin_", ad, "_", rake, file_addin, "_summary.csv"))
      }
    }
  }
}

#' @title Combine aggregation
#' @description Combine aggregation objects across region
#' @param run_date, indicator, indicator_group for this run
#' @param ages: single value or vector of ages
#' @param regions: vector of regions used
#' @param holdouts: vector of holdouts used, e.g. just 0 or c(1,2,3,4,5,0)
#' @param raked: vector of raked values, e.g. just T, just F, or c(T,F)
#' @param dir_to_search: which directory to search in (defaults to share directory)
#' @param delete_region_files: logical. Should we delete the region-specific intermediate files?
#' @param merge_hierarchy_list: logical. Do you want to merge the sp_hierarchy_list onto your admin tables?
#' @param check_for_dupes PARAM_DESCRIPTION, Default: F
#' @return rdata files for each combo of age/holdout/raked
#'   each with admin_0, admin_1, admin_2 data table objects & the sp_hierarchy_list object
#'   that maps them to names of admin units
combine_aggregation <- function(rd = run_date,
                                indic = indicator,
                                ig = indicator_group,
                                ages,
                                regions,
                                holdouts,
                                raked,
                                dir_to_search = NULL,
                                delete_region_files = T,
                                merge_hierarchy_list = F,
                                check_for_dupes = F) {
  
  # Combine aggregation objects across region
  # Jon Mosser / jmosser@uw.edu
  
  # Args:
  #   run_date, indicator, indicator_group for this run
  #   ages: single value or vector of ages
  #   regions: vector of regions used
  #   holdouts: vector of holdouts used, e.g. just 0 or c(1,2,3,4,5,0)
  #   raked: vector of raked values, e.g. just T, just F, or c(T,F)
  #   dir_to_search: which directory to search in (defaults to share directory)
  #   delete_region_files: logical. Should we delete the region-specific intermediate files?
  #   merge_hierarchy_list: logical. Do you want to merge the sp_hierarchy_list onto your admin tables?
  
  # Outputs:
  #   rdata files for each combo of age/holdout/raked
  #   each with admin_0, admin_1, admin_2 data table objects & the sp_hierarchy_list object
  #   that maps them to names of admin units
  
  if (is.null(dir_to_search)) {
    dir_to_search <- paste0("/share/geospatial/mbg/",ig,"/",indic,"/output/",run_date,"/")
  }
  
  message("Combining aggregation results...")
  
  for(rake in raked) {
    for (holdout in holdouts) {
      for (age in ages) {
        message(paste0("\nWorking on age: ", age, " | holdout: ", holdout, " | raked: ", rake))
        
        # Set up lists
        ad0 <- list()
        ad1 <- list()
        ad2 <- list()
        sp_h <- list()
        
        
        for (reg in regions) {
          message(paste0("  Region: ", reg))
          
          load(paste0(dir_to_search, indic, "_", ifelse(rake, "raked", "unraked"),
                      "_admin_draws_eb_bin", age, "_", reg, "_", holdout, ".RData"))
          
          if(merge_hierarchy_list == T) {
            # Prepare hierarchy list for adm0
            ad0_list <- subset(sp_hierarchy_list, select = c("ADM0_CODE", "ADM0_NAME", "region")) %>% unique
            
            # Prepare hierarchy list for adm1
            ad1_list <- subset(sp_hierarchy_list,
                               select = c("ADM0_CODE", "ADM1_CODE", "ADM0_NAME", "ADM1_NAME", "region")) %>%
              unique
            
            # Merge
            admin_0 <- merge(ad0_list, admin_0, by = "ADM0_CODE", all.y = T)
            admin_1 <- merge(ad1_list, admin_1, by = "ADM1_CODE", all.y = T)
            admin_2 <- merge(sp_hierarchy_list, admin_2, by = "ADM2_CODE", all.y = T)
            rm(ad0_list, ad1_list)
          }
          if(check_for_dupes){
            adms <- get_adm0_codes(reg)
            sp_hier <- get_sp_hierarchy()
            include_ad0 <- sp_hier$ADM0[ADM0_CODE %in% adms, ADM0_CODE]
            include_ad1 <- sp_hier$ADM1[ADM0_CODE %in% adms, ADM1_CODE]
            include_ad2 <- sp_hier$ADM2[ADM0_CODE %in% adms, ADM2_CODE]
            
            ad0[[reg]] <- admin_0[ADM0_CODE %in% include_ad0]
            ad1[[reg]] <- admin_1[ADM1_CODE %in% include_ad1]
            ad2[[reg]] <- admin_2[ADM2_CODE %in% include_ad2]
            sp_h[[reg]] <- sp_hierarchy_list
          } else{
            ad0[[reg]] <- admin_0
            ad1[[reg]] <- admin_1
            ad2[[reg]] <- admin_2
            sp_h[[reg]] <- sp_hierarchy_list
          }
          
          rm(admin_0, admin_1, admin_2, sp_hierarchy_list)
        }
        
        # Get to long format & save
        message("  Combining...")
        admin_0 <- rbindlist(ad0)
        admin_1 <- rbindlist(ad1)
        admin_2 <- rbindlist(ad2)
        sp_hierarchy_list <- rbindlist(sp_h)
        
        message("  Saving combined file...")
        save(admin_0, admin_1, admin_2, sp_hierarchy_list,
             file = paste0(dir_to_search, indic, "_",
                           ifelse(rake, "raked", "unraked"),
                           "_admin_draws_eb_bin", age, "_",
                           holdout, ".RData"))
      }
    }
  }
  
  if (delete_region_files == T) {
    # Make sure all full files are written
    combos <- expand.grid(ifelse(raked, "raked", "unraked"), ages, holdouts)
    files_to_check <- sapply(1:nrow(combos), function(i) {
      paste0(dir_to_search, indic, "_", combos[i, 1],
             "_admin_draws_eb_bin", combos[i, 2], "_", combos[i,3], ".RData")
    })
    
    if (all(file.exists(files_to_check))) {
      message("All anticipated combined files were created successfully.  Deleting intermediate files...")
      combos <- expand.grid(ifelse(raked, "raked", "unraked"), ages, regions, holdouts)
      files_to_delete <- sapply(1:nrow(combos), function(i) {
        paste0(dir_to_search, indic, "_", combos[i, 1],
               "_admin_draws_eb_bin", combos[i, 2], "_", combos[i,3], "_", combos[i, 4], ".RData")
      })
      unlink(files_to_delete)
    } else {
      warning("Did not delete intermediate files - not all output files created successfully!")
    }
  }
  
  # Finally, delete the "fin" files
  fin_files_to_delete <- list.files(dir_to_search, pattern = "fin_agg_", full.names=T)
  unlink(fin_files_to_delete)
}
