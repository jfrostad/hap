#' @title Set up pre-run image path
#'
#' @description Return the path to save model data prior to running parallel script.
#'
#' @param indicator_group the indicator group e.g., "hiv" or "u5m".
#'
#' @param indicator the indicator e.g., "wasting_mod_b" or "male_circumcision".
#'
#' @param run_date string run date in YYYY_MM_DD_HH_MM_SS format.
#'
#' @param age the age for the model which uses this data.
#'
#' @param region str region name.
#'
#' @param holdout numeric holdout value (0 for no holdout).
#'
#' @return the path to save the file.
#'
#' @export
pre_run_image_path <- function(indicator_group, indicator, run_date, age, region, holdout) {
  f <- sprintf("pre_run_tempimage_%s_bin%s_%s_%s.RData", run_date, age, region, holdout)
  path_join(get_indicator_dir(indicator_group, indicator), "model_image_history", f)
}
