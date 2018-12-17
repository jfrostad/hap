## Master script to produce validation reports for entire modeling area and each country within.
## Load arguments from qsub
indicator       = as.character(commandArgs()[4])
indicator_group = as.character(commandArgs()[5])
run_date        = as.character(commandArgs()[6])
pop_measure     = as.character(commandArgs()[7])
repo            = as.character(commandArgs()[8])
regions         = as.character(commandArgs()[9])
target_type     = as.character(commandArgs()[10])
target          = as.numeric(commandArgs()[11])
shapefile_version <- as.character(commandArgs()[12])

# indicator <- 'edu_mean'
# indicator_group <- 'education'
# run_date <- '2017_05_09_15_13_20'
# pop_measure <- 'wocba'
# repo <- '/share/code/geospatial/ngraetz/mbg'
# regions <- 'essa_hilo'
# target_type <- '>='
# target <- 6
# shapefile_version <- 'current'

# indicator <- 'had_diarrhea'
# indicator_group <- 'diarrhea'
# run_date <- '2017_06_02_16_15_35'
# pop_measure <- 'total'
# repo <- '/share/code/geospatial/ngraetz/mbg'
# regions <- 'name_diarrhea2'
# target_type <- 'less'
# target <- 0.05
# shapefile_version <- 'current'

  ##################################################################################
  ## Setup.
  ##################################################################################
    message('Setting up...')

      ## Define main directories.
        results_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date, '/')
        val_dir <- paste0(results_dir, 'validation_report')
        dir.create(val_dir, showWarnings = FALSE)

      ## Load libraries and miscellaneous MBG project functions.
        setwd(repo)
        root <- "/home/j/"
        package_lib <- ifelse(grepl("geos", Sys.info()[4]),
                      paste0(root,'temp/geospatial/geos_packages'),
                      '/snfs1/temp/ngraetz/special_packages/mbg_pkgs_conda')
                       # Library for all MBG versioned packages. Ensures that none of this code is
        #    dependent on the machine where the user runs the code.
        .libPaths(package_lib)                                  # Ensures packages look for dependencies here when called with library().
        #    Necessary for seeg libraries.
        source('mbg_central/mbg_functions.R')                   # Functions to run MBG model.
        source('mbg_central/prep_functions.R')                  # Functions to setup MBG run
        source('mbg_central/covariate_functions.R')             # Functions to prep and transform 5*5 covariates
        source('mbg_central/misc_functions.R')                  # Other computational MBG-related functions.
        source('mbg_central/post_estimation_functions.R')
        source('mbg_central/gbd_functions.R')
        source('mbg_central/shiny_functions.R')
        source('mbg_central/holdout_functions.R')
        source('education/pop_splitting_functions.R')
        source('mbg_central/seegMBG_transform_functions.R')
        source('mbg_central/validation_report_functions.R')
        package_list <- c('fields', 'gridGraphics' ,'grid', 'gridExtra', 'gstat', 'magrittr', 'ggplot2', 'doParallel', 'SDMTools', 'foreign', 'rgeos', 'data.table','raster','rgdal','INLA','seegSDM','seegMBG','plyr','dplyr','leaflet')
        for(package in package_list) {
          library(package, lib.loc = package_lib, character.only=TRUE)
        }

        #all_gauls       = get_adm0_codes('africa')
        year_list       = c(2000:2015)

      ## Set up your target (e.g. for SDG comparisons)

        if(target_type == 'greater') target_type <- '>='
        if(target_type == 'less') target_type <- '<='

      ## Check if has holdouts
        n_holdouts <- check_for_holdouts(results_dir)

  ##################################################################################
  ## Common objects for all validation functions.
  ##################################################################################
  ##  - Raking factors
  ##  - Raked/unraked results
  ##  - Populations
  ##  - Shapes to define modeling area
  ##  - Make master list of countries, where each item contains a specific country's draws, pops, and templates.
  ##################################################################################
    message('Defining and loading common objects...')

      ## Define path to csv of all raking factors, load in data.table.
        in_dir  <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date)
        default_rf_path <- paste0(in_dir, '/', indicator, '_rf.csv')
          all_rfs <- fread(default_rf_path)

      ## Define path to .tif of results and raked results, load in raster bricks.
        default_raked_results_path <- paste0(in_dir, '/', indicator, '_mean_raked_raster.tif')
          results_raked <- brick(default_raked_results_path)
        default_results_path <- paste0(in_dir, '/', indicator, '_mean_raster.tif')
          results <- brick(default_results_path)

      ## Load admin2 raster
          admin_level <- 2
          shapes <- shapefile(paste0("/snfs1/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin", admin_level, "/g2015_2014_", admin_level, "/g2015_2014_", admin_level, "_modified.shp"))

      ## Load regional pops
          simple_polygon_list <- load_simple_polygon(gaul_list = get_adm0_codes(regions, shapefile_version = shapefile_version),
                                                     buffer = 0.4,
                                                     subset_only = FALSE,
                                                     shapefile_version = shapefile_version)
          subset_shape   <- simple_polygon_list[[1]]
          simple_polygon <- simple_polygon_list[[2]]
          pop_raster_annual <- suppressMessages(suppressWarnings(load_and_crop_covariates_annual(covs = 'worldpop',
                                                                                                 measures = pop_measure,
                                                                                                 simple_polygon = simple_polygon,
                                                                                                 start_year  = min(year_list),
                                                                                                 end_year    = max(year_list),
                                                                                                 interval_mo = 12,
                                                                                                 agebin      = 1)))

      ## Make master list
        total_periods <- length(names(results))
        #regions <- get_output_regions(in_dir)

        admin_level <- 0
        admin0 <- shapefile(paste0("/snfs1/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin", admin_level, "/g2015_2014_", admin_level, "/g2015_2014_", admin_level, ".shp"))

        master_list <- lapply(regions, pull_country_draws,
                              periods = total_periods,
                              in_dir = in_dir,
                              pop_measure = pop_measure,
                              start_year = 2000,
                              end_year = 2015,
                              admin2_shapes = shapes,
                              shapefile_version = shapefile_version, 
                              all_region_pops = pop_raster_annual[[1]])
        master_list <- do.call(c, unlist(master_list, recursive=FALSE))

  ##################################################################################
  ## Full region report
  ##################################################################################
  ##  - Predictions (raked/unraked) for all countries
  ##  - Raking factors
  ##  - Betas
  ##################################################################################

      all_gauls <- get_adm0_codes(regions, shapefile_version = shapefile_version)

    ## Make quilt plots for all countries before we initialize pdf, because it outputs a lot of intermediate plots along the way.
        df <- load_region_input_data(indicator_group = indicator_group,
                                     indicator = indicator,
                                     run_date = run_date,
                                     reg = regions)

        df <- df[year >= 1998 & year <= 2002, year := 2000]
        df <- df[year >= 2003 & year <= 2007, year := 2005]
        df <- df[year >= 2008 & year <= 2012, year := 2010]
        df <- df[year >= 2013 & year <= 2017, year := 2015]

        all_data <- is_oos_preds(rd = run_date,
                                 all.data = df,
                                 cell_draws_filename = '%s_cell_draws_eb_bin%i_%s_%i.RData', ## in sprintf notation
                                 holdouts = 0, ## number of holdouts. if zero only does in sample
                                 reg = regions,
                                 years = 2000:2015,
                                 indic = indicator,
                                 indic_group = indicator_group,
                                 shapefile_version = shapefile_version, 
                                 holdoutlist = NULL)

        ## Make error variables to map/tabulate (absolute error, RMSE, coverage, etc.)
        if(indicator != 'edu_mean') all_data <- all_data[, IS_abs_error := IS - (get(indicator)/N)]
        if(indicator == 'edu_mean') all_data <- all_data[, IS_abs_error := IS - get(indicator)]

        quilt_plot_list <- lapply(get_adm0_codes(regions, shapefile_version = shapefile_version), plot_quilt,
                                  df = all_data,
                                  sample = "IS_abs_error",
                                  subset_shape = subset_shape)
        names(quilt_plot_list) <- as.character(get_adm0_codes(regions, shapefile_version = shapefile_version))

        regional_quilt <- plot_quilt(gaul_list = get_adm0_codes(regions, shapefile_version = shapefile_version),
                                     df = all_data,
                                     sample = "IS_abs_error",
                                     subset_shape = subset_shape)

    ## Make graphs of calibration vs a given target
      target_data <-  is_oos_preds(rd = run_date,
                                   all.data = df,
                                   cell_draws_filename = '%s_cell_draws_eb_bin%i_%s_%i.RData', ## in sprintf notation
                                   holdouts = 0, ## number of holdouts. if zero only does in sample
                                   reg = regions,
                                   years = 2000:2015,
                                   indic = indicator,
                                   indic_group = indicator_group,
                                   holdoutlist = NULL,
                                   fun = "get_p_target",
                                   target_type = target_type,
                                   target = target)

      target_plot_list <- lapply(get_adm0_codes(regions, shapefile_version = shapefile_version), plot_target,
                                 df = target_data,
                                 sample = "IS",
                                 subset_shape = subset_shape,
                                 target = target,
                                 target_type = target_type)

      names(target_plot_list) <- as.character(get_adm0_codes(regions, shapefile_version = shapefile_version))

      regional_target_plot <- plot_target(gaul_list = get_adm0_codes(regions, shapefile_version = shapefile_version),
                                          df = target_data,
                                          sample = "IS",
                                          subset_shape = subset_shape,
                                          target = target,
                                          target_type = target_type,
                                          region = toupper(regions))

    ## Make covariate importance heat map (if correct outputs are saved for this model run - anything after 6/4/17 should work)
      if(length(list.files(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date), pattern = 'child_model_list')) != 0) {
        cov_importance <- get.cov.wts(rd = run_date,
                                      ind = indicator, ## indicator
                                      ind_gp = indicator_group, ## indicator_group
                                      reg = regions,
                                      age = 0,
                                      holdout = 0,
                                      vallevel = "")
        cov_gg <- plot.cov.wts(rd = run_date,
                               ind = indicator, ## indicator
                               ind_gp = indicator_group, ## indicator_group
                               reg = regions,
                               age = 0,
                               holdout = 0,
                               vallevel = "")
      }

    ## Make full map of raking factors
      rf_maps <- raking_factors_map(indicator = indicator,
                                    indicator_group = indicator_group,
                                    run_date = run_date,
                                    gaul = all_gauls,
                                    year_plot_list = c(2000,2005,2010,2015))

    ## Make plot of betas from INLA
      beta_plot <- make_beta_plot(indicator_group = indicator_group,
                                  indicator = indicator,
                                  run_date = run_date,
                                  region = regions)

    ## Make scatter of all raking factors
      country_rf_plot <- val_raking_factors(gaul_list = all_gauls,
                                            rfs = all_rfs)

    ## Make plots of predictions (raked and unraked)
      pred_maps <- extract_raked_and_unraked(gaul = all_gauls,
                                             results,
                                             results_raked,
                                             master_list,
                                             admin0 = admin0,
                                             load_simple_raster = TRUE,
                                             shapefile_version = shapefile_version)

    ## Run DHS comparison for education (add to config file for this whole script or something later)
      # if(indicator == 'edu_mean') {
      #
      #   ## Load premade .csv of DHS admin1 aggregates for edu_mean and an SPDF with all the relevant location_code/shapefile entries.
      #   load('/share/geospatial/mbg/validation_polygons/dhs_admin1.RData')
      #   dhs_admin1_data <- fread('/share/geospatial/mbg/validation_data/edu_mean_dhs_admin1.csv')
      #
      #   ## Choose which NIDs to compare (all for this region)
      #   loc_names <- fread("/snfs1/WORK/11_geospatial/10_mbg/gaul_to_loc_id.csv")
      #   region_iso3s <- loc_names[GAUL_CODE %in% get_adm0_codes(regions), ihme_lc_id]
      #   region_nid_list <- unique(dhs_admin1_data[iso3 %in% region_iso3s, nid])
      #
      #   ## Plot comparison
      #   unraked_dhs_plot <- compare_admin_estimates(nid_list = region_nid_list,
      #                                               results_raster = results,
      #                                               compare_source_title = 'DHS Admin1',
      #                                               raked_title = 'unraked',
      #                                               outcome_title = 'Mean years',
      #                                               compare_data = dhs_admin1_data,
      #                                               compare_spdf = poly_shapes_all,
      #                                               master_list = master_list)
      #
      #   raked_dhs_plot <- compare_admin_estimates(nid_list = region_nid_list,
      #                                             results_raster = results_raked,
      #                                             compare_source_title = 'DHS Admin1',
      #                                             raked_title = 'raked',
      #                                             outcome_title = 'Mean years',
      #                                             compare_data = dhs_admin1_data,
      #                                             compare_spdf = poly_shapes_all,
      #                                             master_list = master_list)
      #
      # }

    ## Plot errrrthang
    out_dir <- val_dir
    dir.create(out_dir)
    pdf(paste0(out_dir, '/', regions, '.pdf'), width=11, height=18)

      gLegend<-function(a.gplot){
        pdf(NULL) # Workaround for bug in ggplot_gtable causing empty Rplots.pdf to be created
        tmp <- ggplot_gtable(ggplot_build(a.gplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        graphics.off()
        return(legend)
      }
      # grab your legends using the predefined functions, then state their grid location
      map.legend <- gLegend(pred_maps[['raked']])
      map.legend$vp <- viewport(layout.pos.row = 2:14, layout.pos.col = 17:18)
      rf.legend <- gLegend(rf_maps)
      rf.legend$vp <- viewport(layout.pos.row = 15:20, layout.pos.col = 17:18)

      # Initialize plot with master title
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(32, 18)))
      vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
      # Plot all pred maps
      grid.text("Unraked", vp = vplayout(1,1:16), gp=gpar(fontsize=20, fontface='bold'))
      # print(pred_maps[['unraked']][[1]] + theme(legend.position="none"), vp = vplayout(2:5, 1:4))
      # print(pred_maps[['unraked']][[2]] + theme(legend.position="none"), vp = vplayout(2:5, 5:8))
      # print(pred_maps[['unraked']][[3]] + theme(legend.position="none"), vp = vplayout(2:5, 9:12))
      # print(pred_maps[['unraked']][[4]] + theme(legend.position="none"), vp = vplayout(2:5, 13:16))
      print(pred_maps[['unraked']] + theme(legend.position="none"), vp = vplayout(2:7, 1:16))
      grid.text("Raked", vp = vplayout(8,1:16), gp=gpar(fontsize=20, fontface='bold'))
      # print(pred_maps[['raked']][[1]] + theme(legend.position="none"), vp = vplayout(7:10, 1:4))
      # print(pred_maps[['raked']][[2]] + theme(legend.position="none"), vp = vplayout(7:10, 5:8))
      # print(pred_maps[['raked']][[3]] + theme(legend.position="none"), vp = vplayout(7:10, 9:12))
      # print(pred_maps[['raked']][[4]] + theme(legend.position="none"), vp = vplayout(7:10, 13:16))
      print(pred_maps[['raked']] + theme(legend.position="none"), vp = vplayout(9:14, 1:16))
      grid.draw(map.legend)
      # Plot raking factors and violins
      #print(country_rf_plot + theme(legend.position="none"), vp = vplayout(11:17, 1:8))
      #print(beta_plot + theme(legend.position="none"), vp = vplayout(11:17, 9:16))
      # Raking factor maps
      # print(rf_maps[[1]] + theme(legend.position="none"), vp = vplayout(19:23, 1:8))
      # print(rf_maps[[2]] + theme(legend.position="none"), vp = vplayout(19:23, 9:16))
      # print(rf_maps[[3]] + theme(legend.position="none"), vp = vplayout(24:28, 1:8))
      # print(rf_maps[[4]] + theme(legend.position="none"), vp = vplayout(24:28, 9:16))
      print(rf_maps + theme(legend.position="none"), vp = vplayout(15:20, 1:16))
      grid.draw(rf.legend)
      print(country_rf_plot + theme(legend.position="none"), vp = vplayout(21:25, 1:8))
      print(beta_plot + theme(legend.position="none"), vp = vplayout(21:25, 9:16))
      if(exists('cov_gg')) {
        grid.text("Covariate importance", vp = vplayout(26,1:18), gp=gpar(fontsize=20, fontface='bold'))
        print(cov_gg, vp = vplayout(27:32, 1:18))
      }

  ## Plot quilt maps for this region
      quilt.legend <- gLegend(regional_quilt)
      quilt.legend$vp <- viewport(layout.pos.row = 1:15, layout.pos.col = 17:18)
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(28, 18)))
      vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
      # Plot all quilt maps for years that exist
      grid.text("In-Sample", vp = vplayout(1,1:16), gp=gpar(fontsize=20, fontface='bold'))
      print(regional_quilt + theme(legend.position="none"), vp = vplayout(2:16, 1:15))
      grid.draw(quilt.legend)

  ## Plot threshold and cov importance
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(28, 18)))
      vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
      regional_target_plot$vp <- viewport(layout.pos.row = 1:16, layout.pos.col = 1:18)
      grid.draw(regional_target_plot)

  ## Plot DHS admin1 comparison for education
      if(indicator == 'edu_mean') {

        grid.newpage()
        pushViewport(viewport(layout = grid.layout(28, 18)))
        vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
        print(unraked_dhs_plot, vp = vplayout(1:13, 1:16))
        print(raked_dhs_plot, vp = vplayout(15:27, 1:16))

      }

  ## Loop over countries in this region, printing report for each

      ## Use quilt plot list to figure out which countries have data, which do not
      new_quilt_plot_list <- list()
      for(i in 1:length(quilt_plot_list)) {
        this_gaul <- names(quilt_plot_list[i])
        if(!is.null(quilt_plot_list[[i]])) {
          new_quilt_plot_list[[as.character(this_gaul)]] <- quilt_plot_list[[i]]
        }
      }

      ## Make report for each country that has data
      lapply(as.numeric(names(new_quilt_plot_list)), report_each_gaul,
             quilt_plot_list = new_quilt_plot_list,
             target_plot_list = target_plot_list)

      # lapply(133, report_each_gaul,
      #        quilt_plot_list = new_quilt_plot_list,
      #        target_plot_list = target_plot_list)

      dev.off()

  ##################################################################################
  ## Face validity checks
  ##################################################################################
  ##  - Raking factors
  ##  - DHS admin1 comparison
  ##  - Semivariograms
  ##  - Time series, GBD vs. MBG
  ##################################################################################
#     message('VALIDATION STEP 1: Running face validity functions...')
#
#       ## Loop over countries in this region, printing report for each
#         pdf('/share/geospatial/nick_logs/test_report.pdf', width=11, height=18)
#         region_gaul_list <- get_adm0_codes(regions)
#         lapply(region_gaul_list, report_each_gaul)
#         dev.off()
#
#         for(gaul in region_gaul_list) {
#
#       ## Plot 2000, 2005, 2010, 2015 predictions, raked and unraked
#         pred_maps <- extract_raked_and_unraked(gaul,
#                                                results,
#                                                results_raked,
#                                                master_list,
#                                                admin0 = admin0)
#
#       ## Plot raking factors (all + by country)
#         country_rf_plot <- val_raking_factors(gaul_list = gaul,
#                                               rfs = all_rfs)
#
#       ## DHS admin1 validation (all + by country)
#
#       ## (???) Comparison to some threshold (full Bayesian draw-level)
#
#       ## Semivariograms of raw data and predictions at raw data locations (by country)
#         vario_and_violin_plots <- plot_varios_and_violins(indicator = indicator,
#                                                           indicator_group = indicator_group,
#                                                           run_date = run_date,
#                                                           this_gaul = gaul)
#
#       ## GBD and MBG admin0 estimates by year with credible intervals
#         country_time_series_plots <- summarize_admin2(gaul = gaul,
#                                                       indicator = indicator,
#                                                       indicator_group = indicator_group,
#                                                       run_date = run_date,
#                                                       nperiod = total_periods,
#                                                       master_list = master_list)
#
#   ##################################################################################
#   ## Statistical validity
#   ##################################################################################
#   ##  - Covariate relative importance
#   ##  - In-sample and out-of-sample error (quiltplots, tables)
#   ##################################################################################
#     message('VALIDATION STEP 2: Running statistical validity functions...')
#
#       ## Covariate importance
#
#       ## Quiltplots (bias, coverage, RMSE), also summarize into tabulated tables
#
#   ##################################################################################
#   ## Make reports
#   ##################################################################################
#   ##  - All gauls
#   ##  - By country
#   ##################################################################################
#     message('Making final reports for all countries and each country individually...')
#
#         # plot(vario_and_violin_plots[[2]], main="PACF")
#         # grid.echo()
#         # pacf <- grid.grab()
#         # pacf$vp <- viewport(layout.pos.row = 11:14, layout.pos.col = 9:16)
#
#         gLegend<-function(a.gplot){
#           tmp <- ggplot_gtable(ggplot_build(a.gplot))
#           leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#           legend <- tmp$grobs[[leg]]
#           return(legend)
#         }
#         # grab your legends using the predefined functions, then state their grid location
#         map.legend <- gLegend(pred_maps[['raked']][[1]])
#         map.legend$vp <- viewport(layout.pos.row = 1:10, layout.pos.col = 17:18)
#
#         # Initialize plot with master title
#         grid.newpage()
#         pushViewport(viewport(layout = grid.layout(28, 18)))
#         vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
#         # Plot all pred maps
#         grid.text("Unraked", vp = vplayout(1,1:16))
#         print(pred_maps[['unraked']][[1]] + theme(legend.position="none"), vp = vplayout(2:5, 1:4))
#         print(pred_maps[['unraked']][[2]] + theme(legend.position="none"), vp = vplayout(2:5, 5:8))
#         print(pred_maps[['unraked']][[3]] + theme(legend.position="none"), vp = vplayout(2:5, 9:12))
#         print(pred_maps[['unraked']][[4]] + theme(legend.position="none"), vp = vplayout(2:5, 13:16))
#         grid.text("Raked", vp = vplayout(6,1:16))
#         print(pred_maps[['raked']][[1]] + theme(legend.position="none"), vp = vplayout(7:10, 1:4))
#         print(pred_maps[['raked']][[2]] + theme(legend.position="none"), vp = vplayout(7:10, 5:8))
#         print(pred_maps[['raked']][[3]] + theme(legend.position="none"), vp = vplayout(7:10, 9:12))
#         print(pred_maps[['raked']][[4]] + theme(legend.position="none"), vp = vplayout(7:10, 13:16))
#         grid.draw(map.legend)
#         # Plot vario
#         print(vario_and_violin_plots[[1]] + theme(legend.position="none"), vp = vplayout(11:14, 1:16))
#         #grid.draw(pacf)
#         # Plot raking factors and violins
#         print(country_rf_plot + theme(legend.position="none"), vp = vplayout(15:21, 1:8))
#         print(vario_and_violin_plots[[3]] + theme(legend.position="none"), vp = vplayout(15:21, 9:16))
#         # Admin plots
#         print(country_time_series_plots, vp = vplayout(22:28, 1:16))
#
#       }
#
# dev.off()
#
