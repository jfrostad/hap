indicator       = as.character(commandArgs()[4])
indicator_proper_name = as.character(commandArgs()[5])
indicator_group = as.character(commandArgs()[6])
run_date        = as.character(commandArgs()[7])
pop_measure     = as.character(commandArgs()[8])
repo            = as.character(commandArgs()[9])
region         = as.character(commandArgs()[10])
nperiods        = as.numeric(commandArgs()[11])
shapefile_version = as.character(commandArgs()[12])
print(commandArgs())

# indicator = 'stunting_mod_b'
# indicator_proper_name = 'Childhood Stunting'
# indicator_group = 'child_growth_failure'
# run_date = '2017_06_29_01_24_32'
# pop_measure = 'a0004t'
# repo = '/ihme/code/geospatial/dccasey/mbg'
# region = 'wssa'
# nperiods = 16


indicator_proper_name = gsub("###", " ", indicator_proper_name,fixed = T)

message(paste('indicator =', indicator, 'indicator_proper_name =', indicator_proper_name,
              'indicator group =', indicator_group, 'run_date =',run_date, 'pop_measure =', pop_measure,
              'repo =', repo, 'region =', region, 'nperiods =', nperiods))



## Define main directories.
results_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date, '/')
val_dir <- paste0(results_dir, 'validation_report')
dir.create(val_dir, showWarnings = FALSE)

## Load libraries and miscellaneous MBG project functions.
root <- "/home/j/"
package_lib <- ifelse(grepl("geos", Sys.info()[4]),
                      paste0(root,'temp/geospatial/geos_packages'),
                      paste0(root,'temp/geospatial/packages'))

.libPaths(package_lib)                                  # Ensures packages look for dependencies here when called with library().
#    Necessary for seeg libraries.

source(paste0(repo, '/education/pop_splitting_functions.R'))

mbgc_funks = c('mbg_functions.R', 'prep_functions.R', 'covariate_functions.R', 'misc_functions.R', 'post_estimation_functions.R', 'gbd_functions.R', 'holdout_functions.R',
               'seegMBG_transform_functions.R','validation_report_functions.R','validation_reports_pub_functions.R')
for(fff in paste0(repo,'/mbg_central/',mbgc_funks)) source(fff)




package_list <- c('fields', 'gridGraphics' ,'grid', 'gridExtra', 'gstat', 'magrittr', 'ggplot2', 'doParallel', 'SDMTools', 'foreign', 'rgeos', 'data.table','raster','rgdal','INLA','seegSDM','seegMBG','plyr','dplyr','leaflet')
for(package in package_list) {
  suppressWarnings(suppressMessages(library(package, lib.loc = package_lib, character.only=TRUE)))
}

#all_gauls       = get_adm0_codes('africa')
year_list       = c(2000:2015)


#convert regions to full names
if(region == 'essa') region_name = "Eastern Sub-Saharan Africa"
if(region == 'wssa') region_name = "Western Sub-Saharan Africa"
if(region == 'name') region_name = "North Africa and Middle East"
if(region == 'sssa') region_name = "Southern Sub-Saharan Africa"
if(region == 'cssa') region_name = 'Central Sub-Saharan Africa'

## Define path to csv of all raking factors, load in data.table.
in_dir  <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date)
default_rf_path <- paste0(in_dir, '/', indicator, '_rf.csv')
all_rfs <- fread(default_rf_path)

#load polygons and what not
simple_polygon_list <- load_simple_polygon(gaul_list = get_adm0_codes(region,
                                                                      shapefile_version = shapefile_version),
                                           buffer = 0.4,
                                           subset_only = FALSE,
                                           shapefile_version = shapefile_version)
subset_shape   <- simple_polygon_list[[1]]
simple_polygon <- simple_polygon_list[[2]]


## Make quilt plots for all countries before we initialize pdf, because it outputs a lot of intermediate plots along the way.
df <- load_region_input_data(indicator_group = indicator_group,
                             indicator = indicator,
                             run_date = run_date,
                             reg = region)

df <- df[year >= 1998 & year <= 2002, year := 2000]
df <- df[year >= 2003 & year <= 2007, year := 2005]
df <- df[year >= 2008 & year <= 2012, year := 2010]
df <- df[year >= 2013 & year <= 2017, year := 2015]

all_data <- is_oos_preds(rd = run_date,
                         all.data = df,
                         cell_draws_filename = '%s_cell_draws_eb_bin%i_%s_%i.RData', ## in sprintf notation
                         holdouts = 0, ## number of holdouts. if zero only does in sample
                         reg = region,
                         years = 2000:2015,
                         indic = indicator,
                         indic_group = indicator_group,
                         holdoutlist = NULL)


## Make error variables to map/tabulate (absolute error, RMSE, coverage, etc.)
if(indicator != 'edu_mean') all_data <- all_data[, IS_abs_error := IS - (get(indicator)/N)]
if(indicator == 'edu_mean') all_data <- all_data[, IS_abs_error := IS - get(indicator)]


regional_quilt <- plot_quilt(gaul_list = get_adm0_codes(region,
                                                        shapefile_version = shapefile_version),
                             df = all_data,
                             sample = "IS_abs_error",
                             subset_shape = subset_shape)

#regional quilt plot
rq = regional_quilt + ggtitle(paste('Residual Error:', region_name))


#raking factors plot
gaul_list = get_adm0_codes(region, shapefile_version = shapefile_version)
rfs <- all_rfs[name %in% gaul_list, ]
loc_names <- setDT(read.csv("/snfs1/WORK/11_geospatial/10_mbg/gaul_to_loc_id.csv",stringsAsFactors = F))
setnames(rfs, "name", "GAUL_CODE")
rfs <- merge(rfs, loc_names, by="GAUL_CODE")
rfs[, Year:= as.factor(year)]
max_val = max(max(rfs[,.(rake_to_mean, geo_mean)],na.rm=T),na.rm= T)
gg_rfs <- ggplot(data = rfs, aes(x = rake_to_mean, y = geo_mean, color = Year)) +
  geom_point() + ylab("MBG Mean") + xlab("GBD Mean") +theme_bw() + xlim(0, max_val) +
  ylim(0, max_val) + geom_abline(slope = 1) +
  ggtitle(paste0('Comparison of Results in ', region_name, '\n Global Burden of Disease vs. Model Based Geostatistics'))


#make violin plots by country int he region
violin_plots <- lapply(gaul_list, function(the_gaul) plot_violins(indicator = indicator, indicator_proper_name = indicator_proper_name, indicator_group = indicator_group,
                                                     run_date = run_date, this_gaul = the_gaul))

#unlist them and arrange them
#i hate doing this in a for loop, but I couldn't figure out a better way
vps = list()
counter = 1
for(a in 1:length(violin_plots)){
  for(b in 1:length(violin_plots[[a]])){
    if(!is.null(violin_plots[[a]][[b]]) & inherits(violin_plots[[a]][[b]], 'gg')){
      vps[[counter]] = violin_plots[[a]][[b]]
      counter = counter + 1
    }
  }
}

vps_grid = marrangeGrob(grobs = vps, ncol = 3, nrow = 3, top = "")

#load the data for the time trend graphs

# #Pop goes the balloon
# pop_raster_annual <- suppressMessages(suppressWarnings(load_and_crop_covariates_annual(covs = 'worldpop',
#                                                                                        measures = pop_measure,
#                                                                                        simple_polygon = simple_polygon,
#                                                                                        start_year  = min(year_list),
#                                                                                        end_year    = max(year_list),
#                                                                                        interval_mo = 12,
#                                                                                        agebin      = 1)))
#
# #admin 2 stuff
# admin_level <- 2
# shapes <- shapefile(paste0("/snfs1/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin", admin_level, "/g2015_2014_", admin_level, "/g2015_2014_", admin_level, "_modified.shp"))
#
# #dem draws
# master_list <- lapply(region, function(reg) pull_country_draws(reg= reg, periods =nperiods, raked = "", in_dir = in_dir, pop_measure = pop_measure, start_year = 2000,
#                                           end_year = 2015, admin2_shapes = shapes, all_region_pops = pop_raster_annual[[1]]))
#
# master_list <- do.call(c, unlist(master_list, recursive=FALSE))
#
#
# #mkae the GBD trend graphs
# country_time_series <- lapply(gaul_list, function(gaul) summarize_admin2(gaul = gaul,
#                                               indicator = indicator,
#                                               indicator_group = indicator_group,
#                                               run_date = run_date,
#                                               nperiod = nperiods,
#                                               master_list = master_list))
# #Add a title
# country_time_series = lapply(1:length(gaul_list), function(x) country_time_series_plots[[x]] +
#                                ggtitle(paste('MBG vs. GBD Results:', loc_names[GAUL_CODE == gaul_list[x], loc_nm_sh])))
#
# #fix ylabs
# country_time_series = lapply(1:length(gaul_list), function(x) country_time_series[[x]] +
#                                ylab(paste('Mean:', indicator_proper_name)))
#
#
# #arrange the time series
# cts_grid = marrangeGrob(grobs = country_time_series, ncol = 1, nrow = 3, top = "")

#save the plots
graph_dir = paste0(in_dir, '/','valrep_graphs/')
dir.create(graph_dir, recursive = T)


#save to a pdf
pdf(file = paste0(graph_dir,'/', 'validation_report_', region, '.pdf'), width = 7, height = 10)

plot(rq)
plot(gg_rfs)

for(i in vps_grid){
  plot(i)
}
#lapply(cts_grid, plot)

dev.off()





