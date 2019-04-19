# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 06/12/2018
# Purpose: Collapse data for HAP
# source("/homes/jfrostad/_code/lbd/hap/diagnostics/6_data_vetting.R", echo=T)
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j/"
  h_root <- file.path("/ihme/homes", Sys.info()["user"])
  arg <- commandArgs()[-(1:3)] # First args are for unix use only
  
  if (length(arg)==0) {
    # arg <- c("IND", #current project iteration
    #          "8", #output version
    #          1) #number of cores provided to multicore functions
  }
  
  package_lib    <- file.path(h_root, '_code/_lib/pkg')
  ## Load libraries and  MBG project functions.
  #.libPaths(c( .libPaths(), package_lib))
  .libPaths(package_lib)
  
  # necessary to set this option in order to read in a non-english character shapefile on a linux system (cluster)
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
} else {
  j_root <- "J:"
  h_root <- "H:"
  # arg <- c("IND", #current project iteration
  #          "4", #output version
  #          1) #number of cores provided to multicore functions
}

#load packages
pacman::p_load(data.table, dplyr, feather, ggrepel, ggridges, readxl, stringr, viridis) 
#TODO verify which of these are actually necessary, took from a random image in Ani's wash dir
#/share/geospatial/mbg/wash/s_imp/model_image_history
pkg.list <- c('RMySQL', 'data.table', 'dismo', 'doParallel', 'dplyr', 'foreign', 'gbm', 'glmnet', 
              'grid', 'gridExtra', 'gtools', 'magrittr', 'pacman', 'parallel', 'plyr', 'raster', 'rgdal', 'rgeos',
              'seegMBG', 'seegSDM', 'tictoc') #will be loaded by MBG setup

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
cores <- 10
new.gbd.results <- F #set T if GBD results have been updated and need to redownload
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path('/share/geospatial/mbg/input_data/')
raw.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/hap/batch')
geomatched.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/')
census.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/census')
doc.dir <- file.path(j_root, 'WORK/11_geospatial/hap/documentation')
def.file <- file.path(doc.dir, 'definitions.xlsx')
collapse.dir  <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapse/hap/')
model.dir  <- file.path(j_root, 'WORK/11_geospatial/10_mbg/input_data/hap')
share.dir <- file.path('/share/geospatial/jfrostad')

###Output###
graph.dir <- file.path(j_root, 'WORK/11_geospatial/hap/graphs')
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#general functions#
central.function.dir <- file.path(h_root, "_code/_lib/functions")
# this pulls the general misc helper functions
file.path(central.function.dir, "misc.R") %>% source
#hap functions#
hap.function.dir <- file.path(h_root, '_code/lbd/hap/extract/functions')
#this pulls hap collapse helper functions
file.path(hap.function.dir, '/collapse_fx.R') %>% source

##shared functions##
#gbd#
gbd.shared.function.dir <- file.path(j_root,  "temp/central_comp/libraries/v69/r")
file.path(gbd.shared.function.dir, 'get_covariate_estimates.R') %>% source
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source
file.path(gbd.shared.function.dir, 'get_ids.R') %>% source
file.path(gbd.shared.function.dir, 'get_population.R') %>% source
file.path(gbd.shared.function.dir, 'get_outputs.R') %>% source

#lbd#
lbd.shared.function.dir <- file.path(h_root, "_code/lbd/lbd_core/mbg_central")
file.path(lbd.shared.function.dir, 'setup.R') %>% source
mbg_setup(repo=lbd.shared.function.dir, package_list=pkg.list) #load mbg functions

#normalize data
normData <- function(x) {
  (x - min(x))/(max(x)-min(x))
}

#find values %like% helper
getLikeMe <- function(obj, val, invert=F) {
  
  message('finding values that ', ifelse(invert, 'are NOT', 'are') ,' like: ', val, '\n|', 
          '\no~~> from: ', deparse(match.call()$obj))
  
  #add option to invert
  if (invert) { 
    
    out <- names(obj)[!(names(obj) %like% val)]
  
  } else out <- names(obj)[names(obj) %like% val]
  
  return(out)
  
}

#graph helper functions that loop over countries
fueltypeRidges <- function(country, verbose=T) {
  
  message(country)
  
  plot <-
    ggplot(fuel.dt[reg_iso3==country], aes(x=log_count, y=fuel_type, fill=fuel_type)) +
    facet_wrap(~facet) +
    stat_density_ridges(geom = "density_ridges_gradient", scale=4) +
    scale_fill_manual(values=colors) +
    scale_x_continuous("Log(count)", limits=c(-1, 10)) +
    ggtitle(country) +
    theme_bw()
  
  print(plot)
  
  return(NA) #no need to return anything
  
}

#generate also ridgeplots of fueltype use over time
fueltypeTimeRidges <- function(country, verbose=T) {
  
  #subset to country
  if(verbose) message(country)
  plot.dt <- fuel.dt[reg_iso3==country]
  
  #calculate totals to add as a facet
  totals <- copy(cooking[reg_iso3==country]) %>% setnames(., 'N', 'count')
  totals[, fuel_type := 'total']
  totals[, log_count := log(count)]
  totals[count==0, log_count := NA]
  
  #rbind the tables
  plot.dt <- list(plot.dt, totals) %>% rbindlist(., use.names = T, fill=T)
  
  plot <-
    ggplot(plot.dt, 
           aes(x=log_count, y=as.factor(paste(int_year, nid, sep='_')), fill=0.5 - abs(0.5-..ecdf..))) +
    facet_wrap(~fuel_type) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                        alpha = 0.7, scale = 3) +
    scale_fill_viridis(name = "Tail probability", direction = -1) +
    scale_x_continuous("Log(count)") +
    ggtitle(country) +
    theme_bw()
  
  print(plot)
  
  return(NA) #no need to return anything
  
}

#generate also ridgeplots of solid fuel use over time
solidRidges <- function(country, verbose=T) {

  if(verbose) message(country)
  
  plot <-
    ggplot(solid.dt[reg_iso3==country], 
           aes(x=cooking_fuel_solid/N, y=as.factor(int_year), fill=0.5 - abs(0.5-..ecdf..))) +
    facet_wrap(~survey_series) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                        jittered_points = TRUE, position = "raincloud",
                        alpha = 0.7, scale = 0.8, point_alpha=0.5, aes(point_size=N)) +
    scale_fill_viridis(name = "Tail probability", direction = -1) +
    scale_size_area(max_size=3) +
    scale_x_continuous("% Using Solid Fuel", limits=c(-0.1, 1.1)) + #TODO adjust hh size max according to country?
    ggtitle(country) +
    theme_bw()
  
  print(plot)
  
  return(NA) #no need to return anything
  
}

#generate scatterplots comparing solid fuel to gbd ad0 values
#TODO we should try to generate this at ad1 as well
diffScatters <- function(country, verbose=T) {

  if(verbose) message(country)
  
  plot <-
    ggplot(hap.comparison.ad0[reg_iso3==country & !is.na(solid_natl)], 
           aes(x=solid_natl, y=gbd_mean, 
               color=int_year, size=N_natl, shape=survey_series, label=nid_label)) +
    geom_label_repel(size=3) +
    geom_point() +
    geom_abline(slope = 1, linetype='dashed') +
    scale_color_viridis() +
    scale_x_continuous("% Using Solid Fuel (LBD)", limits=c(-0.1, 1.1)) +
    scale_y_continuous("% Using Solid Fuel (GBD 2017)", limits=c(-0.1, 1.1)) +
    ggtitle(country) +
    theme_bw()
  
  print(plot)
  
  return(NA) #no need to return anything
  
}

#generate lineplots comparing solid fuel to gbd ad0 results
lineplotViolins <- function(country, verbose=T) {
  
  if(verbose) message(country)
  
  plot <-
    ggplot(hap.comparison[reg_iso3==country], aes(x=int_year)) +
    geom_ribbon(aes(ymin=gbd_lower, ymax=gbd_upper), fill='chartreuse', alpha=.3) +
    geom_line(aes(y = gbd_mean),  color='chartreuse') +
    geom_violin(aes(y=solid_pct, group=int_year %>% as.factor, color=survey_series, weight=N), trim=T) +
    geom_jitter(aes(y=solid_pct, size=N, color=survey_series), shape=16, position=position_jitter(0.2), alpha=.2) +
    scale_x_continuous("Interview Year") +
    scale_y_continuous("% Using Solid Fuel (GBD 2017)", limits=c(-0.1, 1.1)) +
    ggtitle(country) +
    theme_bw()
  
  print(plot)
  
  return(NA) #no need to return anything
  
}

#generate full suite of plots as a single PDF
vetSuite <- function(country){
  
  message(country, '->scatters')
  diffScatters(country, verbose = F)
  
  message(country, '->violins')
  lineplotViolins(country, verbose = F)
  
  message(country, '->ridges')
  solidRidges(country, verbose = F)
  fueltypeTimeRidges(country)
  fueltypeRidges(country, verbose = F)
  
}
#***********************************************************************************************************************

# ---DATA PREP----------------------------------------------------------------------------------------------------------
#read in the GBD estimates/location info
locs.meta = get_location_metadata(location_set_id = 9, gbd_round_id = 5)
locs <- get_location_hierarchy(41)

#pull hap exposure from gbd2017 - command provided by kate causey
if(new.gbd.results==T){
  gbd.shared.function.archive <- file.path(j_root,  "temp/central_comp/libraries/2017_archive/r")
  file.path(gbd.shared.function.archive, 'get_draws.R') %>% source
  hap.exp <- get_draws(gbd_id_type = "rei_id",
                       gbd_id=87,
                       source="exposure",
                       year_id=c(2000:2017),
                       location_id=locs[,location_id],
                       age_group_id=2,
                       sex_id=2,
                       gbd_round_id=5)
  hap.exp <- hap.exp[parameter=='cat1'] #cat2 is just the inverse
  
  #produce the mean and CI
  draw.cols <- getLikeMe(hap.exp, 'draw')
  hap.exp <- hap.exp[, gbd_lower := apply(.SD, 1, quantile, probs=.025), .SDcols=draw.cols]
  hap.exp <- hap.exp[, gbd_mean := rowMeans(.SD), .SDcols=draw.cols]
  hap.exp <- hap.exp[, gbd_upper := apply(.SD, 1, quantile, probs=.975), .SDcols=draw.cols]
  hap.exp[, (draw.cols) := NULL] #no longer need
  
  #add location hierarchy to prep for merge to geospatial data
  hap.exp <- merge(hap.exp, 
                   locs[, .(location_id, ihme_loc_id, region_name, region_id, super_region_name)], 
                   by='location_id')
  hap.exp[, reg_iso3 := paste0(region_id, ': ', ihme_loc_id)]
  
  write.csv(hap.exp, file = file.path(graph.dir, 'gbd_hap_results.csv'))

} else hap.exp <- file.path(graph.dir, 'gbd_hap_results.csv') %>% fread

#read in the raw data (before collapse and indicator definition)
#compile raw data
# compileAndDefine <- function(x, defs) {
#   
#   message(x)
#   
#   dt <- fread(x) %>% 
#     .[, row_id := .I] %>% 
#     setkey(., row_id) #%>%
#     #defIndicator(., var.fam='cooking', definitions=defs, debug=F, clean_up=F)
#   
# }
# 
# #read in the raw data
# cooking.raw <- file.path(raw.dir) %>% 
#   list.files(full.names = T, pattern='.csv') %>% 
#   mclapply(., compileAndDefine, defs=def.file, mc.cores=10) %>% 
#   rbindlist
# 
# #cleanup
# cooking.raw <- cooking.raw[!is.na(cooking_fuel_mapped)] #drop if missing fuel type
# remove.vars <- names(cooking.raw)[names(cooking.raw) %like% "row_id|ord_|cat_"]
# cooking.raw <- cooking.raw[, (remove.vars) := NULL]
# 
# #add on location hierarchy info
# cooking.raw <- merge(cooking.raw, locs[, .(ihme_loc_id, region_name, super_region_name)], by='ihme_loc_id')
# 
# #set up order of fuel types
cat.order <- c('none', 'electricity', 'gas', 'kerosene', 'wood', 'crop', 'coal', 'dung', 'other', 'unknown')
# cooking.raw[, cooking_fuel := factor(cooking_fuel_mapped, levels=cat.order)]

#set up color scale for fuel types
colors <- c(plasma(8, direction=-1), "#C0C0C0", "#C0C0C0") #use gray for other and unknown
names(colors) <- cat.order

#also read in the latest version of collapsed and resampled data (modelling input)
cooking <- list.files(model.dir, full.names = T)[list.files(model.dir) %>% length %>% max] %>% 
  read_feather %>% 
  as.data.table

#merge on the loc info
cooking <- merge(cooking, locs[, .(ihme_loc_id, region_name, super_region_name, region_id)], by='ihme_loc_id')

#build a better facet title
cooking[, facet := paste0(int_year, ': ', survey_series, '[', nid, ']')]

#also build a region-iso3 var in order to sort plots better
cooking[, reg_iso3 := paste0(region_id, ': ', ihme_loc_id)]
cooking <- cooking[order(-rank(reg_iso3))] #sort by this var

#RIDGEPLOT DATA PREP#
#reshape types of cooking fuel long
#first isolate the different vars for cooking fuel
fuel.types <- names(cooking)[names(cooking) %like% 'fuel']
fuel.types <- fuel.types[!(fuel.types %like% 'solid')] #remove the aggregate
#TODO remove once youve rextracted all
fuel.types <- fuel.types[!(fuel.types %like% 'biomass')] #for now remove biomass 

#remove any other indicators from the dt
null.vars <- names(cooking)[names(cooking) %like% 'cooking_']

#reshape for fuel type
fuel.dt <- melt(cooking[, -(null.vars[!(null.vars %in% fuel.types)]), with=F], 
                measure.vars = list(fuel=fuel.types),
                variable.name = "fuel_type",
                value.name = "count")

#generate a logged count variable in order to normalize for plots
fuel.dt[, log_count := log(count)]
fuel.dt[count==0, log_count := NA]

#set up order of fuel types
fuel.dt[, fuel_type := str_remove_all(fuel_type, 'cooking_fuel_')]
fuel.dt[, fuel_type := factor(fuel_type, levels=cat.order)]

##SOLID FUEL COMPARISONS#
#subset to just solid fuel
nonsolid.vars <- names(cooking)[names(cooking) %like% 'cooking'][-13]
solid.dt <- cooking[, -c(nonsolid.vars), with=F]

#merge on the gbd values for comparison
hap.exp[, int_year := year_id]
hap.comparison <- merge(hap.exp[ihme_loc_id %in% unique(solid.dt$ihme_loc_id),
                                .(gbd_lower, gbd_mean, gbd_upper, ihme_loc_id, int_year, reg_iso3)], 
                        solid.dt, all=TRUE,
                        by=c('ihme_loc_id', 'int_year', 'reg_iso3')) %>% 
  setkeyv(., c('ihme_loc_id', 'int_year', 'reg_iso3'))

#generate values for comparison
hap.comparison[, solid_pct := cooking_fuel_solid/N]
#TODO should be doing this comparison at ad1 if possible
hap.comparison[, solid_natl := weighted.mean(x=solid_pct, w=N), by=key(hap.comparison)]
hap.comparison[, N_natl := sum(N), by=key(hap.comparison)]
hap.comparison[, diff_ratio := solid_natl/gbd_mean]
hap.comparison[, diff_abs := abs(solid_natl-gbd_mean)]
hap.comparison[diff_abs>.15, nid_label := nid] #label any points that are 15% apart

#can set to unique to speed up plotting of scatters
hap.comparison.ad0 <- unique(hap.comparison, by=key(hap.comparison))
#output comparison stats for data vetting sheet
hap.comparison.ad0[!is.na(solid_natl), .(nid, survey_series, ihme_loc_id, int_year, gbd_lower, gbd_mean, gbd_upper,
                       solid_natl, N_natl, diff_ratio, diff_abs)] %>% 
  write.csv(., file = file.path(graph.dir, 'gbd_solid_fuel_comparison.csv'))
#***********************************************************************************************************************

# ---GRAPHS-------------------------------------------------------------------------------------------------------------
#generate graphs for data vetting
#first generate each graph type as a separate pdf series

#country ridges over fueltype
pdf(file=file.path(graph.dir, 'country_ridges_fueltype.pdf'),
    height=8, width=12)

lapply(unique(cooking$reg_iso3), fueltypeRidges)

dev.off()

#country ridges over fueltype vs time
pdf(file=file.path(graph.dir, 'country_ridges_fueltype_v_time.pdf'),
    height=8, width=12)

lapply(unique(cooking$reg_iso3), fueltypeTimeRidges)

dev.off()

#country ridges of solid fuel vs time
pdf(file=file.path(graph.dir, 'country_ridges_solid.pdf'),
    height=8, width=12)

lapply(unique(cooking$reg_iso3), solidRidges)

dev.off()

#scatterplots to compare with gbd values at ad0
pdf(file=file.path(graph.dir, 'gbd_comparison_scatters.pdf'),
    height=8, width=12)

lapply(unique(cooking$reg_iso3), diffScatters)

dev.off()

#violinplots to compare distributions of solid fuel over time against GBD estimate
pdf(file=file.path(graph.dir, 'gbd_comparison_lineplots.pdf'),
    height=8, width=12)

lapply(unique(cooking$reg_iso3), lineplotViolins)

dev.off()

#now generate all the plots as a single PDF to aide in overall vetting
pdf(file=file.path(graph.dir, 'vetting_suite.pdf'),
    height=8, width=12)

lapply(unique(cooking$reg_iso3), vetSuite)

dev.off()