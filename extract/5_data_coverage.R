# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 09/11/2018 (never forget)
# Purpose: Exploring data coverage across indicators for hap
# source("/homes/jfrostad/_code/lbd/housing/5_data_coverage.R", echo=T)
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j/"
  h_root <- "/homes/jfrostad/"
  arg <- commandArgs()[-(1:3)] # First args are for unix use only
  
  if (length(arg)==0) {
    # arg <- c("IND", #current project iteration
    #          "8", #output version
    #          1) #number of cores provided to multicore functions
  }
  
  package_lib    <- sprintf('%s_code/_lib/pkg',h_root)
  ## Load libraries and  MBG project functions.
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

## Set core_repo location and indicator group
user            <- Sys.info()['user']
core_repo       <- '/homes/jfrostad/_code/lbd/lbd_core/'
my_repo         <- '/homes/jfrostad/_code/lbd//housing/model'
commondir       <- paste(core_repo, 'mbg_central/share_scripts/common_inputs', sep = '/')
package_list    <- c(t(read.csv(paste(commondir, 'package_list.csv', sep = '/'), header = FALSE)))

#load packages
pacman::p_load(data.table, RMySQL, dplyr, feather, ggplot2, googledrive, gridExtra, maptools, parallel, 
               raster, RColorBrewer, rgdal, rgeos, scales, surviva, viridis) 

#capture date
today <- "2018_09_17" #date of current post-extraction

#options
topic <- "hap"
this.family <- 'cooking'
indicators <- c('cooking_fuel', 'cooking_type', 'cooking_type_chimney', 'cooking_location', 
                'heating_fuel', 'heating_type', 'heating_type_chimney', 'lighting_fuel', 'electricity', 
                'housing_roof', 'housing_wall', 'housing_floor')
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
in.dir <- file.path(j_root, 'LIMITED_USE/LU_GEOSPATIAL/collapse/hap')
###Output###
out.dir  <- file.path(j_root, 'temp/jfrostad/housing/')
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
tabulateIndicators <- function(dt, indicator, byvar) {

  message('tabulating ', indicator)
  
  out <- dt[, sum(!is.na(get(indicator))), by=byvar]
  setnames(out, 'V1', indicator)
  
  if(byvar=='') out[, merge := 1]
  
}

# Load custom functions
source(paste0(my_repo, '/_lib/fx.R'))

#mbg function lib
pkg.list <- c('RMySQL', 'data.table', 'dismo', 'doParallel', 'dplyr', 'foreign', 'gbm', 'ggplot2', 'glmnet', 
              'grid', 'gridExtra', 'gtools', 'magrittr', 'pacman', 'parallel', 'plyr', 'raster', 'rgdal', 'rgeos',
              'seegMBG', 'seegSDM', 'tictoc') #will be loaded by MBG setup
lbd.shared.function.dir <- file.path(h_root, "_code/lbd/lbd_core/mbg_central")
file.path(lbd.shared.function.dir, 'setup.R') %>% source
mbg_setup(repo=lbd.shared.function.dir, package_list=pkg.list) #load mbg functions
#***********************************************************************************************************************

# ---PREP DATA----------------------------------------------------------------------------------------------------------
drive_download(as_id('1EyShhpe-jWS7pry7S3hIT-js4ktdogsDeMTPd903zfg'), overwrite=T)
codebook <- read_xlsx('hap.xlsx', sheet='sheet1') %>% as.data.table
codebook <- codebook[assigned=='qnguyen1' | assigned == 'jfrostad']

table <- lapply(indicators, tabulateIndicators, dt=codebook, byvar='survey_name') %>%  
  Reduce(function(...) merge(..., all = TRUE, by = "survey_name"), .)

all <- lapply(indicators, tabulateIndicators, dt=codebook, byvar='') %>%  
  Reduce(function(...) merge(..., all = TRUE, by = "merge"), .)

all[, merge := NULL]
all[, survey_name := '1_Total']

table <- list(table, all) %>% rbindlist(use.names=T)

#***********************************************************************************************************************

# ---GRAPH CATEGORIES---------------------------------------------------------------------------------------------------
dt <- paste0(in.dir, "/", "data_", this.family, '_', today, ".feather") %>% read_feather %>% as.data.table
dt[, name := get_gaul_codes(tolower(ihme_loc_id)), by = ihme_loc_id]

locs <- get_location_hierarchy(41)
dt <- merge(dt, locs[, .(ihme_loc_id, region_name, super_region_name)], by='ihme_loc_id')

gaul.codes <- unique(dt$ihme_loc_id) %>% tolower %>% get_gaul_codes
year.list <- unique(dt$int_year)

## Load GBD Estimates for this indicator which will be used in raking
gbd <- load_gbd_data2(gbd_type     = "covariate",
                      gbd_name     = 'pollution_indoor_total_prev',
                      gaul_list    = gaul.codes,
                      measure_id   = 5,
                      age_group_id = 1,
                      metric_id    = 3,
                      year_ids     = year.list)

plot.dt <- merge(dt, gbd, by.x=c('name', 'int_year'), by.y=c('name', 'year'))
plot.dt <- plot.dt[order(mean)]
plot.dt[, order := .GRP, by=ihme_loc_id]
plot.dt[, country := paste0(order, "-", ihme_loc_id)]
plot.dt[, year := round_any(int_year, 5)]

#categorical: set up order of severity (best to worst)
cat.order <- c('none', 'electricity', 'gas', 'kerosene', 'biomass', 'wood', 'crop', 'coal', 'dung', 'other', 'unknown')
plot.dt[, category := factor(cat_cooking_fuel_mapped, levels=cat.order)]
plot.dt[, N_cat := sum(N), by=.(ihme_loc_id, year, category)]
plot.dt[, prop := N_cat/sum(N), by=.(ihme_loc_id, year)]

#ord good: set up order of severity (best to worst)
ord.good.order <- c('clean', 'medium', 'dirty')
plot.dt[, ord_good := factor(ord_good_cooking_fuel_mapped, levels=ord.good.order)]
plot.dt[, N_ord_good := sum(N), by=.(ihme_loc_id, year, ord_good)]
plot.dt[, prop_ord_good := N_ord_good/sum(N), by=.(ihme_loc_id, year)]

#ord bad: set up order of severity (best to worst)
ord.bad.order <- c('clean', 'dirty', 'filthy')
plot.dt[ord_bad_cooking_fuel_mapped=='dirty', ord_bad_cooking_fuel_mapped := 'filthy'] #TODO move to mapping
plot.dt[ord_bad_cooking_fuel_mapped=='medium', ord_bad_cooking_fuel_mapped := 'dirty']
plot.dt[, ord_bad := factor(ord_bad_cooking_fuel_mapped, levels=ord.bad.order)]
plot.dt[, N_ord_bad := sum(N), by=.(ihme_loc_id, year, ord_bad)]
plot.dt[, prop_ord_bad := N_ord_bad/sum(N), by=.(ihme_loc_id, year)]

#set up color scale
colors <- c(plasma(9, direction=-1), "#C0C0C0", "#C0C0C0") #use gray for other and unknown
names(colors) <- hap.order

plot <- ggplot(plot.dt[year>=2000], aes(x=reorder(ihme_loc_id,order,sum))) + 
  geom_bar(aes(fill = category), position = position_stack(reverse = TRUE)) +
  coord_flip() +
  facet_wrap(~year) +
  scale_fill_manual(values = colors) +
  ggtitle("HAP data by number of datapoints", subtitle = "Countries sorted by decreasing use of solid fuels (GBD 2017)") +
  theme(legend.position = "top") +
  theme_minimal()

print(plot)

plot <- ggplot(plot.dt[year>=2000], aes(x=reorder(ihme_loc_id,order,sum))) + 
  geom_bar(aes(fill = category, weight=N), position = position_stack(reverse = TRUE)) +
  coord_flip() +
  facet_wrap(~year) +
  scale_fill_manual(values = colors) +
  ggtitle("HAP data by number of individuals surveyed", subtitle = "Countries sorted by decreasing use of solid fuels (GBD 2017)") +
  theme(legend.position = "top") +
  theme_minimal()

print(plot)

propPlot <- function(region) {
  
  message('plotting: ', region)
  
  plot <- ggplot(plot.dt[year>=2000 & super_region_name==region], aes(x=reorder(ihme_loc_id,order,sum), y=prop, fill=category)) + 
    geom_bar(stat = "identity",position="fill") +
    coord_flip() +
    facet_wrap(~year) +
    scale_fill_manual(values = colors) +
    ggtitle(paste0("HAP data by proportion: ", region), subtitle = "Countries sorted by decreasing use of solid fuels (GBD 2017)") +
    theme(legend.position = "top") +
    theme_minimal()
  
  return(plot)
  
}

pdf(file=file.path(out.dir, 'category_proportions.pdf'), onefile=T, width=11, height=8)
lapply(unique(plot.dt$super_region_name), propPlot) 
dev.off()