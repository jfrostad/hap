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
  h_root <- file.path("/ihme/homes", Sys.info()["user"])
  arg <- commandArgs()[-(1:3)] # First args are for unix use only
  
  if (length(arg)==0) {
    # arg <- c("IND", #current project iteration
    #          "8", #output version
    #          1) #number of cores provided to multicore functions
  }
  
  package_lib    <- file.path(h_root, '_code/_lib/pkg')
  ## Load libraries and  MBG project functions.
  .libPaths(c( .libPaths(), package_lib))
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
pacman::p_load(data.table, RMySQL, dplyr, feather, ggmosaic, ggplot2, ggrepel, googledrive, gridExtra, maptools, 
               questionr, parallel, 
               raster, RColorBrewer, rgdal, rgeos, scales, survival, stringr, tm, viridis, wordcloud) 

## Set core_repo location and indicator group
user            <- Sys.info()['user']
core_repo       <- file.path(h_root, '_code/lbd/lbd_core')
my_repo         <- file.path(h_root, '_code/lbd/hap')
commondir       <- file.path(my_repo, 'mbg_central/share_scripts/common_inputs')
package_list    <- file.path(commondir, 'package_list.csv') %>% fread %>% t %>% c

#capture date
today <- "2018_12_18" #date of current post-extraction

#options
cores <- 10
topic <- "hap"
this.family <- 'cooking'
indicators <- c('cooking_fuel', 'cooking_type', 'cooking_type_chimney', 'cooking_location', 
                'heating_fuel', 'heating_type', 'heating_type_chimney', 'lighting_fuel', 'electricity', 
                'housing_roof', 'housing_wall', 'housing_floor')
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
in.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapse/hap/')
temp.dir <- file.path(j_root, 'temp/jfrostad')
share.dir <- file.path('/share/geospatial/jfrostad')
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
  
  return(out)
  
}

# Load custom functions
source(file.path(my_repo, 'cooking/model/_lib/fx.R'))

#mbg function lib
pkg.list <- c('RMySQL', 'data.table', 'dismo', 'doParallel', 'dplyr', 'foreign', 'gbm', 'ggplot2', 'glmnet', 
              'grid', 'gridExtra', 'gtools', 'magrittr', 'pacman', 'parallel', 'plyr', 'raster', 'rgdal', 'rgeos',
              'seegMBG', 'seegSDM', 'tictoc') #will be loaded by MBG setup
lbd.shared.function.dir <- file.path(my_repo, "mbg_central")
file.path(lbd.shared.function.dir, 'setup.R') %>% source
mbg_setup(repo=lbd.shared.function.dir, package_list=pkg.list) #load mbg functions
#***********************************************************************************************************************

# ---PREP DATA----------------------------------------------------------------------------------------------------------
drive_download(as_id('1Nd3m0ezwWxzi6TmEh-XU4xfoMjZLyvzJ7vZF1m8rv0o'), overwrite=T)
codebook <- read_xlsx('hap.xlsx', sheet='sheet1') %>% as.data.table
codebook <- codebook[assigned=='qnguyen1' | assigned == 'jfrostad' | assigned == 'albrja']

##make a table showing the count of each indicator by survey series##
table <- lapply(indicators, tabulateIndicators, dt=codebook, byvar='survey_name') %>%  
  Reduce(function(...) merge(..., all = TRUE, by = "survey_name"), .)

#also add a total row
all <- lapply(indicators, tabulateIndicators, dt=codebook, byvar='') %>%  
  Reduce(function(...) merge(..., all = TRUE, by = "merge"), .)
all[, merge := NULL]
all[, survey_name := '1_Total']

#merge total row with the other survey series rows and return the table
table <- list(table, all) %>% rbindlist(use.names=T)
#***********************************************************************************************************************

# ---GRAPH CATEGORIES---------------------------------------------------------------------------------------------------
dt <- paste0(in.dir, "/", "data_", this.family, '_', today, ".feather") %>% read_feather %>% as.data.table
dt[, name := tolower(ihme_loc_id) %>% get_adm0_codes, by = ihme_loc_id] #add gaul codes

#add on location hierarchy info
locs <- get_location_hierarchy(41)
dt <- merge(dt, locs[, .(ihme_loc_id, region_name, super_region_name)], by='ihme_loc_id')

## Load GBD Estimates for this indicator which will be used in raking
# TODO figure out why the fix_diacretics helper fx is breaking things for me when set to T,
# with this error:  Error in chartr(replace_me, replace_with, x) : 'old' is longer than 'new' 
# for now i have a workaround of just setting that fx to F
gbd <- load_gbd_data(gbd_type     = "covariate",
                      gbd_name     = 'pollution_indoor_total_prev',
                      gaul_list    = unique(dt$name),
                      measure_id   = 5,
                      metric_id    = 3,
                      year_ids     = unique(dt$int_year),
                      collapse_age_sex = T)

plot.dt <- merge(dt, gbd, by.x=c('name', 'int_year'), by.y=c('name', 'year'))
plot.dt <- plot.dt[order(mean)]
plot.dt[, order := .GRP, by=ihme_loc_id]
plot.dt[, country := paste0(order, "-", ihme_loc_id)]
plot.dt[, year := round_any(int_year, 5)]

#categorical: set up order of severity (best to worst)
# cat.order <- c('none', 'electricity', 'gas', 'kerosene', 'biomass', 'wood', 'crop', 'coal', 'dung', 'other', 'unknown')
# plot.dt[, category := factor(cat_cooking_fuel_mapped, levels=cat.order)]
# plot.dt[, N_cat := sum(N), by=.(ihme_loc_id, year, category)]
# plot.dt[, prop := N_cat/sum(N), by=.(ihme_loc_id, year)]

#ord good: set up order of severity (best to worst)
ord.good.order <- c('clean', 'medium', 'dirty')
plot.dt[, ord_good := factor(ord_good_cooking_fuel_mapped, levels=ord.good.order)]
plot.dt[, N_ord_good := sum(N), by=.(ihme_loc_id, year, ord_good)]
plot.dt[, prop_ord_good := N_ord_good/sum(N), by=.(ihme_loc_id, year)]


#set up color scale
colors <- c(plasma(9, direction=-1), "#C0C0C0", "#C0C0C0") #use gray for other and unknown
names(colors) <- cat.order

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

#set up color scale
colors <- plasma(3, direction=-1)
names(colors) <- ord.good.order

propPlot <- function(region) {
  
  message('plotting: ', region)
  
  plot <- ggplot(plot.dt[year>=2000 & super_region_name==region], aes(x=reorder(ihme_loc_id,order,sum), y=prop_ord_good, fill=ord_good)) + 
    geom_bar(stat = "identity",position="fill") +
    coord_flip() +
    facet_wrap(~year) +
    scale_fill_manual(values = colors) +
    ggtitle(paste0("HAP data by proportion: ", region), subtitle = "Countries sorted by decreasing use of solid fuels (GBD 2017)") +
    theme(legend.position = "top") +
    theme_minimal()
  
  return(plot)
  
}

pdf(file=file.path(out.dir, 'ord_good_proportions.pdf'), onefile=T, width=11, height=8)
lapply(unique(plot.dt$super_region_name), propPlot) 
dev.off()

#set up color scale
colors <- plasma(3, direction=-1)
names(colors) <- ord.bad.order

propPlot <- function(region) {
  
  message('plotting: ', region)
  
  plot <- ggplot(plot.dt[year>=2000 & super_region_name==region], aes(x=reorder(ihme_loc_id,order,sum), y=prop_ord_bad, fill=ord_bad)) + 
    geom_bar(stat = "identity",position="fill") +
    coord_flip() +
    facet_wrap(~year) +
    scale_fill_manual(values = colors) +
    ggtitle(paste0("HAP data by proportion: ", region), subtitle = "Countries sorted by decreasing use of solid fuels (GBD 2017)") +
    theme(legend.position = "top") +
    theme_minimal()
  
  return(plot)
  
}

pdf(file=file.path(out.dir, 'ord_bad_proportions.pdf'), onefile=T, width=11, height=8)
lapply(unique(plot.dt$super_region_name), propPlot) 
dev.off()
#***********************************************************************************************************************

# ---GRAPH CATS (MOSAIC)------------------------------------------------------------------------------------------------
#compile raw data
var.fam <- 'cooking'
cooking <- file.path(share.dir, var.fam) %>% 
  list.files(full.names = T, pattern='uncollapsed') %>% 
  lapply(., function(x) as.data.table(read_feather(x))) %>% 
  rbindlist

#merge location info
cooking <- merge(cooking, locs[, .(ihme_loc_id, region_name, super_region_name)], by='ihme_loc_id')

cat.order <- c('none', 'electricity', 'gas', 'kerosene', 'biomass', 'wood', 'crop', 'coal', 'dung', 'other', 'unknown')
cooking[, cooking_fuel := factor(cooking_fuel_mapped, levels=cat.order)]

#set up color scale
colors <- c(plasma(9, direction=-1), "#C0C0C0", "#C0C0C0") #use gray for other and unknown
names(colors) <- cat.order

#cooking_type
pdf(file=file.path(out.dir, 'global_cooking_fuel_v_cooking_type.pdf'), onefile=T, width=11, height=8)
ggplot(data = cooking[cooking_fuel != '']) +
  geom_mosaic(aes(weight = hh_size,
                  x = product(cooking_fuel, cooking_type_mapped), 
                  fill=cooking_fuel), na.rm=T) +
  scale_fill_manual(values = colors) +
  theme_minimal()
dev.off()

#super regions
fuelVsType <- function(region) {
  
  plot <- ggplot(data = cooking[cooking_fuel != '' & super_region_name == region]) +
  geom_mosaic(aes(weight = hh_size,
                  x = product(cooking_fuel, cooking_type_mapped), 
                  fill=cooking_fuel), na.rm=T) +
  scale_fill_manual(values = colors) +
  ggtitle(paste0("Cooking fuel vs cooking type: ", region)) +
  theme_minimal()
  
  return(plot)
  
}

pdf(file=file.path(out.dir, 'regional_cooking_fuel_v_cooking_type.pdf'), onefile=T, width=11, height=8)
mclapply(unique(cooking[super_region_name != 'High-income', super_region_name]), fuelVsType, mc.cores=cores) 
dev.off()

##location
pdf(file=file.path(out.dir, 'global_cooking_fuel_v_cooking_location.pdf'), onefile=T, width=11, height=8)
ggplot(data = cooking[cooking_fuel != '']) +
  geom_mosaic(aes(weight = hh_size,
                  x = product(cooking_fuel, cooking_location_mapped), 
                  fill=cooking_fuel), na.rm=T) +
  scale_fill_manual(values = colors) +
  theme_minimal()
dev.off()

#super regions
fuelVsLoc <- function(region) {
  
  plot <- ggplot(data = cooking[cooking_fuel != '' & super_region_name == region]) +
    geom_mosaic(aes(weight = hh_size,
                    x = product(cooking_fuel, cooking_location_mapped), 
                    fill=cooking_fuel), na.rm=T) +
    scale_fill_manual(values = colors) +
    ggtitle(paste0("Cooking fuel vs cooking type: ", region)) +
    theme_minimal()
  
  return(plot)
  
}

pdf(file=file.path(out.dir, 'regional_cooking_fuel_v_cooking_location.pdf'), onefile=T, width=11, height=8)
mclapply(unique(cooking[super_region_name != 'High-income', super_region_name]), fuelVsLoc, mc.cores=cores) 
dev.off()

##chimney
pdf(file=file.path(out.dir, 'global_cooking_fuel_v_cooking_chimney.pdf'), onefile=T, width=11, height=8)
ggplot(data = cooking[cooking_fuel != '']) +
  geom_mosaic(aes(weight = hh_size,
                  x = product(cooking_fuel, cooking_type_chimney_mapped), 
                  fill=cooking_fuel), na.rm=T) +
  scale_fill_manual(values = colors) +
  theme_minimal()
dev.off()

#super regions
fuelVsChim <- function(region) {
  
  plot <- ggplot(data = cooking[cooking_fuel != '' & super_region_name == region]) +
    geom_mosaic(aes(weight = hh_size,
                    x = product(cooking_fuel, cooking_type_chimney_mapped), 
                    fill=cooking_fuel), na.rm=T) +
    scale_fill_manual(values = colors) +
    ggtitle(paste0("Cooking fuel vs cooking type: ", region)) +
    theme_minimal()
  
  return(plot)
  
}

pdf(file=file.path(out.dir, 'regional_cooking_fuel_v_cooking_chimney.pdf'), onefile=T, width=11, height=8)
mclapply(unique(cooking[super_region_name != 'High-income', super_region_name]), fuelVsChim, mc.cores=cores) 
dev.off()
#***********************************************************************************************************************

# ---GRAPH CATS (HOUSING)-----------------------------------------------------------------------------------------------
var.fam <- 'housing'
housing <- file.path(share.dir, var.fam) %>% 
  list.files(full.names = T, pattern='uncollapsed') %>% 
  lapply(., function(x) as.data.table(readRDS(x))) %>% 
  rbindlist

#create row ID for vectorized functions
housing[, index := .I]
setkey(housing, index)

#merge location info
housing <- merge(housing, locs[, .(ihme_loc_id, region_name, super_region_name)], by='ihme_loc_id')

#extra the first digit of floor number as an ordinal indicator
housing[, floor_rank := substr(as.character(housing_floor_num), 0, 1)]

##data prep
#clean the strings a bit
housing[, floor := tolower(housing_floor)]
housing[, floor := str_replace_all(floor, '[.]', '')] #remove common errors
housing[, floor := str_replace_all(floor, '<ff>', '')] #remove common errors
housing[, floor := str_replace_all(floor, '<fb>', '')] #remove common errors
housing[, floor := str_replace_all(floor, '<e0>', '')] #remove common errors
housing[, floor := str_replace_all(floor, '<e1>', '')] #remove common errors
housing[, floor := str_replace_all(floor, '<e3>', '')] #remove common errors
housing[, floor := str_replace_all(floor, '<e9>', '')] #remove common errors
housing[, floor := str_replace_all(floor, '<a4>', '')] #remove common errors
housing[, floor := str_replace_all(floor, '<f1>', '')] #remove common errors
housing[, floor := str_replace_all(floor, ' /', ' ')] #remove common errors
housing[, floor := str_replace_all(floor, '/ ', ' ')] #remove common errors
housing[, floor := str_replace_all(floor, '/', ' ')] #remove common errors
housing[, floor := str_replace_all(floor, ' - ', ' ')] #remove common errors
housing[, floor := str_replace_all(floor, ': ', ' ')] #remove common errors
housing[, floor := str_replace_all(floor, '["]', '')] #remove common errors
housing[, floor := str_replace_all(floor, ',', ' ')] #remove common errors

#tabulate the amount of each floor type using hh_size
setkey(housing, ihme_loc_id, floor)
housing[, floor_N := sum(hh_size, na.rm=T), by=key(housing)]
plot.dt <- unique(housing, by=key(housing))
plot.dt <- plot.dt[floor_N > 350] # keep only the more popular types

#plot word cloud by region
wordCloudRegion <- function(region) {
  
  message(region)
  
ggplot(data=plot.dt[region_name %like% region & floor_rank <4]) +
  aes(x = 1, y = 1, size = floor_N, label = floor,
      color= floor_rank) +
  geom_text_repel(segment.size = 0, force = 100) +
  scale_size(range = c(2, 10), guide = FALSE) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  labs(x = '', y = '') +
  facet_grid(~ihme_loc_id) +
  theme_minimal() +
  theme(strip.text = element_text(
    color="black", size=16, lineheight=5.0),
    plot.title = element_text(colour = "black",
                              size = 18,
                              hjust = 0.5, vjust = 0.8, angle = 0)) +
  ggtitle(paste0("Floor Type,", region))
  
}

pdf(file=file.path(out.dir, 'floor_word_cloud.pdf'), onefile=T, width=11, height=8)
lapply(unique(plot.dt[, region_name]), wordCloudRegion) 
dev.off()

cat.order <- c('0', '1', '2', '3', '9')
plot.dt[, floor_rank := factor(floor_rank, levels=cat.order)]

#set up color scale
colors <- c(plasma(4, direction=1), "#C0C0C0") #use gray for other and unknown
names(colors) <- cat.order

#plot word cloud by iso3
wordCloudRegion <- function(region) {
  
  message(region)
  
  ggplot(data=plot.dt[ihme_loc_id %like% region & floor_rank %in% cat.order]) +
    aes(x = 1, y = 1, size = floor_N, label = floor,
        color= floor_rank) +
    geom_text_repel(segment.size = 0, force = 100) +
    scale_size(range = c(2, 10), guide = FALSE) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    scale_color_manual(values = colors) +
    labs(x = '', y = '') +
    facet_grid(~ihme_loc_id) +
    theme_minimal() +
    theme(strip.text = element_text(
      color="black", size=16, lineheight=5.0),
      plot.title = element_text(colour = "black",
                                size = 18,
                                hjust = 0.5, vjust = 0.8, angle = 0)) +
    ggtitle(paste0("Floor Type,", region))
  
}

pdf(file=file.path(out.dir, 'floor_word_cloud_iso3.pdf'), onefile=T, width=11, height=8)
lapply(unique(plot.dt[!is.na(floor), ihme_loc_id]), wordCloudRegion) 
dev.off()


##SCRAP
plot.dt <- housing[!is.na(hh_size)]

wordcloud(words = plot.dt$housing_floor, freq = plot.dt$hh_size, min.freq = 1, #scale = c(2, 0.2),
          max.words=200, random.order=FALSE, rot.per=0.1, 
          ordered.colors=TRUE,
          colors=brewer.pal(8, "Dark2")[factor(plot.dt$floor_rank)])

ggplot(data = housing[survey_series %like% 'MACRO' & floor_rank <= 3]) +
  geom_mosaic(aes(weight = hh_size,
                  x = product(housing_floor, floor_rank), 
                  fill=floor_rank), na.rm=T)
