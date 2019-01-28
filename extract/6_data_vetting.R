# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 06/12/2018
# Purpose: Collapse data for HAP
# source("/homes/jfrostad/_code/lbd/hap/extract/3_collapse.R", echo=T)
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
pacman::p_load(data.table, dplyr, feather, ggridges, readxl, viridis) 
#TODO verify which of these are actually necessary, took from a random image in Ani's wash dir
#/share/geospatial/mbg/wash/s_imp/model_image_history
pkg.list <- c('RMySQL', 'data.table', 'dismo', 'doParallel', 'dplyr', 'foreign', 'gbm', 'ggplot2', 'glmnet', 
              'grid', 'gridExtra', 'gtools', 'magrittr', 'pacman', 'parallel', 'plyr', 'raster', 'rgdal', 'rgeos',
              'seegMBG', 'seegSDM', 'tictoc') #will be loaded by MBG setup

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
cores <- 10
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path('/share/geospatial/mbg/input_data/')
raw.data.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/')
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
#shared functions#
gbd.shared.function.dir <- file.path(j_root,  "temp/central_comp/libraries/current/r")
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source
file.path(gbd.shared.function.dir, 'get_ids.R') %>% source
file.path(gbd.shared.function.dir, 'get_covariate_estimates.R') %>% source

lbd.shared.function.dir <- file.path(h_root, "_code/lbd/lbd_core/mbg_central")
file.path(lbd.shared.function.dir, 'setup.R') %>% source
mbg_setup(repo=lbd.shared.function.dir, package_list=pkg.list) #load mbg functions
#***********************************************************************************************************************

# ---GRAPHS-----------------------------------------------------------------------------------------------------------
#read in the raw data (before collapse and indicator definition)
#compile raw data
compileAndDefine <- function(x, defs) {
  
  dt <- read_feather(x) %>% 
    as.data.table %>% 
    defIndicator(., var.fam='cooking', definitions=defs, debug=F, clean_up=F)
  
}

#read in the raw data
cooking.raw <- file.path(share.dir, 'cooking') %>% 
  list.files(full.names = T, pattern='uncollapsed_p') %>% 
  mclapply(., compileAndDefine, defs=def.file, mc.cores=15) %>% 
  rbindlist

#also read in the collapsed and resampled data (modelling input)


#calculate N
key.cols <- c('cluster_id', 'nid', 'lat', 'long', 'survey_series', 'int_year', 'shapefile', 'location_code')
setkeyv(cooking, key.cols)
cooking[, N := sum(hhweight*hh_size)^2/sum(hhweight^2*hh_size), by=key(cooking)]

#normalize data
normData <- function(x) {
  (x - min(x))/(max(x)-min(x))
}

cooking[, norm_N := lapply(.SD, normData), by=.(ihme_loc_id, nid), .SDcols='N']

#cleanup
cooking <- cooking[!is.na(cooking_fuel_mapped)] #drop if missing fuel type
remove.vars <- names(cooking)[names(cooking) %like% "row_id|ord_|cat_"]
cooking <- cooking[, (remove.vars) := NULL]

#add on location hierarchy info
locs <- get_location_hierarchy(41)
cooking <- merge(cooking, locs[, .(ihme_loc_id, region_name, super_region_name)], by='ihme_loc_id')

#set up order of fuel types
cat.order <- c('none', 'electricity', 'gas', 'kerosene', 'biomass', 'wood', 'crop', 'coal', 'dung', 'other', 'unknown')
cooking[, cooking_fuel := factor(cooking_fuel_mapped, levels=cat.order)]

#set up color scale for fuel types
colors <- c(plasma(9, direction=-1), "#C0C0C0", "#C0C0C0") #use gray for other and unknown
names(colors) <- cat.order

#also bring in the model data and remove NIDs that dont end up making it through the pipeline
mod.dt <- file.path(data.dir, 'cooking_clean.csv') %>% fread
plot.dt <- cooking[nid %in% unique(mod.dt$nid)]
#***********************************************************************************************************************

# ---GRAPHS-----------------------------------------------------------------------------------------------------------
#generate graphs for data vetting
ridgePlot <- function(country) {
  
  message(country)
  
  plot <-
  ggplot(plot.dt[ihme_loc_id==country], aes(x=hh_size, y=cooking_fuel, fill=cooking_fuel)) +
  #ggplot(plot.dt[ihme_loc_id==country], aes(x=hh_size, y=cooking_fuel, fill=0.5 - abs(0.5-..ecdf..))) +
    facet_wrap(int_year~nid) +
    stat_density_ridges(geom = "density_ridges_gradient"#, 
                        #calc_ecdf = TRUE
                        #jittered_points = TRUE, position = "raincloud",
                        #alpha = 0.7, scale = 0.9,
                        ) +
    #scale_fill_viridis(name = "Tail probability", direction = -1)
    scale_fill_manual(values=colors) +
    scale_x_continuous("Household Size", limits=c(0, 10)) + #TODO adjust hh size max according to country?
    ggtitle(country) +
    theme_bw()
  
  print(plot)
  
}

pdf(file=file.path(graph.dir, 'country_ridges.pdf'),
    height=8, width=12)

lapply(unique(cooking$ihme_loc_id), ridgePlot)

dev.off()