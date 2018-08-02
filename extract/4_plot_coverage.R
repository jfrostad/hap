########################################################################
###### Short loop to generate data coverage plots for all regions ######
######                Scott Swartz | 8/29/2017                    ######
########################################################################

#source('/homes/jfrostad/_code/risks/air_hap/extraction/hap_plot.R')

# ---CONFIG----------------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

# disable scientific notation
options(scipen = 999)

library(RMySQL)
library(rgeos)
library(feather)
library(data.table)
library(ggplot2)
library(doParallel)
library(gridExtra)
library(stringr)
library(RColorBrewer)
library(rgdal)
library(raster)
library(magrittr)
library(dplyr)
library(tidyr)

#setup
sing_image = TRUE #whether running on RStudio singularity image
numcores = 3
root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
j_root <- root
repo <- '/share/code/geospatial/kwilson7/lbd_core/'
setwd(repo)
#source('mbg_central/polygon_functions.R')
#source('mbg_central/shapefile_functions.R')

indicator <- 'hap' #water or sani
var <- 'cooking_fuel' #imp, unimp, surface, od, piped

title <- "Cooking Fuel"

polydat <-  read_feather('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_water_unconditional__2018_05_12.feather')
ptdat <- read_feather('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/ptdat_water_unconditional__2018_05_11.feather')
w_collapsed <- rbind(polydat, ptdat)

#import dataset (written during collapse code)
coverage_data <- as.data.table(w_collapsed)
coverage_data <- coverage_data[!(shapefile == 'mombasa' & is.na(lat)),]
coverage_data$point <- ifelse(is.na(coverage_data$lat), 0, 1)

#keep only the surveys that have cooking fuel extracted
load("/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/hap_nids.Rdata", verbose=T)
coverage_data <- coverage_data[nid %in% nids]
coverage_data[, cooking_fuel := 1]

library(plyr)
coverage_data <- plyr::rename(coverage_data, c("total_hh"="N", "svy_id"="nid", "start_year"="year", "id_short" = "cluster_id",
                                               "iso3" = "country", "lat" = "latitude", "long" = "longitude", "year_start" = "year",
                                               "survey_series" = "source"))


#run loop
source('mbg_central/graph_data_coverage.R')
#regions <- c('africa', 'latin_america', 'middle_east','south_asia','se_asia')
regions <- 'se_asia'

#fix problem with a se asia shapefile (naming issue)
coverage_data <- coverage_data[shapefile == "lg_g2015_2007_1", shapefile := "lf_g2015_2007_1"]

for (reg in regions){
  coverage_maps <- graph_data_coverage_values(df = coverage_data,
                                              var = var,
                                              title = title,
                                              legend_title = "Prevalence",
                                              year_min = 1998,
                                              year_max = 2017,
                                              year_var = 'year',
                                              region = reg,

                                              cores = numcores,
                                              indicator = indicator,

                                              extra_file_tag = '',
                                              save_on_share = FALSE,
                                              out_dir = NULL,
                                              core_repo = repo,
                                              log_dir = NULL,

                                              fast_shapefiles = TRUE,
                                              new_data_plots = FALSE,
                                              since_date = NULL,
                                              annual_period_maps = FALSE,
                                              save_period_maps = TRUE,
                                              prep_shiny = FALSE,
                                              return_maps = TRUE,
                                              debug = FALSE,

                                              color_scheme = "classic",
                                              color_scheme_scatter = "brewer",
                                              high_is_bad = FALSE,
                                              cap = 90,
                                              cap_type = "percentile",
                                              legend_min = 0,
                                              legend_max = 1.1,
                                              endemic_gauls = NULL,
                                              stage_3_gray = TRUE,
                                              simplify_polys = TRUE,
                                              tolerance = 0.03,
                                              base_font_size = 18,
                                              map_point_size = 0.8,
                                              poly_line_width = 0.2,

                                              remove_rank = TRUE
  )
}
