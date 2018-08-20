# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 08/20/2018
# Purpose: Hap model, diagnostics
# source("/homes/jfrostad/_code/lbd/housing/model/diagnostics.R", echo=T)
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
  }
  
  package_lib    <- sprintf('%s_code/_lib/pkg',h_root)
  ## Load libraries and  MBG project functions.
  .libPaths(package_lib)
  
  # necessary to set this option in order to read in a non-english character shapefile on a linux system (cluster)
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
} else {
  j_root <- "J:"
  h_root <- "H:"

}

#load packages
pacman::p_load(data.table, corrplot, dplyr, feather, readxl) 
#TODO verify which of these are actually necessary, took from a random image in Ani's wash dir
#/share/geospatial/mbg/wash/s_imp/model_image_history
pkg.list <- c('RMySQL', 'data.table', 'dismo', 'doParallel', 'dplyr', 'foreign', 'gbm', 'ggplot2', 'glmnet', 
              'grid', 'gridExtra', 'gtools', 'magrittr', 'pacman', 'parallel', 'plyr', 'raster', 'rgdal', 'rgeos',
              'seegMBG', 'seegSDM', 'tictoc') #will be loaded by MBG setup

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
run_date <- "2018_08_20_11_18_04" #date of current post-extraction
indicator_group   <- 'hap'
indicator         <- 'cooking_fuel'
cores <- 40

#toggles
plot_covariates <- T
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path('/share/geospatial/mbg/input_data/')
doc.dir <- file.path(j_root, 'WORK/11_geospatial/hap/documentation')
def.file <- file.path(doc.dir, 'definitions.xlsx')
share.dir <- file.path('/share/geospatial/mbg', indicator_group, indicator, 'output', run_date)

###Output###
out.dir  <- file.path(j_root,'temp/jfrostad', indicator_group, indicator)

#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#general functions#
central.function.dir <- file.path(h_root, "_code/_lib/functions")
# this pulls the general misc helper functions
file.path(central.function.dir, "misc.R") %>% source
#hap functions#
hap.function.dir <- file.path(h_root, '_code/lbd/housing/extract/functions')
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
  
#custom fx
#fx written by natalia in order to plot the covariate weights for a given model
get_cov_weights <- function(indicator, indicator_group, run_date, regions, outdir) {
  
  # use mbg_central functions to calculate covariate weights and make plots
  all_plots <- lapply(regions, function(r) {
    # calculate weights (this auto-saves the output)
    cov.wts <- get.cov.wts(rd = run_date,
                           ind = indicator,
                           ind_gp = indicator_group,
                           reg = r,
                           age = 0,
                           holdout = 0)
    
    # make plots
    cov.plots <- plot.cov.wts(rd = run_date,
                              ind = indicator,
                              ind_gp = indicator_group,
                              reg = r,
                              age = 0,
                              holdout = 0)
    cov.plots <- cov.plots + labs(x="", y="", title=r)
    return(cov.plots)
  })
  
  # save plots (combined, and individually)
  pdf(paste0(outdir, "/cov_wts_all_regions.pdf"), width=10, height=7)
  do.call("grid.arrange", all_plots)
  for (ii in 1:length(regions)) print(all_plots[[ii]])
  dev.off()
  
  return("Plots saved!")
}
#***********************************************************************************************************************

# ---COVARIATES---------------------------------------------------------------------------------------------------------
#load in the cov data, saved by parallel model after loading and cropping
cov_list <- readRDS('/homes/jfrostad/temp/hap_covs.RDS')

if (plot_covariates == TRUE) {
  message('Saving covariate maps')
  file.path(out.dir, '/cov_maps') %>% dir.create
  covariates <- names(cov_list)
  for (cov in covariates) {
    message('-->saving...', cov)
    png(paste0(out.dir, '/cov_maps/', cov, '.png'), 1800, 1200)
    print(spplot(cov_list[[cov]],
                 main=list(label=cov,cex=2)))
    dev.off()
  }
}

#make a brick with just the 2010 vals
makeBrick <- function(ras, selection) {
  
  if(nlayers(ras)>1) {
    ras[[selection]] %>% return 
  } else return(ras)
  
}

brick <- lapply(cov_list, makeBrick, selection=10) %>% stack #stack bricks homie

#make a correlation plot with all covs
corr <- layerStats(brick, 'pearson', na.rm=T)
corr_matrix <- corr$'pearson correlation coefficient'

corr <- layerStats(brick, 'spearman', na.rm=T)
corr_matrix <- corr$'pearson correlation coefficient'

png(paste0(out.dir, '/covariates/corrplot.png'), 1800, 1200)
  corrplot.mixed(corr_matrix)
dev.off()

#get/plot covariate weights using central function
get_cov_weights(indicator=indicator, 
               indicator_group=indicator_group,
               run_date=run_date,
               regions=c('per'),
               outdir=out.dir)
#***********************************************************************************************************************

# ---INPUT DATA---------------------------------------------------------------------------------------------------------
#read in the input data
dt <- file.path(data.dir, 'cooking_fuel.csv') %>% fread

#bring in region and sr info
locs <- get_location_hierarchy(2)
dt <- merge(dt, locs[, .(ihme_loc_id, region_name, super_region_name)], by.x='country', by.y='ihme_loc_id')

#make violin plots
dt[, prev := cooking_fuel / N]

png(paste0(out.dir, '/solid_fuel_violin.png'), 1800, 1200)
ggplot(data = dt, aes(x = as.factor(round_any(year, 5)), 
                      y = prev)) + 
  geom_violin(position = "dodge") + facet_wrap(~country)
dev.off()

#peru plot
ggplot(data = cooking[ihme_loc_id=="PER"], aes(x = as.factor(round_any(int_year, 5)), 
                      y = prev)) + 
  geom_violin(position = "dodge") + facet_wrap(~country)
