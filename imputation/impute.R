# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 06/12/2018
# Purpose: Collapse data for HAP
# source("/homes/jfrostad/_code/lbd/hap/imputation/impute.R", echo=T)
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
  .libPaths(package_lib)
  
  # necessary to set this option in order to read in a non-english character shapefile on a linux system (cluster)
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
} else {
  j_root <- "J:"
  h_root <- "H:"
}

#load packages
pacman::p_load(data.table, dplyr, feather, fst, mgcv, mgcViz, naniar, readxl) 
#TODO verify which of these are actually necessary, took from a random image in Ani's wash dir
#/share/geospatial/mbg/wash/s_imp/model_image_history
pkg.list <- c('RMySQL', 'data.table', 'dismo', 'doParallel', 'dplyr', 'foreign', 'gbm', 'ggplot2', 'glmnet', 
              'grid', 'gridExtra', 'gtools', 'magrittr', 'pacman', 'parallel', 'plyr', 'raster', 'rgdal', 'rgeos',
              'seegMBG', 'seegSDM', 'tictoc') #will be loaded by MBG setup

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
cores <- 20
manual_date <- "2018_12_18" #set this value to use a manually specified extract date
latest_date <- T #set to TRUE in order to disregard manual date and automatically pull the latest value
new.extract <- F
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/')
census.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/census')
doc.dir <- file.path(j_root, 'WORK/11_geospatial/hap/documentation')
def.file <- file.path(doc.dir, 'definitions.xlsx')

###Output###
graphs.dir <- file.path(j_root, 'WORK/11_geospatial/hap/graphs/hh_size')
out.dir  <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapse/hap/')
model.dir  <- file.path(j_root, 'WORK/11_geospatial/10_mbg/input_data/hap')
share.model.dir  <- file.path('/share/geospatial/mbg/input_data/')
temp.dir <- file.path('/share/geospatial/jfrostad')
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
#mbg_setup(repo=lbd.shared.function.dir, package_list=pkg.list) #load mbg functions

#TODO move to misc fx
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
#***********************************************************************************************************************

# ---PREP---------------------------------------------------------------------------------------------------------------
##read in data##
#pull in the combined pt/poly dataset if there has been a recent post-extraction run
if (new.extract) {
  #automatically pull latest date if manual date not provided
  # get input version from most recently modified data file
  if (latest_date) { 
    file_date <- gsub('.feather|points_|poly_', 
                      '', 
                      sort(list.files(data.dir, pattern = '*points'), decreasing=T)[1]) #pull latest poitns filename
  } else file_date <- manual_date

  raw <- paste0(data.dir, 'points_', file_date, ".feather") %>% 
    read_feather %>% 
    as.data.table
  
  names <- c('nid', 'iso3', 'geospatial_id', 'point', 'lat', 'long', 'shapefile', 
             'location_code', 'survey_series', 'survey_module', 'survey_name', 'int_year', 
             'hhweight', 'psu', 'smaller_site_unit', 'strata', 'admin_1', 'admin_4', 'admin_5', 
             'urban', 'hh_size', 'electricity', 'lighting_fuel', 
             getLikeMe(raw, 'heating'), getLikeMe(raw, 'cooking'), getLikeMe(raw, 'housing'))
  
  pt <- raw[, (names), with=F]
  
  #note that poly file is now split into 2 files due to size limitations
  raw <- lapply(paste0(data.dir, c('poly1', 'poly2'), '_', file_date, ".feather"), 
                read_feather) %>% rbindlist
  
  #combine
  raw <- list(raw[, (names), with=F], 
             pt) %>% rbindlist 
  
  #save as fst
  write.fst(raw, path=file.path('/share/geospatial/jfrostad/hap_data_raw.fst'))
  
  #cleanup
  list(pt, raw) %>% rm
  
  #examine missingness
  gg_miss_fct(x = raw, fct = survey_series) + labs(title = "NA by Variable | Survey Series") +
    theme(axis.text.x = element_text(angle = 15))
  
  ggsave(filename=file.path(graphs.dir, 'hap_missingness_by_survey.png'), width=12, height=8, units='in')
  
} else raw <- file.path('/share/geospatial/jfrostad/hap_data_raw.fst') %>% read.fst(path=., as.data.table=T)

#add on location info
#read in the GBD estimates/location info
locs.meta = get_location_metadata(location_set_id = 9, gbd_round_id = 5)

raw <- merge(raw, locs.meta[, .(ihme_loc_id, region_id, super_region_id)], 
             by.x='iso3', by.y='ihme_loc_id')

#add a different value to urban=missing so we can visualize more easily
raw[is.na(urban), urban := 99]

#set iso3 to first 3 only
raw[, ihme_loc_id := substr(iso3, start=1, stop=3) %>% as.factor]

#factorize variables
raw[, reg := region_id %>% as.factor]
raw[, urb := urban %>% as.factor]

#trim dt to the vars you expect to use in prediction
pred.vars <- c('nid', 'ihme_loc_id', 'iso3', 'admin_1', 'geospatial_id', 'survey_series', 'survey_module', 'int_year', 'urban', 'urb',
               'hhweight', 'hh_size', 'electricity', 'housing_floor', 'cooking_fuel_mapped',
               'region_id', 'reg', 'super_region_id')

#split pt/poly again, we will try to use lat/long to predict in pt
pt <- raw[, c(pred.vars, 'lat', 'long'), with=F] %>% na.omit
poly <- raw[, c(pred.vars, 'shapefile'), with=F] %>% na.omit

stop()
#***********************************************************************************************************************

# ---GRAPH--------------------------------------------------------------------------------------------------------------
#try some exploratory graphing
scatterVsYear <- function(super_region) {
  
  message(super_region)
  
  these.locs <- locs.meta[super_region_id==super_region]

  #points
  plot <-
  ggplot(pt[super_region_id==super_region], 
         aes(x=int_year, y=hh_size, shape=electricity %>% as.factor, color=region_id %>% as.factor)) + 
    geom_jitter() +
    facet_wrap(~urban) +
    scale_color_brewer(palette='Paired',
                       labels=these.locs[region_name != '', region_name] %>% unique) +
    ggtitle('Point data',
            subtitle = these.locs[1, super_region_name]) +
    theme_minimal() 
  
  print(plot)
  
  #polys
  plot <-
    ggplot(poly[super_region_id==super_region], 
           aes(x=int_year, y=hh_size, shape=electricity %>% as.factor, color=region_id %>% as.factor)) + 
    geom_jitter() +
    facet_wrap(~urban) +
    scale_color_brewer(palette='Paired',
                       labels=these.locs[region_name != '', region_name] %>% unique) +
    ggtitle('Point data',
            subtitle = these.locs[1, super_region_name]) +
    theme_minimal() 
  
  print(plot)
  
  return(NA)

}

violinVsYear <- function(super_region) {
  
  message(super_region)
  
  these.locs <- locs.meta[super_region_id==super_region]

  plot <-
    ggplot(pt[super_region_id==super_region], 
           aes(x=int_year %>% as.factor, y=log(hh_size), shape=electricity %>% as.factor, color=region_id %>% as.factor)) + 
    geom_violin(aes(group=int_year %>% as.factor), trim=T) +
    geom_jitter(aes(color=region_id %>% as.factor), shape=16, position=position_jitter(0.2), alpha=.2) +
    facet_wrap(~urban) +
    scale_color_brewer(palette='Paired',
                       labels=these.locs[region_name != '', region_name] %>% unique) +
    ggtitle('Point data',
            subtitle = these.locs[1, super_region_name]) +
    theme_minimal() 
  
  print(plot)
  
  plot <-
    ggplot(poly[super_region_id==super_region], 
           aes(x=int_year %>% as.factor, y=log(hh_size), shape=electricity %>% as.factor, color=region_id %>% as.factor)) + 
    geom_violin(aes(group=int_year %>% as.factor), trim=T) +
    geom_jitter(aes(color=region_id %>% as.factor), shape=16, position=position_jitter(0.2), alpha=.2) +
    facet_wrap(~urban) +
    scale_color_brewer(palette='Paired',
                       labels=these.locs[region_name != '', region_name] %>% unique) +
    ggtitle('Point data',
            subtitle = these.locs[1, super_region_name]) +
    theme_minimal() 
  
  print(plot)
  
}

#now generate all the plots as a single PDF to aide in overall vetting
pdf(file=file.path(graphs.dir, 'size_vs_year.pdf'),
    height=8, width=12)

lapply(unique(pt$super_region_id), scatterVsYear)

dev.off()

#now generate all the plots as a single PDF to aide in overall vetting
pdf(file=file.path(graphs.dir, 'size_vs_year_violin.pdf'),
    height=8, width=12)

lapply(unique(pt$super_region_id), violinVsYear)

dev.off()

#plot the hh sizes that are greater than 15, could use some exploration
ggplot(raw[hh_size>10 & hh_size<35], aes(hh_size, fill=survey_series)) + 
  geom_histogram(bins=uniqueN(raw[hh_size>10 & hh_size<35, hh_size])) +
  ggtitle('HH Sizes > 15') +
  theme_minimal()

ggplot(raw[hh_size>35 & hh_size<105], aes(hh_size, fill=survey_series)) + 
  geom_histogram(bins=uniqueN(raw[hh_size>35 & hh_size<105, hh_size])) +
  ggtitle('HH Sizes > 35') +
  theme_minimal()

#are there differences in cooking fuel distributions?
#set up order of fuel types then produce color scales
fuel.order <- c('none', 'electricity', 'gas', 'kerosene', 'coal', 'wood', 'crop', 'dung', 'other', 'unknown', 'missing')
fuel.colors <- c(plasma(8, direction=-1), "#C0C0C0", "#C0C0C0", "#C0C0C0") #use gray for other and unknown
names(fuel.colors) <- fuel.order

raw[, fuel_factor := factor(cooking_fuel, levels=fuel.order)]

ggplot(raw[hh_size>15 & hh_size<105 & !is.na(fuel_factor)], aes(as.factor(hh_size), fill=fuel_factor)) + 
  geom_bar(position="fill") +
  #scale_y_continuous(labels = percent) +
  scale_fill_manual(values = fuel.colors) +
  ggtitle('HH Sizes > 35') +
  theme_minimal()

#***********************************************************************************************************************

# ---MODEL--------------------------------------------------------------------------------------------------------------
##RIDGE##
#first try a ridge regression on hh_size
#setup response/predictor matrix
predictors <- pt[, -c('hh_size', 'admin_1', 'geospatial_id'), with=F] %>% sparse.model.matrix(~., .)
lambda.vals <- 10^seq(3,-3,length=100)

#fit a ridge regression
#use alpha=0 for ridge
registerDoParallel(cores)
ridge.mod <- cv.glmnet(x=predictors, y=hh_size, family='poisson', 
                       lambda=lambda.vals, alpha=0, nfolds=5, parallel=T)

#fit full model using selected lambdas
model = glmnet(x = predictors, y= hh_size, family = 'poisson', lambda = ridge.mod$lambda.1se, alpha = 0)

#plot results from CV
pdf(file.path(graphs.dir, 'ridge_results.pdf'), height=8, width=12)

plot(ridge.mod$glmnet.fit, xvar = "lambda") #coefficient plot
plot(ridge.mod) #plot MSE

dev.off()

#generate predictions
preds <- predict(ridge.mod, newx=predictors, type='response')
pt[, ridge_pred := preds]
summary(pt[, ridge_pred-hh_size])
hist(pt[, ridge_pred-hh_size])

##GAM##
#try gam
gam.mod <- bam(hh_size ~ s(int_year, by=reg, bs='tp', k=5) + s(lat, long, by=ihme_loc_id, bs='tp', k=5) + electricity + urb, 
               data = pt, family=poisson(), discrete=TRUE, nthreads=20)

gam.mod2 <- 

#generate predictions
preds <- predict(gam.mod2, newdata=pt, type='response')
pt[, gam_pred := preds]
pt[, resid := hh_size - gam_pred]

#plot results
plotResid <- function(dt, region) {
  
  plot <-
    ggplot(dt[region_id==region], aes(resid, fill=ihme_loc_id)) + 
    geom_density(alpha=.2) +
    facet_wrap(~urban) +
    xlim(-15, 15) +
    theme_minimal()
  
  print(plot)
  return(NA)
  
}



pdf(file=file.path(graphs.dir, 'regional_resid.pdf'),
    height=8, width=12)

lapply(unique(pt$region_id), plotResid, dt=pt)

dev.off()

#plot residuals vs time
plotResidTime <- function(dt, region) {
  
  plot <-
    ggplot(dt[region_id==region], aes(x=int_year, y=resid, color=ihme_loc_id)) + 
    geom_violin(aes(group=int_year %>% as.factor), trim=T) +
    geom_jitter(aes(color=ihme_loc_id), shape=16, position=position_jitter(0.2), alpha=.2) +
    facet_wrap(~urban) +
    theme_minimal()
  
  print(plot)
  return(NA)
  
}

pdf(file=file.path(graphs.dir, 'regional_resid_v_year.pdf'),
    height=8, width=12)

lapply(unique(pt$region_id), plotResidTime, dt=pt)

dev.off()

#plot residuals v space
plotResidSpace <- function(dt, region) {
  
  plot <-
    ggplot(dt[region_id==region]) + 
    stat_summary_hex(aes(x = long, y = lat, z=resid),
                     fun = mean, bins = 50, alpha = 0.75) +
    scale_fill_gradientn(colours = c("blue","red")) +
    ggtitle('Point data',
            subtitle = locs.meta[region_id==region, region_name] %>% unique) +
    theme_minimal()
  
  print(plot)
  
  return(NA)
  
}

pdf(file=file.path(graphs.dir, 'regional_resid_v_space.pdf'),
    height=8, width=12)

lapply(unique(pt$region_id), plotResidSpace, dt=pt)

dev.off()

#plot mgcviz results
m <- getViz(mod)

#GAMCHECK
print(plot(m, allTerms = T), pages = 1)

#qqplot
qq(m, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))

#2d residual plot of lat long
ck1 <- check2D(m, x1 = "lat", x2 = "long")
ck1 + l_gridCheck2D(gridFun = mean) #+ scale_fill_brewer(type='div')

#plot first smooth with rug
o <- plot( sm(m, 2) )
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()
