# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 07/12/2019
# Purpose: Function to help vet data for a given problem NID
# source("/homes/jfrostad/_code/lbd/hap/diagnostics/_lib/vetting_tool.R", echo=T)
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  
  j_root <- "/home/j/"
  h_root <- file.path("/ihme/homes", Sys.info()["user"])
  l_root <- "/share/limited_use/"
  
  package_lib    <- file.path(h_root, '_code/_lib/pkg')
  ## Load libraries and  MBG project functions.
  #.libPaths(c( .libPaths(), package_lib))
  .libPaths(package_lib)
  
  # necessary to set this option in order to read in a non-english character shapefile on a linux system (cluster)
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
} else {
  
  j_root <- "J:"
  h_root <- "H:"
  l_root <- 'L:'
  
}

#load packages
pacman::p_load(data.table, dplyr, feather, fst, ggrepel, googledrive, naniar, readxl, sf, stringr, viridis) 
#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
options(scipen=999) #not a fan
problem.nid <- 30325 #set the NID you want to vet
redownload.hap <- F #set T if new codebooking activity for HAP
redownload.wash <- F #set T if new data vetting activity for WASH
build.wordcloud <- F #set T if you want to print a wordcloud to examine the string mapping
plot.pts <- T #set T if you want to print a map of the model input by SFU%
plot.miss <- T 
remote <- T
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path('/share/geospatial/mbg/input_data/')
raw.dir <- file.path(l_root, 'LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/hap/')
geomatched.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/')
geog.dir <- file.path(j_root, 'WORK/11_geospatial/05_survey shapefile library/codebooks')
census.dir <- file.path(l_root, 'LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/census')
doc.dir <- file.path(j_root, 'WORK/11_geospatial/hap/documentation')
  package_list <- file.path(doc.dir, 'package_list.csv') %>% fread %>% t %>% c
def.file <- file.path(doc.dir, 'definitions.xlsx')
collapse.dir  <- file.path(l_root, 'LIMITED_USE/LU_GEOSPATIAL/collapse/hap/')
model.dir  <- file.path(j_root, 'WORK/11_geospatial/10_mbg/input_data/hap')
share.dir <- file.path('/share/geospatial/jfrostad')

#borders file for plotting admin2 borders
ad1.borders <- file.path(j_root, 'WORK/11_geospatial/admin_shapefiles/current/lbd_standard_admin_1_simplified.shp') %>% read_sf
ad2.borders <- file.path(j_root, 'WORK/11_geospatial/admin_shapefiles/current/lbd_standard_admin_2_simplified.shp') %>% read_sf

###output###
graph.dir <- file.path(j_root, 'WORK/11_geospatial/hap/graphs/vetting')

##Refresh google sheets##
setwd(doc.dir)
if(redownload.hap==T) drive_download(as_id('1Nd3m0ezwWxzi6TmEh-XU4xfoMjZLyvzJ7vZF1m8rv0o'), overwrite=T)
if(redownload.wash==T) drive_download(as_id('1xn91Y3_lIr0G0Z_BBn-HMO9B9vFFzvCf_PQYpIV4AM8'), overwrite=T)
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
lbd.shared.function.dir <- file.path(h_root, "_code/lbd/hap/mbg_central")
file.path(lbd.shared.function.dir, 'setup.R') %>% source
mbg_setup(repo=lbd.shared.function.dir, package_list = package_list)

#helper function in order to pull all relevant files for a given problem NID
vetAssistant <- function(this.nid,
                         var.fam='cooking',
                         dirs=list (
                           'raw'=raw.dir, #by default these dirs can be pulled from global namespace
                           'collapse'=collapse.dir,
                           'model'=model.dir,
                           'doc'=doc.dir,
                           'geog'=geog.dir
                         )) {
  
  message('pulling relevant files for problem survey, nid = ', this.nid)
  
  #pull the codebook
  message(' -> codebook')
  cb <-
    file.path(dirs[['raw']], 'hap.xlsx') %>% 
    read_xlsx(., sheet='codebook') %>% 
    as.data.table
  
  #pull the raw data
  message(' --> raw data') 
  raw <- 
    file.path(dirs[['raw']]) %>% 
    list.files(full.names = T, pattern='.csv') %>% 
    .[. %like% this.nid] %>% 
    fread
  
  #pull the collapsed data
  message(' ---> collapsed data')
  col <- 
    file.path(dirs[['collapse']]) %>% 
    list.files(full.names = T, pattern='.fst') %>% 
    sort(., decreasing=T) %>% 
    .[1] %>% #pull most recent collapsed data 
    read.fst(., as.data.table=T) 
  
  #pull the model input data
  message(' ----> mbg input data') 
  mod <- 
    list.files(dirs[['model']], full.names = T) %>% 
    sort(., decreasing=T) %>% 
    .[1] %>% #pull most recent collapsed data 
    read.fst(., as.data.table=T) 
  
  #pull the string combos
  message(' -----> string matches')
  str <- 
    file.path(dirs[['doc']], 'str_review', paste0(var.fam, '_string_match_tabulations.csv')) %>% 
    fread
  
  #pull the admin level comparisons to gbd and missingness diagnostics
  #TODO improve file naming here to incorporate housing in future
  message(' ------> adm level comparisons') 
  adm <- 
    file.path(dirs[['doc']], 'gbd_solid_fuel_comparison.csv') %>% 
    fread

  #pull the geographies info
  message(' -------> geographies info')
  geog <-
    file.path(dirs[['geog']]) %>% 
    #get survey series from the collapsed data
    list.files(full.names = T, 
               pattern=paste0(col[nid==this.nid, survey_series %>% unique], '.csv')) %>% 
    fread
  
  #subset to the nid and then output info list
  out <- list (
    'cb'=cb,
    'raw'=raw,
    'col'=col,
    'mod'=mod,
    'str'=str,
    'adm'=adm,
    'geog'=geog
  ) %>% 
    lapply(., function(x) x[nid==this.nid]) %>% 
    return
}


##GRAPHS##

#plot word cloud
wordCloud <- function(str.dt, this.var, order, colors) {
  
  #TODO move back to p_load when versioning issues are resolved (new singularity?)
  require(ggwordcloud)
  
  #subset data
  dt <- copy(str.dt)
  dt[, factor_mapped := factor(var_mapped, levels=order)]
  
  plot <- 
    ggplot(data=dt) +
    aes(x = 1, y = 1, size = prop, label = var_og,
        color= factor_mapped) +
    geom_text_wordcloud_area()+
    scale_size(range = c(5, 20), guide = FALSE) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    labs(x = '', y = '') +
    facet_grid(~var) +
    scale_color_manual(values = colors) +
    theme_minimal() +
    theme(strip.text = element_text(
      color="black", size=16, lineheight=5.0),
      plot.title = element_text(colour = "black",
                                size = 18,
                                hjust = 0.5, vjust = 0.8, angle = 0))
  
  print(plot)
  
  return(NULL)
  
}

#create a map of the SFU% by lat/long to show spatial distribution of model dataset
mapPoints <- function(info_list, borders_file) {

  #build plot data
  dt <- info_list[['mod']] %>% 
    copy %>% 
    .[, ADM0_CODE := get_adm0_codes(iso3), by=iso3] #merge on ad0 code
  #subset borders file
  borders <- borders_file[borders_file$ADM0_CODE %in% dt$ADM0_CODE,]
  
  #pull out survey name from codebook
  svy_name <- info_list[['cb']]$survey_name
  
  #make plot
  plot <- 
  ggplot(data=borders) +
    geom_sf(color='gray4') +
    geom_point(data=dt, aes(x=longitude, y=latitude, color=cooking_fuel_solid/N, size=N)) +
    scale_color_viridis_c('SFU%', option='plasma') +
    ggtitle(paste0('Final MBG Input Dataset, for NID #', problem.nid), 
            paste0(svy_name, '...[N=', sum(dt$N), ']')) +
    theme_bw()
  
  print(plot) #display
  
  return(NA)
  
}

#create a map of missingness over a given variable
plotMiss <- function(info_list, by_var) {
  
  dt <- copy(info_list[['raw']]) %>% 
    .[, byvar := get(by_var)]
  
  #examine missingess by geospatial_id
  plot <-
    gg_miss_fct(x = dt, fct = byvar) + 
    labs(title = "Missingness vs space")
  
  print(plot) #display
  
  return(NA)
  
}
#***********************************************************************************************************************

# ---VET----------------------------------------------------------------------------------------------------------------
#build the vetting object
info <- vetAssistant(problem.nid)

#display available files
str(info)
names(info)

#example of how to look at one (codebook)
info[['cb']]

#look at raw string data too
info[['str']][var=='cooking_fuel']
#***********************************************************************************************************************

# ---GRAPHING-----------------------------------------------------------------------------------------------------------
#build color scales#
#set up order of fuel types then produce color scales
fuel.order <- c('none', 'electricity', 'gas', 'kerosene', 'coal', 'wood', 'crop', 'dung', 'other', 'unknown', 'missing')
fuel.colors <- c(plasma(8, direction=-1), "#C0C0C0", "#C0C0C0", "#C0C0C0") #use gray for other and unknown
names(fuel.colors) <- fuel.order

#set up order of cooking locations then produce color scales
loc.order <- c('outside', 'kitchen', 'inside', 'other')
loc.colors <- c(plasma(3, direction=-1), "#C0C0C0") #use gray for other
names(loc.colors) <- loc.order

#set up order of cooking types then produce color scales
type.order <- c('open_chimney', 'closed', 'open', 'other')
type.colors <- c(plasma(3, direction=-1), "#C0C0C0") #use gray for other 
names(type.colors) <- type.order

#save plots
if (remote) pdf(file = paste0(graph.dir, '/', problem.nid, '_vetting_tool.pdf'), height=8, width=11)

#create wordcloud using internal function based on ggwordcloud#
#TODO setup scale for all vars
if (build.wordcloud) wordCloud(str.dt= info[['str']], order=fuel.order, colors=fuel.colors)

#check out spatial patterns#
if (plot.pts) mapPoints(info_list=info, borders_file=ad1.borders)

#check out missingness patterns
if (plot.miss) plotMiss(info_list=info, by_var = 'admin_1')

#save plots
if (remote) dev.off()
#***********************************************************************************************************************