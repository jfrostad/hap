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
  
  package_lib    <- file.path(h_root, '_code/_lib/pkg')
  ## Load libraries and  MBG project functions.
  #.libPaths(c( .libPaths(), package_lib))
  .libPaths(package_lib)
  
  # necessary to set this option in order to read in a non-english character shapefile on a linux system (cluster)
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
} else {
  
  j_root <- "J:"
  h_root <- "H:"
  
}

#load packages
pacman::p_load(data.table, dplyr, feather, fst, ggrepel, ggwordcloud, googledrive, readxl, stringr, viridis) 

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
options(scipen=999) #not a fan
problem.nid <- 20417 #set the NID you want to vet
redownload.hap <- T #set T if new codebooking activity for HAP
redownload.wash <- T #set T if new data vetting activity for WASH
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path('/share/geospatial/mbg/input_data/')
raw.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/hap/')
geomatched.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/')
census.dir <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hap/census')
doc.dir <- file.path(j_root, 'WORK/11_geospatial/hap/documentation')
def.file <- file.path(doc.dir, 'definitions.xlsx')
collapse.dir  <- file.path('/share/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapse/hap/')
model.dir  <- file.path(j_root, 'WORK/11_geospatial/10_mbg/input_data/hap')
share.dir <- file.path('/share/geospatial/jfrostad')

###Output###
graph.dir <- file.path(j_root, 'WORK/11_geospatial/hap/graphs')

##Refresh google sheets##
setwd(doc.dir)
if(redownload.hap==T) drive_download(as_id('1Nd3m0ezwWxzi6TmEh-XU4xfoMjZLyvzJ7vZF1m8rv0o'), overwrite=T)
if(redownload.wash==T) drive_download(as_id('1xn91Y3_lIr0G0Z_BBn-HMO9B9vFFzvCf_PQYpIV4AM8'), overwrite=T)
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
#helper function in order to pull all relevant files for a given problem NID
vetAssistant <- function(this.nid,
                         var.fam='cooking',
                         dirs=list (
                           'raw'=raw.dir, #by default these dirs can be pulled from global namespace
                           'collapse'=collapse.dir,
                           'model'=model.dir,
                           'doc'=doc.dir
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
  
  #subset to the nid and then output info list
  out <- list (
    'cb'=cb,
    'raw'=raw,
    'col'=col,
    'mod'=mod,
    'str'=str,
    'adm'=adm
  ) %>% 
    lapply(., function(x) x[nid==this.nid]) %>% 
    return
}


#plot word cloud
wordCloud <- function(str.dt, this.var, order, colors) {
  
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
#***********************************************************************************************************************

# ---VET----------------------------------------------------------------------------------------------------------------
#build the vetting object
info <- vetAssistant(problem.nid)

#display available files
str(info)
names(info)

#example of how to look at one (codebook)
info[['cb']]
#***********************************************************************************************************************

# ---WORD CLOUDS FUELTYPE-----------------------------------------------------------------------------------------------
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

#create wordcloud using internal function based on ggwordcloud
#TODO setup scale for all vars
wordCloud(str.dt= info[['str']], order=fuel.order, colors=fuel.colors)

#look at raw data too
info[['str']][var=='cooking_fuel']
