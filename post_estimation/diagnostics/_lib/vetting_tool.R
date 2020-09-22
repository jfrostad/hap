# ----HEADER------------------------------------------------------------------------------------------------------------
# Author:#REDACTED
# Date: 07/12/2019
# Purpose: Function to help vet data for a given problem NID
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

#REDACTED

#load packages
require(ggwordcloud)
pacman::p_load(data.table, dplyr, farver, feather, fst, ggrepel, googledrive, 
               imguR, naniar, readxl, sf, stringr, viridis) 
#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#R options
options(scipen=999) #not a fan

#googledrive options
redownload.hap <- F #set T if new codebooking activity for HAP
redownload.wash <- F #set T if new data vetting activity for WASH

#plot options
imgur <- T #set T if saving addtl diagnostics to imgur
imgur_refresh <- F #set T if needing to refresh imgur token (must be done locally)
plot.wordcloud <- T #set T if you want to print a wordcloud to examine the string mapping
plot.pts <- T #set T if you want to print a map of the model input by SFU%
plot.miss <- T #set T to create plots of missingness over different factors
remote <- F #set T to save plots, if working in a remote or qlogin situation
use.sf <- T #set T if you are not having issues with the sf package
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#REDACTED
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
lbd.shared.function.dir <- file.path(h_root, "_#REDACTED")
file.path(lbd.shared.function.dir, 'setup.R') %>% source
suppressMessages(mbg_setup(repo=lbd.shared.function.dir, package_list = package_list))

#helper function in order to pull all relevant files for a given problem NID
vetAssistant <- function(this.nid,
                         var.fam='cooking',
                         dirs=list (
                           'raw'=raw.dir, #by default these dirs can be pulled from global namespace
                           'collapse'=collapse.dir,
                           'pe'=pe.dir,
                           'model'=model.dir,
                           'doc'=doc.dir,
                           'geog'=geog.dir,
                           'gbd'=gbd.dir
                         ),
                         debug=F) {
  
  message('pulling relevant files for problem survey, nid = ', this.nid)
  
  #pull the codebook
  message(' -> codebook')
  cb <-
    file.path(dirs[['doc']], 'hap.xlsx') %>% 
    read_xlsx(., sheet='codebook') %>% 
    as.data.table
  
  if (debug) browser()

  #pull the raw data
  message(' --> raw data') 
  raw.files <- 
    file.path(dirs[['raw']]) %>% 
    list.files(full.names = T, pattern='.csv') %>% 
    .[. %like% this.nid] %>% 
    #added second check against survey name in case of nids that are subsets of other, longer nids
    .[. %like% cb[nid==this.nid, ihme_loc_id]]
  
  if(length(raw.files)==1) { 
    
    raw <- fread(raw.files) 
    
  } else if (length(raw.files)>1) {
    
    raw <- lapply(raw.files, fread) %>% 
      rbindlist(fill=T, use.names=T) 
    
  } else if(length(raw.files)==0) raw <- 'No extract'
  
  #pull the collapsed data
  message(' ---> collapsed data')
  col <- 
    file.path(dirs[['collapse']]) %>% 
    list.files(full.names = T, pattern='.fst') %>% 
    sort(., decreasing=T) %>% 
    .[1] %>% #pull most recent collapsed data 
    read.fst(., as.data.table=T) 
  
  #TODO
  #if collapse is broken, check the post-extract file
  # if (nrow(col[nid==this.nid])==0) {
  #   pe <- 
  #     file.path(dirs[['pe']]) %>% 
  #     list.files(full.names = T, pattern='.fst') %>% 
  #     sort(., decreasing=T) %>% 
  #     .[1]
  
  #pull the model input data
  message(' ----> mbg input data') 
  mod <- 
    list.files(dirs[['model']], pattern='.fst', full.names = T) %>% 
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
  
  #pull the problem clusters diagnostic
  message(' ------> dropped clusters diagnostics') 
  drop <- 
    file.path(dirs[['doc']], paste0(var.fam, '/dropped_clusters.csv')) %>% 
    fread

  #pull the geographies info
  message(' --------> geographies info')
  if (nrow(col[nid==this.nid])==0) geog <- 'Cannot autopull geographies because collapse is broken.'
  else {
  geog <-
    file.path(dirs[['geog']]) %>% 
    #get survey series from the collapsed data
    list.files(full.names = T, 
               pattern=paste0(col[nid==this.nid, survey_series %>% unique], '.csv')) %>% 
    { if(length(.)>0) fread(.) else NA }
  }
  
  #pull the wash tracking sheet info
  message(' ---------> wash tracking sheet')
  wash <- 
  file.path(dirs[['doc']], 'WaSH Tracking Sheet.xlsx') %>% 
    read_xlsx(., sheet='WaSH Tracking Sheet') %>% 
    as.data.table

  #pull the gbd2017 hap outliers info
  message(' ----------> gbd outliers sheet')
  gbd <- file.path(dirs[['gbd']], 'outlier_db.csv') %>% 
    fread
  
  #subset to the nid and then output info list
  out <- list (
    'cb'=cb,
    'raw'=raw,
    'col'=col,
    'mod'=mod,
    'str'=str,
    'adm'=adm,
    'drop'=drop,
    'geog'=geog,
    'wash'=wash,
    'gbd'=gbd
  ) %>% 
    lapply(., function(x) {
      if (is.data.table(x)) x[nid==this.nid]
      else x
    }
  ) %>% return
}


##GRAPHS##

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
  
  return(plot)
  
}

#create a map of the SFU% by lat/long to show spatial distribution of model dataset
mapPoints <- function(info_list, borders_file=NULL, plot_borders=use.sf) {
  
  #TODO move to p_load when versioning issues are resolved
  if (plot_borders) require(sf)

  #build plot data
  dt <- info_list[['mod']] %>% 
    copy %>% 
    .[, ADM0_CODE := get_adm0_codes(iso3), by=iso3] #merge on ad0 code

  #subset borders file
  if (plot_borders) borders <- borders_file[borders_file$ADM0_CODE %in% dt$ADM0_CODE,]
  
  #pull out survey name from codebook
  svy_name <- info_list[['cb']]$survey_name
  
  #create plot canvas
  if (plot_borders) canvas <- ggplot(data=borders) + geom_sf(color='gray4')
  else canvas <- ggplot(data=dt)
  
  #finish plot
  plot <- 
  canvas +
    geom_point(data=dt, aes(x=longitude, y=latitude, color=cooking_fuel_solid/N, size=N)) +
    scale_color_viridis_c('SFU%', option='plasma', direction = -1) +
    ggtitle(paste0('Final MBG Input Dataset, for NID #', problem.nid), 
            paste0(svy_name, '...[N=', sum(dt$N), ']')) +
    theme_bw()
  
  print(plot) #display
  
  return(plot)
  
}

#create a map of missingness over a given variable
plotMiss <- function(info_list, by_var) {
  
  dt <- copy(info_list[['raw']]) %>% 
    .[, byvar := get(by_var)]
  
  #examine missingess by geospatial_id
  plot <-
    gg_miss_fct(x = dt, fct = byvar) + 
    labs(title = paste0("Missingness vs ", by_var))
  
  print(plot) #display
  
  return(plot)
  
}

#helper function to use imguR pkg to upload your plots to a single album w proper title
imgUploadHelper <- function(plots, my_tkn=tkn, cb=info[['cb']], nid=problem.nid) {
  
  #initialize title
  alb_title <- paste(cb$ihme_loc_id, nid, 'DIAGNOSTICS', sep='_')
  
  for (i in 1:length(plots)) {
    
    img <- imgur(token=my_tkn, height=900, width=1200)
    print(plots[[i]])
    obj <- imgur_off(img)
    #create album if on first loop
    if (i==1) alb <- create_album(obj, title = alb_title, privacy = "hidden", token = tkn)
    add_album_images(alb$id, obj, token = my_tkn)
    
  }
  
  message('www.imgur.com/a/',alb$id)
  
}

#helper function to examine the raw data DTA
readRawData <- function(this.nid,
                        var.fam='cooking',
                        dirs=list (
                          'raw'=raw.dir, #by default these dirs can be pulled from global namespace
                          'collapse'=collapse.dir,
                          'pe'=pe.dir,
                          'model'=model.dir,
                          'doc'=doc.dir,
                          'geog'=geog.dir,
                          'gbd'=gbd.dir
                        ),
                        debug=F) {
  
  if (debug) browser()
  
  #get path from codebook
  cb <-
    file.path(dirs[['doc']], 'hap.xlsx') %>% 
    read_xlsx(., sheet='codebook') %>% 
    as.data.table %>% 
    .[nid==this.nid]
  
  path <- cb$file_path %>% 
    gsub('J:', j_root, .) %>% 
    gsub('L:', l_root, .) %T>%
    message('reading raw file from\n', .)
    
  dt <- haven::read_dta(path) %>% 
    as.data.table
  
  message('it has these cols\n', cat(names(dt), '|'))
  message('attemping to subset to relavant cols')
  
  dt <-
    dt %>% 
    setnames(., names(dt), names(dt) %>% tolower) %>% 
    .[, c(cb$hh_size, cb$cooking_fuel), with=F] %>% 
    setnames(., c('hh_size', 'cooking_fuel')) %>% 
    return
    
}
#***********************************************************************************************************************

# ---VET----------------------------------------------------------------------------------------------------------------

#which nid are we vetting?
problem.nid <- 326837 #set the NID you want to vet

#build the vetting object
info <- vetAssistant(problem.nid)

#examine raw data
raw <- readRawData(problem.nid)

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
plot_list <- list() #initialize list
#REDACTED

#create wordcloud using internal function based on ggwordcloud#
if (plot.wordcloud) plot_list[[1]] <- wordCloud(str.dt= info[['str']], order=fuel.order, colors=fuel.colors)

#check out spatial patterns#
if (plot.pts) plot_list[[2]] <- mapPoints(info_list=info, borders_file=ad2.borders)

#check out missingness patterns
if (plot.miss) plot_list[[3]] <- plotMiss(info_list=info, by_var = 'admin_1')

#save plots
if (remote) dev.off()

#upload plots
if (imgur) imgUploadHelper(plots=plot_list)
#***********************************************************************************************************************