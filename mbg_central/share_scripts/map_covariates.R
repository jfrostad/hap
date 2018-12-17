##################################
# Author: Rebecca Stubbs
# Date: 5/31/2017
# Purpose: Create pdf map libraries
# of geospatial covariates
##################################

# cd /share/code/geospatial/stubbsrw/mbg/scripts/
# git pull origin cgf_stubbsrw
# qsub -cwd -P proj_geo_nodes -l geos_node=TRUE -pe multi_slot 30 -N map_covariates -e /homes/stubbsrw/qsub_logs/errors -o /homes/stubbsrw/qsub_logs/output /share/code/geospatial/stubbsrw/mbg/child_growth_failure/r_shell_geos.sh map_covariates.R
#####################################################################

# Prepare the workspace
rm(list=ls())
root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/") #discovering what system the code is running on-- the cluster, windows, etc.

# Define where you want the maps to be written to
out_dir<-paste0(root,"/WORK/11_geospatial/01_covariates/covariate_maps/") #Other options might be: "/share/geospatial/covariates/" "temp/geospatial/covariate_maps/" 

# Loading libraries, scripts, etc. 
package_list <- c('data.table','raster','MapSuite', 'sp')
if(root=="J:/"){
  package_lib<-paste0(root,'temp/geospatial/packages_windows')
}else{
  package_lib <- ifelse(grepl("geos", Sys.info()[4]),
                        paste0(root,'temp/geospatial/geos_packages'),
                        paste0(root,'temp/geospatial/packages'))
}
.libPaths(package_lib)  # Ensures packages look for dependencies here when called with library().
for(package in package_list) {library(package, lib.loc = package_lib, character.only=TRUE)}

# Define whether you want maps that already exist to be overwritten and not skipped
overwrite<-F

#####################################################################
# Define the List of Covariates and Measures
#####################################################################
#' What we need is a list of variables, and their measures, to know what
#' covariates to look for. We can achieve this by reading in a config file,
#' or by manually defining (or reading in a different .csv, etc) the variables
#' we are interested in and their measures.


# Loading in a config file to discover the covariates you want to map:
repo<-"/share/code/geospatial/stubbsrw/mbg/" #<------------------------------------------------------------------- This is the repo that the config, and the load_config function will be called from.
source(paste0(repo,'/mbg_central/prep_functions.R')) # You need to load in these function go get the load_config() function

indicator<-"wasting_mod_b" # <-------------------------------------------------------------------------------------------- YOUR INDICATOR AND INDICATOR GROUP GOES HERE
indicator_group<-"child_growth_failure"

# Read in a config file, get the covariates used by that model
config <- load_config(repo = repo, indicator_group = indicator_group, indicator = indicator)

fe<-data.table(variable=unlist(strsplit(config[V1=='fixed_effects']$V2," ")),
               fixed_measures=unlist(strsplit(config[V1=='fixed_effects_measures']$V2," ")))
fe<-fe[variable!="+"]

variables<-fe$variable
measures<-fe$fixed_measures

# Altnernately, you could simply define a list of covariates and measures that are the same length, or read in a .csv, etc.

if(length(variables)!=length(measures)){
  stop("Looks like you have defined a list of variables and measures that don't have the same length-- each variable needs to have a measure!")
}

#####################################################################
# Map Covariates Using MapSuite
#####################################################################
#' What we need is a list of variables, and their measures, to know what
#' covariates to look for. We can achieve this by reading in a config file,
cov_tif_dir<-paste0(root,"WORK/11_geospatial/01_covariates/00_MBG_STANDARD/")

for (r in 1:length(variables)){ # For each variable/measure combination...
  cov<-variables[r]
  measure<-measures[r]
  
  print(paste0("Working on: ",cov,", measure: ",measure))
  
  if( overwrite==F & !file.exists(paste0(out_dir,cov,"_",measure,".pdf")) ){ # Note that this will not re-write the PDF if it already exists, unless `overwrite` is T.
    
    # Check to see if the covariate is synoptic (this information isn't captured in the 
    # config file):
    temporal_options<-list.dirs(paste0(cov_tif_dir,cov,"/",measure),full.names=F,recursive=F)
    if ("synoptic" %in% temporal_options){measure_type<-"synoptic"}
    if("1y" %in% temporal_options){measure_type<-"1y"}
    
    # Define the file paths and names
    raster_directory<-paste0(cov_tif_dir,cov,"/",measure,"/",measure_type,"/")
    raster_name<-paste0(cov,"_",measure,"_",measure_type)
    
    # Discovering the .tifs prefixes (this determines the years for each variable)
    prefixes<-data.table(files=list.files(raster_directory,pattern=".tif")) # Getting all the .tifs in the folder
    prefixes[,prefix:=unlist(strsplit(files,".tif"))[1],by=c("files")] # Getting only the stuff before the .tif
    prefixes<-unique(prefixes$prefix) # Only the unique .tifs
    prefixes<-gsub("_00_00","", prefixes) # getting rid of "_00_00" at the tail end of things:
    if(measure_type=="synoptic"){
      raster_years<-"synoptic"
    }else{
      raster_years<-as.numeric(gsub(paste0(raster_name,"_"),"", prefixes)) # Getting the years available for this raster
      raster_years<-raster_years[raster_years %in% 2000:2015]
    }
    
    # Getting all the covariate measurements from disparate years into 1 data object:
    message("Getting raster info as a table")
    map_table<-list()
    for (yr in raster_years){
      print(yr)
      if(yr=="synoptic"){
        results_tif<-raster(paste0(raster_directory,raster_name,".tif"))
      }else{
        results_tif<-raster(paste0(raster_directory,raster_name,"_",yr,"_00_00.tif"))
      }
      pts <- data.table(rasterToPoints(results_tif)) # Create a data.table based on that year
      pts[,id:=seq(1:nrow(pts))] # Generate a point ID
      pts[,year:=yr] # Assign a year
      map_table[[as.character(yr)]]<-pts # add the data.table to the list
    }
    map_table<-rbindlist(map_table)
    names(map_table)<-c("x","y","value","id","year")
    
    if("synoptic"%in% unique(map_table$year)){
      map_years<-"synoptic"
    }else{
      map_years<-2000:2015
    }
    
    # Start a PDF, Map The Results
    pdf(file=paste0(out_dir,cov,"_",measure,".pdf"),width=15,height=10)
    lim<-c(min(map_table$value,na.rm=T),max(map_table$value,na.rm=T)) # Discovering the min/max
    
    for(yr in map_years){
      data<-map_table[year==yr,] # Parsing out the data year by year like this makes this operation run faster.
      print(paste0("Mapping covariate ",cov,"; Year: ",yr))
      
      # MAP THE COVARIATE #<----------------------------------------------------------------------------------------------Here is where you would change formatting parameters.
      map_object<-RasterMap(coords = data, id = "id", xcol= "x", ycol = "y",
                            variable= "value", 
                            map_colors_limits = lim,
                            map_colors = wpal("sky"),
                            histogram=T, 
                            map_title = paste0(cov,": ",measure), map_subtitle=paste0("Year: ",yr), include_titles = T,
                            return_objects = T)$map
      print(map_object)
    }
    dev.off()
    
  }else{
    print(paste0("There's already a map PDF for ",cov," ",measure," ; Skipping"))
  }
} # For each variable-measure...