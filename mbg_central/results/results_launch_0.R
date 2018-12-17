#####################################################################
## Author: Rebecca Stubbs
## Date: August 11, 2017
##
## Purpose: Qsub out the jobs for generating tables by admin unit for
## each indicator, using a .csv as your reference document.
##
## NOTE: Run this code from the mbg_central directory. FOR EXAMPLE:
#
# cd /share/code/geospatial/stubbsrw/mbg/mbg_central
# git pull origin cgf_stubbsrw
# qsub -cwd -P proj_geo_nodes -l geos_node=TRUE -N results_launch -e /homes/stubbsrw/qsub_logs/errors -o /homes/stubbsrw/qsub_logs/output /share/code/geospatial/stubbsrw/mbg/child_growth_failure/r_shell_geos.sh ./results/results_launch_0.R child_growth_failure
#####################################################################

# FILES REQUIRED TO RUN THIS CODE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' This code submits jobs (01_collapse_draws_by_admin.R) for each indicator-run-date
#' based on a .csv that defines the following columns:
#' ``````````````````````````````````````````````````````````````````````````````````
#' indicator_group       The string name of the indicator group-- note that this means
#'                       you can compile estimates from multiple indicator groups, if
#'                       given a valid run date, etc. 
#'                 
#' indicator_name        The string name that defines the indicator
#' 
#' date_string           The string that defines the run date you are interested in.
#' 
#' raked                 Whether you want the raked, or unraked version of that indicator
#' 
#' indicator_longname    The longer, "prettier" name used to differentiate the indicators
#' 
#' final_set             Boolean (T or F, 1 or 0) for indicators that should be included in the "final set" of 
#'                       results (unraked versions of variables that also have raked versions
#'                       would probably be a 0 here, for example). This defines what variables
#'                       show up in comparison graphs, etc. 
#'
#' pop_measure           The population measurement that serves as your denominator (most commonly,
#'                       this will be a0004t, or age groups 00 (birth) to 04, inclusive (total
#'                       population)-- aka, the under-5 population. 
#'
#' overwrite             Boolean (T or F); defines whether results sets that have already
#'                       been generated (a file with results output already exists) will be 
#'                       overwritten. If F, the code will skip those indicators that already have qd
#'                       output files.
#'
#' shapefile_version     String indicatig version of shapefile to pull
#'
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' SCRIPTS THIS CODE KICKS OFF:
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' results_aggregate_1.R
#' --------------------------
#' Collapses cell-prediction (at the draw level) into admin 0, 1, and 2 levels, weighted by population in each pixel
#' 
#' results_summarize_2.R
#' --------------------------
#' Collapses admin-level draws into summary statistics (such as mean, upper, and lower confidence bounds), compares 
#' estimates across time (for confidence intervals of % change from 1 time period to another)
#' 
#' results_maps.R
#' --------------------------
#' Generates quick and simple maps of model results in main 5 years (2000,2005,2010,2015)
#' 
#' graphs_and_numbers_master_script.R
#' ------------------------------------
#' A team-specific script that produces the numbers and figures required for results pulling and exploration
#' 
#' Note that more scripts could be added to this submit script, based on the other results you want to run.

# Prepare Workspace
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if(!dir.exists(paste0("/share/code/geospatial/",Sys.getenv("LOGNAME"),"/mbg/mbg_central/"))){stop("Hmm. This script is set up such that your gitRepo on the cluster is structured in a certain way (see this file's source code).")}

  # Setting the root, loading in libraries, and functions:
    setwd(paste0("/share/code/geospatial/",Sys.getenv("LOGNAME"),"/mbg/mbg_central/"))
    for(function_script in list.files(getwd(),pattern="*_functions.R")){message(function_script);source(function_script)};message("Central Functions Loaded.")
    load_libs(c('data.table','rmarkdown'))

    indicator_group<-commandArgs()[4]; message(indicator_group)
    
    # testing
    #indicator_group<-"child_growth_failure"
    
  # Ensure that the results_config exists, and if it doesn't, create it (and the directory, if necessary):
    if(!dir.exists(paste0("/share/geospatial/mbg/",indicator_group,"/results/"))){
      # Create the directory
      dir.create(paste0("/share/geospatial/mbg/",indicator_group,"/results/"))
      message("There is now a directory in J work at the following destination: ")
      message(paste0("/share/geospatial/mbg/",indicator_group,"/results/"))
      message("This is where you should store the results_config.csv for your indicator group.")
    }
    
    if(!file.exists(paste0("/share/geospatial/mbg/",indicator_group,"/results/results_config.csv"))){
      template<-data.table(indicator_group=character(0),
                 indicator_name=character(0),
                 date_string=character(0),
                 raked=character(0),
                 indicator_category=character(0),
                 indicator_longname=character(0),
                 final_set=character(0),
                 pop_measure=character(0),
                 age=character(0),
                 holdout=character(0),
                 overwrite=character(0),
                 shapefile_version = character(0))
      write.csv(template,file=paste0("/share/geospatial/mbg/",indicator_group,"/results/results_config.csv"),row.names=F)
      stop(paste0("There is now a .csv at /share/geospatial/mbg/",indicator_group,"/results/results_config.csv, 
              with the fields you need to fill out. See documentation in this script's source code for what each field needs to contain."))
    }
    
  # Determining a "run time" for this batch of results-pulling, that will preserve the circumstances that went into shaping this batch of results.
    results_pull_time<-make_time_stamp(T)

  # Loading in the csv with the run dates and indicators you are interested in running.  
    results<-fread(paste0("/share/geospatial/mbg/",indicator_group,"/results/results_config.csv"))
    results[,admin_draws_path:=paste0("/share/geospatial/mbg/",indicator_group,"/",indicator_name,"/output/",date_string,"/",indicator_name,"_admin_draws",
                                      ifelse(raked,"_raked.Rdata",".RData"))]
    
  # Creating a directory for the results to go into, writing the .csv of the results config to that file for safe-keeping.
    dir.create(paste0("/share/geospatial/mbg/",indicator_group,"/results/results_",results_pull_time),recursive=T,showWarnings = F)
    write.csv(results,file=paste0("/share/geospatial/mbg/",indicator_group,"/results/results_",results_pull_time,"/results_config.csv"),row.names=F)

# Submit jobs to the cluster, for each of the results you are interested in:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  shell_script<-ifelse(grepl("geos", Sys.info()[4]), "r_shell_geos.sh", "r_shell.sh") # This depends on there being a r geos shell and a normal shell script in the mbg_central directory.
    
  # Creating output directories for errors and warnings, if they don't already exist:
   output_dir<-paste0("/homes/",Sys.getenv("LOGNAME"),"/qsub_logs/errors/","results_",results_pull_time)
   error_dir<-paste0("/homes/",Sys.getenv("LOGNAME"),"/qsub_logs/output/","results_",results_pull_time)
   dir.create(path=output_dir,recursive=T,showWarnings=F)
   dir.create(path=error_dir,recursive=T,showWarnings=F)
   message(paste0("Using the following paths for errors: ",error_dir," and output: ",output_dir))

  nslots<-10 # Number of slots you want to use for this task
    for (i in seq(1,nrow(results))){
      qsub <- paste("qsub -cwd",
                    "-P proj_geo_nodes -l geos_node=TRUE -pe multi_slot ", nslots,
                    "-N", paste0("agg_",results$indicator_name[i],ifelse(results$raked[i],"rkd_","unrkd_")),
                    paste0(" -e ",error_dir," -o ",output_dir," "),
                    shell_script," ./results/results_aggregate_1.R", results$indicator_group[i], results$indicator_name[i],results$date_string[i], results$raked[i],results$pop_measure[i],results$overwrite[i],
                    results$age[i], results$holdout[i], results$shapefile_version[i])
      system(qsub)
      message(qsub)
      message("-------------  ^ Qsubbed ---------------")
    }
  
  
# Make some maps of the results:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq(1,nrow(results))){
    qsub <- paste("qsub -cwd",
                  "-P proj_geo_nodes -l geos_node=TRUE -pe multi_slot ", 1,
                  "-N", paste0("map_",i,"_",ifelse(results$raked[i],"rkd_","unrkd_"),results$indicator_name[i]),
                  paste0(" -e ",error_dir," -o ",output_dir," "),
                  shell_script," ./results/results_maps.R", results$indicator_group[i], results$indicator_name[i], results$date_string[i], results$raked[i],results_pull_time)
    system(qsub)
    message(qsub)
    message("-------------  ^ Qsubbed ---------------")
  }
  
# Wait for the files to be written, then kick off the job for summarizing the results into confidence intervals:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  files_generated<-0 # Starting this off at a non-zero value, this will get overwritten
  while (files_generated!=nrow(results)) { # Checking every 10 seconds to see if the file sizes are changing
    Sys.sleep(10)
    files_generated<-sum(file.exists(results$admin_draws_path))
  }
  message("All of the admin draw files seem to exist, but they may still be getting written to-- we are going to wait 1 minute and make sure the file sizes haven't changed.")
  
   if(sum(file.exists(results$admin_draws_path))==nrow(results)){
     size_differences<-5 # Starting this off at a non-zero value, this will get overwritten during each loop.
       while (size_differences != 0) { # Checking every 10 seconds to see if the file sizes are changing
       sizes<-file.size(results$admin_draws_path)
       Sys.sleep(10)#waiting 60 seconds
       size_differences<-sum(sizes-file.size(results$admin_draws_path))
       }
       message("Files do not seem to be growing in size any more (based on 10-second re-recheck intervals), seems safe to proceed! Moving on to collapsing the draws-level measurements down to summary statistics.")
   }
  
  ## Qsubbing out the aggregation/lower/upper/mean estimates:
  nslots<-10
  qsub <- paste("qsub -cwd",
                "-P proj_geo_nodes -l geos_node=TRUE -pe multi_slot ", nslots,
                "-N", "results_summarize",
                paste0(" -e ",error_dir," -o ",output_dir," "),
                shell_script," ./results/results_summarize_2.R", indicator_group, results_pull_time)
  system(qsub)
  message(qsub)
  message("-------------  ^ Qsubbed ---------------")
  
# Run the script that kicks off your graphics, etc
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("Waiting for the time series pdf to exist, which will indicate that the admin files have saved.")
  
  files_generated<-0 # Starting this off at a non-zero value, this will get overwritten
  while (!file.exists(paste0("/share/geospatial/mbg/",indicator_group,"/results/","results_",results_pull_time,"/time_series.pdf"))) { # Checking every 10 seconds to see if the file sizes are changing
    Sys.sleep(10)
    message("waiting...")
  }
  message("The time series plot pdf exists, so we can kick off the rest of the results numbers specific to your indicator.")
  
  if(sum(file.exists(results$admin_draws_path))==nrow(results)){
    size_differences<-5 # Starting this off at a non-zero value, this will get overwritten during each loop.
    while (size_differences != 0) { # Checking every 10 seconds to see if the file sizes are changing
      sizes<-file.size(results$admin_draws_path)
      Sys.sleep(10)#waiting 60 seconds
      size_differences<-sum(sizes-file.size(results$admin_draws_path))
    }
    message("Files do not seem to be growing in size any more (based on 10-second re-recheck intervals), seems safe to proceed! Moving on to collapsing the draws-level measurements down to summary statistics.")
  }
  
  nslots<-5
  qsub <- paste("qsub -cwd",
                "-P proj_geo_nodes -l geos_node=TRUE -pe multi_slot ", nslots,
                "-N", "master_plot_script",
                paste0(" -e ",error_dir," -o ",output_dir," "),
                shell_script,paste0(" ../",indicator_group,"/results/graphs_and_numbers_master_script.R"), indicator_group, results_pull_time)
  system(qsub)
  message(qsub)
  message("-------------  ^ Qsubbed ---------------")
  
  
