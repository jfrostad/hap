# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 03/21/2020
# Purpose: Launch parallel jobs
#source('/homes/jfrostad/_code/lbd/hap/post_estimation/test/1_launcher.R') 
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

# set structural arguments
user            <- Sys.info()['user']
repo            <- file.path('/homes', user, 'dir') # TODO put the repo where you have stored this code here
script          <- file.path(repo, '2_rocket.R')

# Load MBG packages and functions
# message('Loading in required R packages and MBG functions')
# package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
# source(paste0(repo, '/mbg_central/setup.R'))
# mbg_setup(package_list = package_list, repos = repo)

# set cluster options
use_geos_nodes  <- T
proj_arg        <- ifelse(use_geos_nodes, 'proj_geo_nodes', 'proj_geospatial_dia')
proj            <- ifelse(use_geos_nodes, paste0(' -P ', proj_arg, ' -l gn=TRUE '), paste0(' -P ', proj_arg, ' '))
jmem            <- '50G' #note this may need to change based on your selected parameter

# set analysis options
option_1 <- TRUE

# list iterations
parameters <- c('x',
                'y',
                'z')

#***********************************************************************************************************************

# ----LAUNCH------------------------------------------------------------------------------------------------------------

for (param in parameters) {

      #name job
      jname           <- paste0('tap_', region)
      
      # set up qsub
      sys.sub <- paste0('qsub -e /share/temp/sgeoutput/', user,'/errors -o /share/temp/sgeoutput/', user, '/output ', 
                        '-l m_mem_free=', jmem, ' -P ', proj_arg, ifelse(use_geos_nodes, ' -q geospatial.q ', ' -q all.q '),
                        '-l fthread=1 -l h_rt=16:00:00:00 -v sing_image=default -N ', jname, ' -l archive=TRUE ')
      r_shell <- file.path(repo, 'mbg_central/share_scripts/shell_sing.sh')
      args <- paste(param, option_1)
      
      # run launch script
      paste(sys.sub, r_shell, script, args) %>% 
        system

}

#***********************************************************************************************************************