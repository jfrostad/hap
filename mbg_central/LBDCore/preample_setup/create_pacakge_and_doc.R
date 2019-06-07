
#######################################################
## Building package and documentation for LBDCore
#######################################################

#######################################################
## (1) Build and install LBD
#######################################################

## Path setup
rm(list=ls())
personal_lib <- ifelse(grepl('health_fin', Sys.getenv("SINGULARITY_NAME")) ,  
                       "~/R/x86_64-pc-linux-gnu-library/3.6/", 
                       "~/R/x86_64-pc-linux-gnu-library/3.6geo/")
if(!dir.exists(personal_lib)) dir.create(personal_lib, recursive = TRUE)
Sys.setenv(R_LIBS_USER = personal_lib)
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()) )

## Set up MKLROOT
Sys.setenv(MKLROOT = "/opt/intel/compilers_and_libraries/linux/mkl")

## Get libs
# library(styler)


## Build and install LBDCore
# roxygen2::roxygenise('/share/code/geospatial/sadatnfs/lbd_core/mbg_central/LBDCore/')
system("rm /share/code/geospatial/sadatnfs/lbd_core/mbg_central/LBDCore/*.so /share/code/geospatial/sadatnfs/lbd_core/mbg_central/LBDCore/*.o /share/code/geospatial/sadatnfs/lbd_core/mbg_central/LBDCore/*.gcda")
system("rm -rf /share/code/geospatial/sadatnfs/lbd_core/mbg_central/LBDCore/docs")
system(paste0("rm -rf ", personal_lib, "LBDCore ", personal_lib, "00*"))

devtools::document('/share/code/geospatial/sadatnfs/lbd_core/mbg_central/LBDCore/')
devtools::install('/share/code/geospatial/sadatnfs/lbd_core/mbg_central/LBDCore/', dependencies = FALSE, upgrade = FALSE)



#######################################################
## (2) Build static docs
#######################################################

library(LBDCore)
library(stringr)
library(pkgdown)

# ## Style everything inside so that we can grep all the functions easily
# # style_dir('/share/code/geospatial/sadatnfs/lbdcore_pacakge/mbg_central/R_funk/modified_base_funks/')
# 
# ## Get all functions from our base funks
# all_base_functions <- list.files('/share/code/geospatial/sadatnfs/lbdcore_pacakge/mbg_central/R_funk/modified_base_funks/')
# 
# functions_in_LBDCore <- lapply(all_base_functions, function(bb) {
#   sort(unique(c((sort(gsub("#|\"|\\ ", "", system(paste0('find /share/code/geospatial/sadatnfs/lbdcore_pacakge/mbg_central/R_funk/modified_base_funks/', bb ,
#                                 ' -name "*.R" -exec grep -hiC 0 "<- function(" {} \\; | grep -o -E ".{0,99} <-{0,0}" | sed "s/<//g" | sed "s/ //g"'), 
#                          intern = TRUE)  ))),
#   
#   (sort(gsub("#|\"|\\ ", "", system(paste0('find /share/code/geospatial/sadatnfs/lbdcore_pacakge/mbg_central/R_funk/modified_base_funks/', bb ,
#                                                  ' -name "*.R" -exec grep -hiC 0 "<-function(" {} \\; | grep -o -E ".{0,99} <-{0,0}" | sed "s/<//g" | sed "s/ //g"'), 
#                                           intern = TRUE)  ))),
#   
#   (sort(gsub("#|\"|\\ ", "", system(paste0('find /share/code/geospatial/sadatnfs/lbdcore_pacakge/mbg_central/R_funk/modified_base_funks/', bb ,
#                                                  ' -name "*.R" -exec grep -hiC 0 "<-function (" {} \\; | grep -o -E ".{0,99} <-{0,0}" | sed "s/<//g" | sed "s/ //g"'), 
#                                           intern = TRUE)  ))),
#   
#   (sort(gsub("#|\"|\\ ", "", system(paste0('find /share/code/geospatial/sadatnfs/lbdcore_pacakge/mbg_central/R_funk/modified_base_funks/', bb ,
#                                                  ' -name "*.R" -exec grep -hiC 0 "<- function (" {} \\; | grep -o -E ".{0,99} <-{0,0}" | sed "s/<//g" | sed "s/ //g"'), 
#                                           intern = TRUE)  ))))
#   ))
#   
# })
# 
# names(functions_in_LBDCore) <- gsub(".R", "", all_base_functions)
# 
# all_funks <- unlist(functions_in_LBDCore, use.names = F)
# 
# ## Check all functions from the package
# package_funks2 <- sort(gsub("`", "", template_reference()[[1]][[1]]$contents))
# package_funks <- sort(ls('package:LBDCore'))
# 
# ## See the diffs
# setdiff(package_funks2, package_funks)
# setdiff(package_funks, package_funks2)
# 
# ### Exceptions
# # "format_plot_obj_scatter" "g_legend" "get_legend" can all go into visualization
# # points.quadtree and points.quadtree.leaf are part of holdout
# 
# ## Baddies: Topics missing from index: archive__make_population_weights, clip, dircos, 
# ## lstype, make_country_list_allAdmins, must_haves, notMissingIdx, pullupstream, run_child_stackers, waitformodelstofinishu5m
# 
# for(xx in sort(names(functions_in_LBDCore))) {
#   cat(gsub("_", " ", paste0('- title: ',xx, '\n')))
#   for(yy in functions_in_LBDCore[[xx]]) {
#     cat(paste0('  - \'`', yy, '`\'\n'))  }
# }
# 
# 
# 
# 
# ## Build static HTML dox
# pkgdown::build_site(pkg = "/share/code/geospatial/sadatnfs/lbd_core/mbg_central/LBDCore", preview = FALSE, examples = TRUE, document = FALSE)
#
#
# ## Copy to my sandbox
# system("rm -rf /ihme/homes/sadatnfs/htdocs/LBDCore")
# system("cp -rf /share/code/geospatial/sadatnfs/lbd_core/mbg_central/LBDCore/docs /ihme/homes/sadatnfs/htdocs/LBDCore")
# system("chmod 755 -R /ihme/homes/sadatnfs/htdocs/LBDCore")
