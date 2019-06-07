
#######################################################
## Building package and documentation for LBDCore
#######################################################

## Install library to temp folder
system("rm -rf /tmp/LBDCore || echo 'Nothing to clean'")
.libPaths(c("/tmp", .libPaths()))
devtools::document('mbg_central/LBDCore/')
devtools::install('mbg_central/LBDCore/', dependencies = FALSE, upgrade = FALSE)

## Build static HTML dox
try(pkgdown::clean_site(pkg = "mbg_central/LBDCore"))
pkgdown::build_site(pkg = "mbg_central/LBDCore", examples = TRUE, document = FALSE)

## Copy to Nafis' sandbox
system("rm -rf /ihme/homes/sadatnfs/htdocs/LBDCore")
system("cp -rf mbg_central/LBDCore/docs /ihme/homes/sadatnfs/htdocs/LBDCore")
system("chmod 777 -R /ihme/homes/sadatnfs/htdocs/LBDCore")
