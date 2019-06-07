### onLoad and onAttach functions ###

.onLoad <- function(libname, pkgname) {
  MBG_ROOT <<- "/share/geospatial/mbg"
  SGE_OUTPUT_ROOT <<- "/share/temp/sgeoutput"
  CC_ENV_DIR <<- "/ihme/cc_resources/libraries/gbd_env/r"
  Sys.setenv(MKLROOT = "/opt/intel/compilers_and_libraries/linux/mkl")
  fix_raster_tmpdir()
}
