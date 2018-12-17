# Tests for get_admin_shapefile.
#
# Tests written for Hadley Wickham's "assertthat" package.
#
# It is expected that these will transition to use the "testthat" style of testing described here
# http://r-pkgs.had.co.nz/tests.html
# once this code is migrated to a true package.

# Note: these only work if run on a machine with access to the cluster drive /snfs1
test_get_admin_shapefile <- function() {
    # Sanity check - are we connected to /snfs1?
    assert_that(dir.exists("/snfs1/WORK/11_geospatial/admin_shapefiles"),
                            msg = "Not connected to /snfs1 - cannot test")
    # Basic use
    a0 <- get_admin_shapefile(0)
    assert_that(are_equal(a0, "/snfs1/WORK/11_geospatial/admin_shapefiles/current/lbd_standard_admin_0.shp"))
    a1 <- get_admin_shapefile(1)
    assert_that(are_equal(a1, "/snfs1/WORK/11_geospatial/admin_shapefiles/current/lbd_standard_admin_1.shp"))
    a2 <- get_admin_shapefile(2)
    assert_that(are_equal(a2, "/snfs1/WORK/11_geospatial/admin_shapefiles/current/lbd_standard_admin_2.shp"))

    # Use `suffix` argument to get e.g., the DBF file
    a1_dbf = get_admin_shapefile(1, suffix=".dbf")
    assert_that(are_equal(a1_dbf, "/snfs1/WORK/11_geospatial/admin_shapefiles/current/lbd_standard_admin_1.dbf"))

    # Use `type` argument to get raking or disputed files
    raking <- get_admin_shapefile(type="raking")
    assert_that(are_equal(raking, "/snfs1/WORK/11_geospatial/admin_shapefiles/current/lbd_standard_raking.shp"))
    disputed <- get_admin_shapefile(type="disputed_mask")
    assert_that(are_equal(disputed, "/snfs1/WORK/11_geospatial/admin_shapefiles/current/lbd_disputed_mask.shp"))

    # Use `version` argument to specify non-current
    old_admin1 <- get_admin_shapefile(1, version='2018_07_17')
    assert_that(are_equal(old_admin1, "/snfs1/WORK/11_geospatial/admin_shapefiles/2018_07_17/lbd_standard_admin_1.shp"))

    # Defaults to admin 0
    a0_noargs = get_admin_shapefile()
    assert_that(are_equal(a0_noargs, a0))

    # Backwards compatibility - support valid admin level + raking = T; return raking shapefile
    raking_compat = get_admin_shapefile(1, raking=TRUE)
    assert_that(are_equal(raking_compat, raking))

    # Fail for admin values > 2 (the file does not exist)
    a3 <- try(get_admin_shapefile(3), silent=TRUE)
    assert_that(is.error(a3))

    # Fail when extension is provided that has no related file
    bad_suffix = try(get_admin_shapefile(suffix=".wat"), silent=TRUE)
    assert_that(is.error(bad_suffix))

    # Fail when type is not "admin", "raking", or "disputed_mask"
    bad_type = try(get_admin_shapefile(type="secret_shapes"), silent=TRUE)
    assert_that(is.error(bad_type))

    # Fail when version does not exist
    bad_version = try(get_admin_shapefile(version="2001_01_01"), silent=TRUE)
    assert_that(is.error(bad_version))
}
