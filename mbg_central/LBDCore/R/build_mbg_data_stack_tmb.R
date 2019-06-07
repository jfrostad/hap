#' @title Build Stack for TMB MBG model
#' @description Organize Data and Parameter Stacks to fit a TMB MBG model
#' @author Roy Burstein
#'
#' @param d prepped model frame with no NAs
#' @param yl a vector of years for analysis (i.e. c(2001,2002,2003))
#' @param zl a vector of zcol for analysis (i.e. ages c(1,2,3,4,5). Must be integers starting with 1)
#' @param fes a string of model fixed effects of form 'var1 + var2 + var3' or as string vector
#'  corresponding to column names in d
#' @param indic indicator name
#' @param country_re TRUE/FALSE include country re. If true and there is a zcol then there will be random slope as well (todo)
#' @param nid_re TRUE/FALSE include re on survey. Need nid column in the data frame. defaults to use_nid_res from config.
#' @param exclude_cs character vector of covariates to exclude from centrescaling
#' @param indic indicator name, corresponding to the response variable in d
#' @param mesh an inla mesh object. NOTE! this gets remade inside this function... Roy said there was a good reason
#' @param s2mesh Logical. Should the mesh be created on the surface of
#'   a sphere? If TRUE, s2params is used to specify mesh parameters
#'   instead of max_edge and mesh_offset
#' @param s2params string of 3 element numeric vector in R
#'   notation. e.g. "c(25, 500, 1000)". The entries describe the
#'   minimum triangle edge length allowed, hos far to extend the mesh
#'   beyond the 'simple' boundary, and the maximum allowed triangle
#'   edge length, respectively. Units are in kilometers. Used only if
#'   s2mesh=TRUE.
#'
#' @return returns a named list with Data and Parameters to be passed into fit_mbg_tmb()
#'
#' TODO: Allow users to set priors, allow different data model (gaussian to start), country random effects
#'
#' @export
build_mbg_data_stack_tmb <- function(d = df,
                                     yl = year_list,
                                     zl = z_list,
                                     fes = all_fixed_effects,
                                     indic = indicator,
                                     country_re = use_inla_country_res,
                                     nid_re = use_nid_res,
                                     exclude_cs = "",
                                     nugget = FALSE,
                                     zcol = NULL,
                                     shapefile_version = "current",
                                     scale_gaussian_variance_N = TRUE,
                                     mesh = mesh_s) {

  # TODO incorporate an z_list like we do year_list istead of using max(zcol)

  # ensure d is a dt
  d <- setDT(d)

  # zcol
  if (!zcol %in% colnames(d)) {
    message("No Z column detected")
    d[[zcol]] <- 0
  } else if (is.null(zcol)) {
    message("Z column was set as null")
    d[[zcol]] <- 0
  }
  if (!all(unique(d[[zcol]]) %in% zl)) {
    message("WARNING: zl and d[[zcol]] do not completely match up.. ")
    # check that we there arent values in zcol not matching z_list
    d[, dropz := !get(zcol) %in% zl]
    if (any(d$dropz != FALSE)) {
      message(sprintf("WARNING: Detected some z values in zcol (%s) which were not in the z_list", zcol))
      message(sprintf("WARNING: Due to this, dropping %i rows from the input data", sum(d$dropz == TRUE)))
      print(table(d$dropz, d$age))
      d <- subset(d, dropz == FALSE)
      d[, dropz := NULL]
    }
  }

  # make a fake data point with no weight for the max period and Z to fill out the GP
  d <- rbind(d, d[1, ])
  d[[zcol]][nrow(d)] <- max(zl)
  d[["period"]][nrow(d)] <- length(yl)
  d[["weight"]][nrow(d)] <- 0


  # look for z dimension
  num_z <- 1 # TODO get this from the z-list
  if (length(zl) > 1) {
    message(sprintf("More than one unique %s found, initiating Z in the GP", zcol))
    num_z <- length(zl)

    # set A proj grouping. The ordering here must match the ordering of epsilon_stz in the template
    grp <- setDT(expand.grid(1:length(yl), 1:max(zl)))
    setnames(grp, c("Var1", "Var2"), c("period", zcol))
    grp[, group := 1:.N]
    d <- merge(d, grp, by = c("period", zcol), all.x = TRUE) # warning this may re-order things, so do not use d stuff from above
  } else {
    # set Aproj grouping to period if there is but one z value
    d$group <- d$period
  }

  # coordinates at data points. these are passed to TMB in long,lat so
  # we keep them and create another set for 3d coords
  coords <- cbind(d$longitude, d$latitude)

  # remake mesh ## TODO? why remake?
  mesh <- build_space_mesh(
    d = d,
    simple = simple_polygon,
    max_edge = mesh_s_max_edge,
    mesh_offset = mesh_s_offset,
    s2mesh = use_s2_mesh,
    s2params = s2_mesh_params
  )

  # if we have  mesh on s2, first convert coords to spherical to project to mesh
  data.locs <- coords ## long, lat
  if (mesh$manifold == "S2") {
    ## then the mesh is on the sphere and we need to use 3d coords
    data.locs <- lonlat3D(data.locs[, 1], data.locs[, 2])
  }

  # make a projection matrix from data to st mesh
  A.proj <- INLA::inla.spde.make.A(
    mesh = mesh,
    loc = data.locs,
    group = d$group
  )

  # Build SPDE object using INLA (must pass mesh$idx$loc when supplying Boundary) to get matrices
  spde <- INLA::inla.spde2.matern(mesh, alpha = 2)

  # make a clean design matrix. make sure all fes appear in d
  fes <- unlist(strsplit(fes, " \\+ "))
  if (!all(fes %in% names(d))) {
    stop("Check your fes argument, not all covariate names appear in d.")
  }
  if (length(fes) != 0) {
    X_xp <- as.matrix(cbind(int = 1, d[, c(fes), with = FALSE]))
  } else {
    X_xp <- as.matrix(cbind(int = rep(1, nrow(d))))
  }

  # add in age fixed effects
  # TODO eventually do country random slopes for each FE level of Z
  if (num_z > 1) {
    message(sprintf("Adding fixed effects for levels of zcol (%s)", zcol))
    for (z in 2:num_z) {
      X_xp <- cbind(X_xp, d[[zcol]] == z)
    }
    colnames(X_xp) <- c(colnames(X_xp)[colnames(X_xp) != ""], paste0("FE_z_level__", 2:num_z))
    exclude_cs <- c(exclude_cs, paste0("FE_z_level__", 2:num_z))
  }

  # cs_df. imports seegMBG
  cs_df <- getCentreScale(X_xp, exclude = c("int", exclude_cs))
  X_xp <- centreScale(X_xp, df = cs_df)

  # get data range in case we want to clamp for prediction later
  clamper <- data.table(apply(X_xp, 2, range))

  # sort nugget and RE indicators
  nugget <- as.numeric(as.logical(nugget))
  country_re <- as.numeric(as.logical(country_re))
  nid_re <- as.numeric(as.logical(nid_re)) # these do not get used in prediction

  # check there is more than one observed country or nid if REs for those are set
  if (length(unique(d$country)) == 1 & country_re == TRUE) {
    message("WARNING: Only found one unique country in this data frame, so turning off country random effects.")
    country_re <- FALSE
  }
  if (length(unique(d$nid)) == 1 & nid_re == TRUE) {
    message("WARNING: Only found one unique NID in this data frame, so turning off NID random effects.")
    nid_re <- FALSE
  }

  # make a gaul/country/cntry_RE_idx mapping table
  md <- get_location_code_mapping(shapefile_version = shapefile_version)
  cntrys <- unique(as.character(d$country))
  gaulz <- md$GAUL_CODE[md$ihme_lc_id %in% cntrys]
  if (length(cntrys) != length(gaulz)) {
    stop("When making country RE IDs could not get gauls from metadata of each country ISO. ")
  }
  cntry_re_map <- data.table(
    country = cntrys,
    gaul_code = gaulz,
    re_id = 0:(length(cntrys) - 1)
  )
  cntry_re_vec <- cntry_re_map$re_id[match(as.character(d$country), cntry_re_map$country)]

  # make an nid_re mapping table NID NID
  if (use_nid_res) {
    nidz <- unique(as.numeric(d$nid))
    nid_re_map <- data.table(
      nid = nidz,
      re_id = 0:(length(nidz) - 1)
    )
    nid_re_vec <- nid_re_map$re_id[match(as.numeric(d$nid), nid_re_map$nid)]
  } else {
    nidz <- 0
    nid_re_map <- data.table(
      nid = nidz,
      re_id = 0:(length(nidz) - 1)
    )
    nid_re_vec <- rep(0, nrow(df))
  }
  # sort country RE, for now just intercept. later add Random slope by zcol


  # set GP RE array, or matrix depending on if we have a z dimension
  if (num_z > 1) {
    Epsilon_stz <- array(0, dim = c(mesh$n, length(yl), num_z))
  } else {
    Epsilon_stz <- array(0, dim = c(mesh$n, length(yl)))
  }

  # set up vectors of model family
  # look for convention in the data of lik_fam_<<binom,gauss>>, if not there, default to config family
  lik_gaussian <- lik_binomial <- rep(0, nrow(d))
  if (("lik_fam_binom" %in% names(d)) & ("lik_fam_gauss" %in% names(d))) {
    lik_gaussian <- d$lik_fam_gauss
    lik_binomial <- d$lik_fam_binom
    message(sprintf(
      "Found row specific data likelihood indicators, will use those. %i rows binom, %i rows gauss",
      sum(lik_binomial), sum(lik_gaussian)
    ))
  } else {
    if (indicator_family == "binomial") {
      lik_binomial <- rep(1, nrow(d))
      message("Using indicator family binomial for all rows")
    } else if (indicator_family == "gaussian") {
      lik_gaussian <- rep(1, nrow(d))
      message("Using indicator family gaussian for all rows")
    }
  }

  if (any(lik_gaussian + lik_binomial != 1)) {
    stop("Not all rows in your data have been assigned a model (binom or gauss), or some have been assigned multiple!")
  }

  # also look for sd if already exists in the data for the gauss rows to use
  # This is useful for crosswalked values with some data uncertainty, convention is variable named sd_<<INDICATOR>>
  sd_i <- rep(0, nrow(d))
  if (paste0("sd_", indicator) %in% names(d)) {
    message("Found SD estimates to use for crosswalked values.")
    sd_i <- d[[paste0("sd_", indicator)]]
    sd_i[is.na(sd_i)] <- 0
  }


  # run a quick regression to get starting values for the fixed effects
  # This can speed up model fitting if iterations are slow.
  # Note this fit is currently assuming binomial
  print(sum(lik_binomial))
  print(sum(lik_gaussian))
  if (sum(lik_binomial) > 1) {
    message("LM for starting fe vals")
    y <- (d[[indic]][lik_binomial == 1] + .0001) / d$N[lik_binomial == 1]
    y[y <= 0] <- 0.001
    y[y >= 1] <- 0.999
    fe_start <- round(unname(lm(qlogis(y) ~ -1 + X_xp[lik_binomial == 1, ])$coefficients), 4)
  } else {
    message("Default starting fe vals")
    fe_start <- rep(0, ncol(X_xp))
  }
  message(sprintf("starting values for fixed effects: %s", paste0(fe_start, collapse = ", ")))


  # cannot allow a gaussian likelihood to also have a nugget in the linear term, it leads to issues
  if (nugget == 1 & all(lik_gaussian == 1)) {
    message("WARNING:: Nugget in all gaussian model leads to identifiability issues. Removing nugget for you.")
    nugget <- 0
  }


  # check if user wants to scale gaussian variance by N, if not set them all to one in the gaussian rows
  n_i <- d$N
  if (scale_gaussian_variance_N == FALSE & sum(lik_gaussian) > 1) {
    message("Not scaling gaussian error by N since scale_gaussian_variance_N == FALSE.")
    n_i[lik_gaussian == 1] <- 1
  }

  # print some messages for random effectss
  if (nugget == TRUE) message("USING NUGGET (INDIVIDUAL OBSERVATION) RANDOM EFFECTS")
  if (country_re == TRUE) message("USING COUNTRY RANDOM EFFECTS")
  if (nid_re == TRUE) message("USING NID RANDOM EFFECTS")

  # NOTE (RB 2AUG2018) I have concerns about identifiability in situations where there is one survey in a
  #  country and country and nid REs are both turned on
  # identify these obervations and pass them to model so that the model can ignore nid random effects in these situation
  #  another option could be to use the map to set these values to zero in this case. Waiting first to see if this actually causes convergence
  #  issues before dealing with it

  # Construct a list of all Data necessary to TMB to fit
  Data <- list(
    num_i = nrow(d), # Total number of observations
    num_s = mesh$n, # Number of vertices in SPDE mesh
    num_t = length(yl), # Number of periods
    num_z = num_z, # 3rd dimension for GP,
    y_i = d[[indic]], # Number of observed events in the cluster (N+ in binomial likelihood)
    n_i = d$N, # Number of observed exposures in the cluster (N in binomial likelihood)
    t_i = d$period - 1, # Sample period ( starting at zero because C)
    c_re_i = cntry_re_vec, # vector of country ids, ( starting at zero because C)
    nid_re_i = nid_re_vec, # vector of survey ids, ( starting at zero because C)
    w_i = d$weight, # Data weight for each row
    X_ij = X_xp, # Covariate design matrix
    M0 = spde$param.inla$M0, # SPDE sparse matrix
    M1 = spde$param.inla$M1, # SPDE sparse matrix
    M2 = spde$param.inla$M2, # SPDE sparse matrix
    Aproj = A.proj, # mesh to prediction point projection matrix
    lik_gaussian_i = lik_gaussian, # data likelihood for each row
    lik_binomial_i = lik_binomial, # data likelihood for each row
    sd_i = sd_i, # crossalked standard deviation
    options = c(
      1, # option1==1 use priors
      1, # option2==1 ADREPORT off
      nugget, # option3==1 include nugget
      country_re, # option4==1 country random effects
      nid_re
    )
  ) # option5==1 NID random effects
  # option6==1 covariates sum to 1 constraint
  # option7==1 no intercept

  # TODO Allow users to set priors

  # Set staring values for parameters
  Parameters <- list(
    alpha_j = fe_start, # FE parameters alphas
    logtau = -0.5, # Matern/AR tau
    logkappa = -0.5, # Matern Range
    trho = 0.95, # temporal rho
    zrho = 0.95, # 3rd dimension of GP rho (TODO)
    log_nugget_sigma = -1, # log(SD) of the normal nugget term
    log_cre_sigma = -1, # log(SD) of the normal country intercept term (later add slopes as vector)
    log_nidre_sigma = -1, # log(SD) of the normal NID intercept term
    log_gauss_sigma = -1, # log(SD) of normal model
    Epsilon_stz = Epsilon_stz, # Random Effects: GP locations
    nug_i = rep(0, nrow(d)), # Random Effects: Nugget Values
    cntry_re = rep(0, nrow(cntry_re_map)), # Random Effects Values of country random effects (later add in slope stuff)
    nid_re = rep(0, nrow(nid_re_map))
  ) # Random Effects Values of country random effects (later add in slope stuff)


  # put bounds on parameters (Note, this is especially important for rhos)
  L <- c(rep(-10, ncol(X_xp)), -10, -10, -99, -99, -10, -10, -10) ## updated the rho limits from .99999 since I transformed them in the cpp
  U <- c(rep(10, ncol(X_xp)), 10, 10, 99, 99, 10, 10, 10)
  pn <- c(rep("alpha_j", ncol(X_xp)), "logtau", "logkappa", "trho", "zrho", "log_nugget_sigma", "log_cre_sigma", "log_nidre_sigma")
  names(L) <- names(U) <- pn

  # return the list
  return(list(
    Data = Data,
    Parameters = Parameters,
    cs_df = cs_df,
    clamper = clamper,
    coords = coords,
    mesh = mesh,
    cntry_re_map = cntry_re_map,
    nid_re_map = nid_re_map,
    L = L,
    U = U
  ))
}
