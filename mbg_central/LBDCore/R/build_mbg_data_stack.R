#' @title Build an INLA data stack for modeling
#'
#' @description Generates an INLA stack that contains information
#'   mapping data to various INLA effects (GP, random effects, ...)
#'
#' @param df data.frame/table containing observational data. must have
#'   latitude and longitude columns as well as extracted values for
#'   all covariates that will be used in modeling
#'
#' @param fixed_effects string containing fixed effects used in
#'   res_fit separated by " + "
#'
#' @param mesh_s An 'inla.mesh' object used to fit the spatial SPDE GP
#'   approximation in res_fit
#'
#' @param mesh_t An 'inla.mesh' object used to fit the temporal
#'   correlation structure in res_fit
#'
#' @param exclude_cs vector of strings detailing covariates that are
#'   not center-scaled
#'
#' @param usematernnew Logical. if TRUE, used penalized complexity
#'   priors. Not currently suggested...
#'
#' @param sig0 if usematernnew, median prior value for spde rho
#'
#' @param rho0 if usematernnew, median prior value for spde sigma
#'
#' @param coefs.sum1 Logical. If TRUE, add a constraint to ensure
#'   covariate coefs sum to 1 in fitted model
#'
#' @param use_ctry_res Logical. If TRUE, include country random
#'   effects
#'
#' @param use_subnat_res Logical. If TRUE, include subnational (admin-1) random
#'   effects
#'
#' @param remove_non_subnats Logical. If TRUE, NA out all points of the subnational
#'   random effect which are not associated with the country
#'
#' @param use_nugget Logical. If TRUE, include a nugget effect
#'
#' @param stacker_names string vector of child model names
#'
#' @param yl numeric vector of years in model
#'
#' @param zl numeric vector of third dimension in kronecker
#'   product. Only implemented in TMB
#'
#' @param zcol column name of z values associated with zl. Only
#'   implemented in TMB
#'
#' @param scale_gaussian_variance_N Logical. Do you want to scale
#'   gaussian variance by sample size? Only implemented in TMB.
#'
#' @param tmb Logical. use tmb?
#'
#' @return List containing 1) 'inla.stack' object, 2) 'inla.spde'
#'   object, 3) cs_df: a center-scale dataframe containing info on
#'   mean and SD used to scale covaraites in design matrix
#'
#' @export
build_mbg_data_stack <- function(df,
                                 fixed_effects,
                                 mesh_s,
                                 mesh_t,
                                 exclude_cs = "",
                                 usematernnew = F,
                                 sig0 = 0.5,
                                 rho0 = 0.3,
                                 coefs.sum1 = FALSE,
                                 use_ctry_res = FALSE,
                                 use_subnat_res = FALSE,
                                 remove_non_subnats = FALSE,
                                 use_nugget = FALSE,
                                 stacker_names = child_model_names,
                                 yl = year_list,
                                 zl = z_list,
                                 zcol = zcol,
                                 scale_gaussian_variance_N = TRUE,
                                 tmb = FALSE,
                                 shapefile_version = "current") {
  if (nchar(stacker_names[1]) == 0 & coefs.sum1 == TRUE) {
    message("WARNING! You've chosen sum-to-1 but don't appear to be using any stackers. Unless you have a very good reason to do this, it probably doesn't make sense. As such, we're setting coefs.sum1 <- FALSE")
    coefs.sum1 <- FALSE
  }

  # if fitting with tmb, punt this over to
  if (tmb == TRUE) {
    message("Returning a TMB model stack.")
    return(
      build_mbg_data_stack_tmb(
        d = df,
        yl = yl, # year list
        fes = fixed_effects, # fixed effects in the model
        indic = indicator, # indicator
        exclude_cs = exclude_cs,
        nugget = use_nugget,
        country_re = use_ctry_res,
        nid_re = use_nid_res,
        zl = zl,
        zcol = zcol,
        shapefile_version = shapefile_version,
        scale_gaussian_variance_N = scale_gaussian_variance_N,
        mesh = mesh_s
      )
    ) # spatial mesh

    # else do the inla version
  } else {


    # construct an SPDE model with a Matern kernel
    message("Building SPDE...")
    if (usematernnew) {
      # rho0 is typical range, sig0 typical sd
      spde <- local.inla.spde2.matern.new(
        mesh = mesh_s,
        prior.pc.rho = c(rho0, 0.5),
        prior.pc.sig = c(sig0, 0.5),
        alpha = 2
      )
    } else {
      spde <- inla.spde2.matern(mesh = mesh_s, alpha = 2)
    }

    ## Build projector matrix between data locs and spatial mesh
    data.locs <- as.matrix(df[, c("longitude", "latitude"), with = F])
    if (mesh_s$manifold == "S2") {
      ## then the mesh is on the sphere and we need to use 3d coords
      data.locs <- lonlat3D(data.locs[, 1], data.locs[, 2])
    }
    ## here we actually build the projector matrix, A
    A <- inla.spde.make.A(
      mesh = mesh_s,
      loc = data.locs,
      group = df$period,
      group.mesh = mesh_t
    )

    if (coefs.sum1) {
      ## make A matrix comprised of covariate fixed_effects column vectors
      f.e.v <- stacker_names ## fixed eff. vec.
      A.covar <- as.matrix(df[, f.e.v, with = FALSE])
    }

    if (is.null(mesh_t)) {
      space <- inla.spde.make.index("space",
        n.spde = spde$n.spde
      )
    } else {
      space <- inla.spde.make.index("space",
        n.spde = spde$n.spde,
        n.group = mesh_t$m
      )
    }


    # find cov indices
    if (fixed_effects != "NONE" & nchar(fixed_effects) > 0) {
      f_lin <- reformulate(fixed_effects)
      message("Indexing covariates...")
      covs_indices <- unique(c(match(all.vars(f_lin), colnames(df))))

      # make design matrix, center the scaling
      design_matrix <- data.frame(
        int = 1,
        df[, covs_indices, with = F]
      )


      cs_df <- getCentreScale(design_matrix, exclude = c("int", exclude_cs))


      design_matrix <- centreScale(design_matrix,
        df = cs_df
      )
    } else {
      design_matrix <- data.frame(int = rep(1, nrow(df)))
      cs_df <- getCentreScale(design_matrix, exclude = c("int", "rates"))
    }

    # construct a 'stack' object for observed data
    cov <- df[[indicator]] # N+_i
    N <- df$N # N_i

    if (use_ctry_res) {
      ## add an numeric gaul code to use in random effects
      design_matrix$CTRY.ID <- gaul_convert(df$country, shapefile_version = shapefile_version)
    }

    if (use_subnat_res) {
      ## add subnat ID to use in random effects
      design_matrix$SUBNAT.ID <- df$ADM1_CODE

      if (remove_non_subnats) {
        ## NA out subnational rows which are 0
        design_matrix$SUBNAT.ID[design_matrix$SUBNAT.ID == 0] <- NA
      }
    }

    if (use_nugget) {
      design_matrix$IID.ID <- 1:nrow(design_matrix)
    }

    message("Stacking data...")
    if (coefs.sum1 == TRUE & nchar(fixed_effects) > 1) {
      ## build in covar effect to be used in sum1 constraint
      stack.obs <- inla.stack(
        data = list(covered = cov),
        A = list(A, 1, A.covar),
        effects = list(
          space,
          design_matrix,
          list(covar = 1:ncol(A.covar))
        ),
        tag = "est"
      )
    } else {
      stack.obs <- inla.stack(
        data = list(covered = cov),
        A = list(A, 1),
        effects = list(
          space,
          design_matrix
        ),
        tag = "est"
      )
    }

    return_list <- list(stack.obs, spde, cs_df)

    return(return_list)
  }
}
