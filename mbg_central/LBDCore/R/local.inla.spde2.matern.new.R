#' @title SPDE priors function
#' @description FUNCTION_DESCRIPTION
#' @param mesh PARAM_DESCRIPTION
#' @param alpha PARAM_DESCRIPTION, Default: 2
#' @param prior.pc.rho PARAM_DESCRIPTION
#' @param prior.pc.sig PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[INLA]{character(0)}}
#' @rdname local.inla.spde2.matern.new
#' @export
local.inla.spde2.matern.new <- function(mesh, alpha = 2, prior.pc.rho,
                                        prior.pc.sig) {
  # Call inla.spde2.matern with range and standard deviationparametrization
  d <- INLA:::inla.ifelse(inherits(mesh, "inla.mesh"), 2, 1)
  nu <- alpha - d / 2
  kappa0 <- log(8 * nu) / 2
  tau0 <- 0.5 * (lgamma(nu) - lgamma(nu + d / 2) - d / 2 * log(4 * pi)) - nu * kappa0
  spde <- inla.spde2.matern(
    mesh = mesh,
    B.tau = cbind(tau0, nu, -1),
    B.kappa = cbind(kappa0, -1, 0)
  )

  # Change prior information
  param <- c(prior.pc.rho, prior.pc.sig)
  spde$f$hyper.default$theta1$prior <- "pcspdega"
  spde$f$hyper.default$theta1$param <- param
  spde$f$hyper.default$theta1$initial <- log(prior.pc.rho[1]) + 1
  spde$f$hyper.default$theta2$initial <- log(prior.pc.sig[1]) - 1

  # End and return
  return(invisible(spde))
}
