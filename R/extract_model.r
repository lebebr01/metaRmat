#' Extract relavent model values from metafor
#'
#' @param model A metafor model object, likely coming from
#'   \code{\link{fit_model}}.
#'
#' @export
extract_model <- function(model) {

  beta_vector <- model[['b']]
  beta_matrix <- as.matrix(beta_vector)

  B <- vec2sm(beta_matrix, diag = FALSE, order = NULL)

  diag(B) <- 1


  V <-  model[['vb']]

  tau2 <-  model[['tau2']]
  rho <-  model[['rho']]
  # tau_mat <- tau_matrix(rho, tau2)

  list(beta_matrix = B,
       var_matrix = V,
       tau = tau2,
       rho = rho)
}
