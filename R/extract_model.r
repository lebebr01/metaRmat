#' Extract relavent model values from metafor
#'
#' @param model A metafor model object, likely coming from
#'   \code{\link{fit_model}}.
#' @param variable_names A character vector specifying variable names
#'   of original data. This should be the same as passed to \code{\link{prep_data}} function.
#'
#' @export
extract_model <- function(model, variable_names) {

  beta_vector <- model[['b']]
  beta_matrix <- as.matrix(beta_vector)

  B <- corpcor::vec2sm(beta_matrix, diag = FALSE, order = NULL)

  diag(B) <- 1

  column_row_names <- extract_var_names(variable_names)
  rownames(B) <- colnames(B) <- column_row_names


  V <-  model[['vb']]

  tau2 <-  model[['tau2']]
  rho <-  model[['rho']]
  # tau_mat <- tau_matrix(rho, tau2)

  list(beta_matrix = B,
       var_matrix = V,
       tau = tau2,
       rho = rho)
}
