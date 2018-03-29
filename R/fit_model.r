#' Fit meta analysis models with metafor
#'
#' This function is a wrapper that calls metafor to fit models.
#'
#' @param data
#' @param effect_size
#' @param var_cor
#' @param weights
#' @param moderators
#' @param random_params
#' @param structure
#' @param test
#' @param intercept
#' @param estimation_method
#' @param ... Additional parameters passed to metafor. See \code{\link{rma.mv}}
#'   for more details.
#' @importFrom metafor rma.mv
#'
#' @export
fit_model <- function(data, effect_size, var_cor, weights = NULL,
                      moderators = NULL, random_params = NULL,
                      structure = 'CS', test = 't',
                      intercept = FALSE, estimation_method = 'REML', ...){

  effect_size <- data[[effect_size]]

  if(is.null(random_params)) {
    metafor::rma.mv(yi = effect_size, V = var_cor, W = weights,
                    mods = moderators, data = data,
                    struct = structure, intercept = intercept,
                    test = test, method = estimation_method, ...)
  } else {
    metafor::rma.mv(yi = effect_size, V = var_cor, W = weights,
                    mods = moderators, random = random_params,
                    data = data, struct = structure, intercept = intercept,
                    test = test, method = estimation_method, ...)
  }

}


#' Path Model Function
#'
#' This function fits the path model and returns adjusted standard errors.
#'
#' @param model This is model syntax specified in the format by lavaan. See
#'    \code{\link{sem}} for more details about model syntax.
#' @param data A list that contains the correlation matrix for model fitting
#'   and the variance matrix. This would most likely come from the
#'   \code{\link{extract_model}} function.
#' @param num_obs Number of observations
#'
#' @importFrom lavaan sem
#'
#' @export
#'
path_model <- function(model, data, num_obs) {

  fitted_model <- lavaan::sem(model, sample.cov = data[['beta_matrix']],
                              sample.nobs = num_obs)

  fitted_model

}
