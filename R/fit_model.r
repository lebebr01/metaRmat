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

  raw_data <- data[['data']]
  effect_size <- raw_data[[effect_size]]

  variance_covariance <- data[[var_cor]]

  if(is.null(random_params)) {
    metafor::rma.mv(yi = effect_size, V = variance_covariance, W = weights,
                    mods = moderators, data = raw_data,
                    struct = structure, intercept = intercept,
                    test = test, method = estimation_method, ...)
  } else {
    metafor::rma.mv(yi = effect_size, V = variance_covariance, W = weights,
                    mods = moderators, random = random_params,
                    data = raw_data, struct = structure, intercept = intercept,
                    test = test, method = estimation_method, ...)
  }

}


#' Path Model Function
#'
#' This function fits the path model and returns adjusted standard errors.
#'
#' @param data A list that contains the correlation matrix for model fitting
#'   and the variance matrix. This would most likely come from the
#'   \code{\link{extract_model}} function.
#' @param model This is model syntax specified in the format by lavaan. See
#'    \code{\link{sem}} for more details about model syntax.
#' @param num_obs Number of observations
#' @param adjust_se Adjust the standard errors to reflect the ...
#'
#' @importFrom lavaan sem parTable fitmeasures
#'
#' @export
#'
path_model <- function(data, model, num_obs, adjust_se = TRUE) {

  fitted_model <- lavaan::sem(model, sample.cov = data[['beta_matrix']],
                              sample.nobs = num_obs)

  if(adjust_se) {
    computed_se <- var_path(data[['beta_matrix']],
                            data[['var_matrix']])
  } else {
    computed_se <- NULL
  }

  model_output <- list(parameter_estimates = lavaan::parTable(fitted_model),
                       fit_measures = fitmeasures(fitted_model),
                       computed_se = computed_se)

  class(model_output) <- 'corrMeta'

  model_output


}
