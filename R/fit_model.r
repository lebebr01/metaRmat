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
