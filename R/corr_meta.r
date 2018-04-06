#' Master function
#'
#' This function does all of the steps in a single unified function.
#'
#' @param data Raw data input
#' @param n Sample size
#' @param type type of adjustment (olkin siotani)
#' @param missing What to do with missing data
#' @param variable_names An optional character vector specifying variable names
#'   of original data. This is mostly used to pass to \code{\link{df_to_corr}}.
#' @param ID A variable name, as a character string, to use as names for the list elements.
#' @param effect_size
#' @param var_cor
#' @param weights
#' @param moderators
#' @param random_params
#' @param structure
#' @param test
#' @param intercept
#' @param estimation_method
#' @param model This is model syntax specified in the format by lavaan. See
#'    \code{\link{sem}} for more details about model syntax.
#' @param num_obs Number of observations
#' @param ... Additional parameters passed to metafor. See \code{\link{rma.mv}}
#'   for more details.
#'
#' @export
corr_meta <- function(data, n, type = c('average','weighted', 'simple'),
                      missing = 'remove', variable_names = NULL, ID = NULL,
                      effect_size, var_cor, weights = NULL,
                      moderators = NULL, random_params = NULL,
                      structure = 'CS', test = 't',
                      intercept = FALSE, estimation_method = 'REML',
                      model = NULL, num_obs = NULL, ...) {

  input_metafor <- prep_data(data, n, type = type,
                             variable_names = variable_names, ID = ID)

  metafor_model <- fit_model(data = input_metafor$data, effect_size = effect_size,
                            var_cor = input_metafor$V, moderators = moderators,
                            random_params = random_params,
                            structure = structure, test = test,
                            intercept = intercept,
                            estimation_method = estimation_method, ...)

  metafor_results <- extract_model(metafor_model,
                                   variable_names = variable_names)

  lavaan_output <- path_model(data = model_out_random, model = model,
                              num_obs = num_obs)

  updated_se <- var_path(metafor_results[['beta_matrix']],
                         metafor_results[['var_matrix']])

}
