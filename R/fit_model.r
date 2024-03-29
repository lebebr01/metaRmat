#' Fit meta analysis models with metafor
#'
#' This function is a wrapper that calls metafor to fit models.
#'
#' @param data Raw data input
#' @param effect_size A vector of observed effect sizes to be modeled with
#'   metafor. See \code{\link{rma.mv}} for more details.
#' @param var_cor Variance covariance matrix of sampling errors or sampling weights
#'   passed on to metafor. See \code{\link{rma.mv}} for more details.
#' @param weights Optional weights matrix passed to metafor. Default is NULL.
#'   See \code{\link{rma.mv}} for more details.
#' @param moderators Moderators (covariates, predictors) to be included in the
#'   analysis with metafor. Default is NULL meaning no predictors.
#'   See \code{\link{rma.mv}} for more details.
#' @param random_params Specification of the random effect structure. This
#'   argument takes the form a single sided formula. Default is NULL which
#'   means a fixed effects model will be fitted with metafor.
#'   See \code{\link{rma.mv}} for more details.
#' @param structure The variance structure passed to metafor. Default is "CS"
#'   which represents compound symmetry. See \code{\link{rma.mv}} for more
#'   details.
#' @param test What type of test statistic should be used.
#'   Default is a z-statistic. See \code{\link{rma.mv}} for more details.
#' @param intercept Should an intercept be included in the metafor model.
#'   See \code{\link{rma.mv}} for more details.
#' @param estimation_method Estimation method to pass on to metafor. Default is
#'   "REML". See \code{\link{rma.mv}} for more details.
#' @param ... Additional parameters passed to metafor. See \code{\link{rma.mv}}
#'   for more details.
#' @importFrom metafor rma.mv
#'
#' @export
fit_model <- function(data, effect_size, var_cor, weights = NULL,
                      moderators = NULL, random_params = NULL,
                      structure = 'UN', test = 'z',
                      intercept = FALSE, estimation_method = 'REML', ...){

  raw_data <- data[['data']]
  effect_size <- raw_data[[effect_size]]

  variance_covariance <- data[[var_cor]]

  if(is.null(weights)) {
    if(is.null(random_params)) {
      metafor::rma.mv(yi = effect_size, V = variance_covariance,
                      mods = moderators, data = raw_data,
                      struct = structure, intercept = intercept,
                      test = test, method = estimation_method, ...)
    } else {
      metafor::rma.mv(yi = effect_size, V = variance_covariance,
                      mods = moderators, random = random_params,
                      data = raw_data, struct = structure, intercept = intercept,
                      test = test, method = estimation_method, ...)
    }
  } else {
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
}


#' Path Model Function
#'
#' This function fits the path model and returns adjusted standard errors.
#'
#' @param data A list that contains the correlation matrix for model fitting
#'   and the variance matrix. This would most likely come from the
#'   \code{\link{extract_model}} function.
#' @param model This is model syntax specified in the format by lavaan.
#' @param num_obs Number of observations
#' @param adjust_se Adjust the standard errors to reflect the ...
#' @param method_mat Method of estimation, can either be "loehlin" or "lavaan".
#'   Default is "loehlin"
#' @param method_null Unsure
#' @param ... Currently not used.
#' @seealso See \url{https://lavaan.ugent.be/} for more information on lavaan
#' syntax.
#'
#' @export
#'
path_model <- function(data, model, num_obs, adjust_se = TRUE,
                       method_mat = 'loehlin',
                       method_null = 'sem', ...) {

  fitted_model <- model_fit(model_input = model, R = data[['beta_matrix']],
                            method_mat = method_mat, method_null = method_null,
                            N = num_obs)

  coefficients <- format_coefficients(fitted_model[['path_coefficients']])

  # fitted_model <- lavaan::sem(model, sample.cov = data[['beta_matrix']],
  #                             sample.nobs = num_obs, ...)
  #
  # coefficients <- lavaan::parTable(fitted_model)

  if(adjust_se) {
    # outcomes <- subset(coefficients,
    #                    subset = op == '~',
    #                    select = c('lhs', 'rhs'))
    # num_outcomes <- split(outcomes,
    #                       outcomes$lhs)
    #
    # variables <- lapply(lapply(num_outcomes, unlist), unique)

    num_outcomes <- length(coefficients)
    variables <- lapply(coefficients, unique_names)

    # formulas <- lapply(seq_along(variables), function(xx)
    #   paste0(variables[[xx]][1], " ~ ", paste(variables[[xx]][2:length(variables[[xx]])], collapse = " + "))
    #   )

    data_list <- lapply(seq_along(variables),
                        function(xx)
                          matrix_subset(data[['beta_matrix']], variables[[xx]])
    )

    corr_pairs <- lapply(seq_along(data_list), function(xx)
      data.frame(t(utils::combn(colnames(data_list[[xx]]), 2))))

    corr_vector <- lapply(seq_along(data_list), function(xx)
      corpcor::sm2vec(data_list[[xx]]))

    corr_elements <- lapply(seq_along(data_list), function(xx)
      cbind(corr_pairs[[xx]], corr = corr_vector[[xx]]))

    full_corr_matrix <- cbind(data.frame(t(utils::combn(colnames(data[['beta_matrix']]), 2))),
                              corr = corpcor::sm2vec(data[['beta_matrix']]))
    full_corr_matrix[['num']] <- 1:nrow(full_corr_matrix)

    matches <- lapply(seq_along(corr_elements), function(xx)
      merge(corr_elements[[xx]], full_corr_matrix,
            by = c('X1', 'X2', 'corr')))

    matches <- lapply(seq_along(matches), function(xx)
      matches[[xx]][order(matches[[xx]][['num']]), ]
      )

    var_list <- lapply(seq_along(variables),
                       function(xx)
                         data[['var_matrix']][matches[[xx]][['num']],
                                              matches[[xx]][['num']]]
    )

    computed_se <- lapply(seq_along(data_list), function(xx)
      var_path(data_list[[xx]],
               var_list[[xx]],
               type = 'stdslopes')
    )
  } else {
    computed_se <- NULL
  }

  model_output <- list(beta_matrix = data[['beta_matrix']],
                       parameter_estimates = coefficients,
                       fit_measures = fitted_model,
                       computed_var = computed_se,
                       model = model)

  class(model_output) <- 'metaRmat'

  model_output
}

#' @importFrom stats pnorm
#' @export
summary.metaRmat <- function(object, fit_measures = TRUE, ...) {

  fixed_coef <- do.call('rbind', object[['parameter_estimates']])

  standard_errors <- lapply(seq_along(object[['computed_var']]), function(xx)
                            sqrt(diag(object[['computed_var']][[xx]])))

  fixed_coef[['standard_errors']] <- do.call('c', standard_errors)
  fixed_coef[['test_statistic']] <- fixed_coef[['estimate']] / fixed_coef[['standard_errors']]
  fixed_coef[['p_value']] <- stats::pnorm(abs(fixed_coef[['test_statistic']]), lower.tail = FALSE) * 2

  # random_coef <- subset(object[['parameter_estimates']],
  #                       subset = op == "~~",
  #                       select = c('lhs', 'rhs', 'est'))
  #
  # variances <- subset(random_coef, lhs == rhs,
  #                     select = c('lhs', 'est'))
  # names(variances) <- c('Variable', 'Estimate')
  # variances[['Estimate']] <- round(variances[['Estimate']], 3)
  #
  # variances <- paste(lapply(1:nrow(variances), function(xx)
  #   paste(variances[xx, ], collapse = " = ")), collapse = "\n ")
  #
  # covariances <- subset(random_coef, lhs != rhs)
  # covariances[['est']] <- round(covariances[['est']], 3)
  #
  # covariances <- paste(lapply(1:nrow(covariances), function(xx)
  #   paste(c(paste(covariances[1, c('lhs', 'rhs')], collapse = ' WITH '),
  #           covariances[1, 'est']), collapse = ' = ')))

  if(fit_measures) {
    # fit_meas <- data.frame(type = rownames(data.frame(data[['fit_measures']])),
    #                        data.frame(data[['fit_measures']], row.names = NULL))
    fit_meas <- object[['fit_measures']]
  } else {
    fit_meas <- NULL
  }

  structure(list(beta_matrix = data.frame(object[['beta_matrix']]),
                 model = object[['model']],
                 fit_measures = fit_meas,
                 #variance = variances,
                 #covariance = covariances,
                 fixed_coef = fixed_coef
                 ), class = "summary.metaRmat")

}

#' @export
print.summary.metaRmat <- function(x, digits = max(3, getOption("digits") - 3),
                                   signif.stars = getOption("show.signif.stars"),
                                   tidy = FALSE, ...) {

  if(tidy) {

  } else {
    cat('Average Correlation Matrix: \n')
    print(x$beta_matrix)
    cat('\n \n Model Fitted: \n', x$model, "\n \n")
      cat('Variance Estimates: \n', x$variance, "\n \n")
        cat('Covariance Estimates: \n', x$covariance, "\n \n")
        cat('Fixed Effects: \n')
        print(x$fixed_coef)
    # data.frame(x$fixed_coef[, 1:2], printCoefmat(x$fixed_coef[, 3:ncol(x$fixed_coef)], digits = digits, signif.stars = signif.stars,
    #              has.Pvalue = TRUE, cs.ind = 1:2, tst.ind = 3, P.values = TRUE))
  }
}
