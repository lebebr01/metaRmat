#' Prep Data for Metafor
#'
#' @param data
#' @param n Sample size
#' @param type
#' @param missing
#' @param variable_names An optional character vector specifying variable names
#'   of original data. This is mostly used to pass to \code{\link{df_to_corr}}.
#'
#' @importFrom Matrix bdiag
#' @importFrom corpcor sm2vec
#'
#' @export
prep_data <- function(data, n, type = c('average','weighted', 'simple'),
                      missing = 'remove', variable_names = NULL) {

  if(is.data.frame(data)) {
    data <- df_to_corr(data, variables = variable_names)
  }

  olkin <- olkin_siotani(data, n, type)

  variance_covariance <- var_cov(olkin)

  data_vector_list <- lapply(data,  corpcor::sm2vec)
  data_vector <-   matrix(unlist(data_vector_list))

  outcome <- rep(1:length(data_vector_list[[1]]), length(data))
  outcome <- as.factor(outcome)
  study <- sort(rep(1:length(data), length(data_vector_list[[1]])))

  if(missing == 'remove') {
    miss_loc <- missing_data(data_vector)

    data_vector <- data_vector[miss_loc]
    variance_covariance <- variance_covariance[c(miss_loc), c(miss_loc)]
    outcome <- outcome[miss_loc]
    study <- study[miss_loc]
  }

  list(data = data.frame(yi = data_vector,
                         outcome = outcome,
                         study = study
  ),
  V = variance_covariance
  )
}
