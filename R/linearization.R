#' @name TSL Variance
#'
#' @rdname linearization
#'
#' @title Use Taylor linearization to get variance-covariance estimate
#'
#' @description Use Taylor linearization (i.e. the Delta Method) to obtain the variance of a function or the variance-covariance matrix of a vector-valued function
#'
#' @param f An expression or list of expressions created using the `expression()` function, e.g. `expression(x/y)` or `list(expression(sin(x + y)), expression(cos(x + y)))`.
#' @param values A named list of values where first derivatives are to be evaluated, e.g. `list(y = 10, x = 5)`. The names must match variables in the expression(s).
#' @param vcov_matrix A variance-covariance matrix for the variables in the expression must be provided.
#'
#' @return A variance-covariance matrix for the expressions evaluated at the specified points. If only one expression is provided, the result is a single numeric value.
#' @export
#'
#' @examples
#'
#' # Sample values of y and x
#' var_cov_pop <- matrix(c(5,4,4,5), byrow = TRUE, nrow = 2)
#' sampled_obs <- MASS::mvrnorm(n = 100, mu = c(120, 100), Sigma = var_cov_pop)
#'
#' y <- sampled_obs[,1]
#' x <- sampled_obs[,2]
#'
#' # Estimate ratio of y to x
#' ratio <- mean(y)/mean(x)
#'
#' # Estimate sampling covariance of X and Y
#' x_y_vcov_estimate <- cov(sampled_obs) / (nrow(sampled_obs) - 1)
#'
#' # Estimate sampling covariance of ratio
#' get_tsl_variance(list('ratio' = expression(y/x),
#'                       'product' = expression(x/y)),
#'                  values = list('y' = mean(y),
#'                                'x' = mean(x)),
#'                  vcov_matrix = x_y_vcov_estimate)
#'
#'

get_tsl_variance <- function(f, values, vcov_matrix) {

  if (!is.list(f)) {
    exprsn_list <- list(f)
  } else {
    exprsn_list <- f
  }

  fn_inputs <- lapply(exprsn_list, function(f) {
    if (is.expression(f)) {
      fn_inputs <- all.vars(f)
    }
    return(fn_inputs)
  })

  fn_inputs <- unique(unlist(fn_inputs))
  names(fn_inputs) <- fn_inputs
  n_inputs <- length(fn_inputs)

  # Check inputs

    for (exprsn in exprsn_list) {
      if ((is.null(exprsn) || length(exprsn) > 1) | !is.expression(exprsn)) {
        stop("`f` must be an expression or list of expressions, e.g. `list('f1' = expression(y/x), 'f2' = expression(y*x))`")
      }
    }

    if ((!is.null(values) && !is.list(values)) || is.null(names(values))) {
      stop("If `values` is specified, it must be a named list (e.g. `list(x = 1, y = 2)`)")
    }
  ## Make sure evaluation point names match the function inputs

    if (!is.null(values)) {
      input_diffs <- union(
        setdiff(fn_inputs, names(values)),
        setdiff(names(values), fn_inputs)
      )
      if (length(input_diffs) > 0) {
        stop("The variables in `f` must match the names of `values`.")
      }
    } else {
      stop("Values must be specified in the `values` argument.")
    }

  ## Check that dimension of variance-covariance matrix compatible with values

  if (nrow(vcov_matrix) != n_inputs || ncol(vcov_matrix) != n_inputs) {
    stop("The number of rows and number columns of `vcov_matrix` must match the length of `values`.")
  }


  # Calculate the Jacobian at the supplied values
  jacobian <- schneidr::get_jacobian(f = f,
                                     eval_at = values)

  # Linearized variance estimate

  tsl_var_est <- jacobian %*% vcov_matrix %*% t(jacobian)

  if (nrow(tsl_var_est) == 1) {
    tsl_var_est <- as.numeric(tsl_var_est)
  }

  return(tsl_var_est)
}
