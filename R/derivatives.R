#' @name Derivatives
#'
#' @rdname derivatives
#'
#' @title Tools for differentiating mathematical functions
#'
#' @description Obtain a function's Jacobian (matrix of first derivatives) or Hessian (matrix of second partial derivatives).
#'
#' @param f A mathematical expression generated with the \code{expression} function (e.g. \code{expression(y*sin(x))})
#' @param as_matrix Whether to return the symbolic matrix of derivatives as a list of expressions or as a character matrix.
#' @param eval_at A named list giving a point at which the matrix of derivatives is to be evaluated (e.g. \code{list(x = 1, y = 2)})
#'
#' @return If \code{as_matrix = TRUE}, a character matrix showing the symbolic matrix of derivatives. If \code{as_matrix = FALSE}, the result is a nested list of expressions. To retrieve the entry in row i column j of the matrix, select \code{hessian_result[[i]][[j]]}. If an evaluation point is specified with \code{eval_at}, the matrix of derivatives is evaluated and the result is a numeric matrix.
#'
#' @family math tools
#'
#'
#' @examples
#' # Create a mathematical expression
#' fn <- expression(a * log(p/(1-p)))
#'
#' # Get the symbolic Hessian
#' ## As a nested list
#'   get_hessian(fn)
#'
#' ## As a character matrix
#'   get_hessian(fn, as_matrix = TRUE)
#'
#' # Evaluated the Hessian at a specific point
#'
#'   get_hessian(fn, eval_at = list(a = 100, p = 0.5))
#'
#' @rdname derivatives
#' @export
get_hessian <- function(f, as_matrix = FALSE, eval_at = NULL) {

  if (is.expression(f)) {
    fn_inputs <- all.vars(f); names(fn_inputs) <- fn_inputs
    n_inputs <- length(fn_inputs)
  }

  # Error-checking:

  ## Check input types

    if (!is.expression(f)) {stop("`f` must be an expression (e.g. `expression(sin(x)^2)`)")}

    if (!is.logical(as_matrix)) {stop("`as_matrix` must be TRUE or FALSE (the default)")}

    if (!is.null(eval_at) && !is.list(eval_at)) {
      stop("If `eval_at` is specified, it must be a named list (e.g. `list(x = 1, y = 2)`)")
    }
  ## Make sure evaluation point names match the function inputs

    if (!is.null(eval_at)) {
      input_diffs <- union(
        setdiff(fn_inputs, names(eval_at)),
        setdiff(names(eval_at), fn_inputs)
      )
      if (length(input_diffs) > 0) {
        stop("The variables in `f` must match the names of `eval_at`.")
      }
    }

  # Obtain the symbolic Hessian as a nested list

    list_result <- lapply(fn_inputs, function(x) lapply(fn_inputs, function(x) NULL))

    for (i in seq_len(n_inputs)) {

      first_deriv <- D(f, fn_inputs[i])
      first_deriv <- as.expression(first_deriv)

      for (j in seq_len(n_inputs)) {

        second_partial_deriv <- D(first_deriv, fn_inputs[j])
        second_partial_deriv <- as.expression(second_partial_deriv)

        list_result[[i]][[j]] <- second_partial_deriv

      }
    }

  # Convert the symbolic Hessian to a character matrix
    if (is.null(eval_at)) {
      if (as_matrix) {
        matrix_result <- matrix(as.character(diag(n_inputs)), nrow = n_inputs, ncol = n_inputs)

        for (i in seq_len(n_inputs)) {
          for (j in seq_len(n_inputs)) {

            matrix_result[i, j] <- gsub("expression", "", toString(list_result[[i]][[j]]), fixed = TRUE)
            dimnames(matrix_result) <- list(fn_inputs, fn_inputs)
          }
        }
      }
    }

  # Evaluate the Hessian at a set point if a named list is provided

    if (!is.null(eval_at)) {
      result_vals <- matrix(data = NA_real_, nrow = n_inputs, ncol = n_inputs)

      for (i in seq_len(n_inputs)) {
        for (j in seq_len(n_inputs)) {

          result_vals[i, j] <- eval(list_result[[i]][[j]], envir = eval_at)
          dimnames(result_vals) <- list(fn_inputs, fn_inputs)

        }
      }
    }

  # Return the final result

    if (!is.null(eval_at)) {
      return(result_vals)
    } else if (as_matrix) {
      return(matrix_result)
    } else {
      return(list_result)
    }
}

#' @rdname derivatives
#' @export
#' @examples
#' # Get the symbolic Jacobian
#' ## As a nested list
#'    get_jacobian(fn)
#'
#' ## As a character matrix
#'    get_jacobian(fn, as_matrix = TRUE)
#'
#' # Evaluate the Jacobian at a specific point
#'
#'    get_jacobian(fn, eval_at = list(a = 100, p = 0.5))

get_jacobian <- function(f, as_matrix = FALSE, eval_at = NULL) {

  if (is.expression(f)) {
    fn_inputs <- all.vars(f); names(fn_inputs) <- fn_inputs
    n_inputs <- length(fn_inputs)
    n_outputs <- length(f)
  }

  # Error-checking:

  ## Check input types

  if (!is.expression(f)) {stop("`f` must be an expression (e.g. `expression(sin(x)^2)`)")}

  if (!is.logical(as_matrix)) {stop("`as_matrix` must be TRUE or FALSE (the default)")}

  if (!is.null(eval_at) && !is.list(eval_at)) {
    stop("If `eval_at` is specified, it must be a named list (e.g. `list(x = 1, y = 2)`)")
  }
  ## Make sure evaluation point names match the function inputs

  if (!is.null(eval_at)) {
    input_diffs <- union(
      setdiff(fn_inputs, names(eval_at)),
      setdiff(names(eval_at), fn_inputs)
    )
    if (length(input_diffs) > 0) {
      stop("The variables in `f` must match the names of `eval_at`.")
    }
  }

  # Obtain the symbolic Jacobian as a nested list

  list_result <- lapply(seq_len(n_outputs), function(x) lapply(fn_inputs, function(x) NULL))

  for (i in length(f)) {

    for (j in seq_len(n_inputs)) {

      first_deriv <- D(f[i], fn_inputs[j])
      first_deriv <- as.expression(first_deriv)

      list_result[[i]][[j]] <- first_deriv

    }
  }

  # Convert the symbolic Jacobian to a character matrix
  if (is.null(eval_at)) {
    if (as_matrix) {
      matrix_result <- matrix(NA_character_, nrow = length(f), ncol = n_inputs)

      for (i in length(f)) {
        for (j in seq_len(n_inputs)) {

          matrix_result[i, j] <- gsub("expression", "", toString(list_result[[i]][[j]]), fixed = TRUE)
          dimnames(matrix_result) <- list(names(f), fn_inputs)
        }
      }
    }
  }

  # Evaluate the Jacobian at a set point if a named list is provided

  if (!is.null(eval_at)) {
    result_vals <- matrix(data = NA_real_, nrow = length(f), ncol = n_inputs)

    for (i in length(f)) {
      for (j in seq_len(n_inputs)) {

        result_vals[i, j] <- eval(list_result[[i]][[j]], envir = eval_at)
        dimnames(result_vals) <- list(names(f), fn_inputs)

      }
    }
  }

  # Return the final result

  if (!is.null(eval_at)) {
    return(result_vals)
  } else if (as_matrix) {
    return(matrix_result)
  } else {
    return(list_result)
  }
}
