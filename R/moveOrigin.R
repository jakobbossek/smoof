#' @title
#' Return a function with a shifted origin.
#'
#' @description
#' Move the origin of \code{fn} by \code{shift}.
#' This is useful to test if an algorithm is sensitive to the absolute location of the global optimum.
#' 
#' Note that all other aspects of the function are also shifted (global and local optima, and any box constraints).
#' Therefore the relative location of the optima w.r.t. the bounding box do not change, only the absolute position!
#' This is different from the shift transformation applied by BBOB to generate different instances.
#'
#' @param fn [\code{smoof_function}]\cr
#'   Smoof function which should be shifted
#' @param shift [\code{numeric}]\cr
#'   Location of origin of the shifted function in the parameter space of \code{fn}
#' @return [\code{smoof_shifted_function}]
#' @examples
#' fn = makeBBOBFunction(dimensions = 2L, fid = 1L, iid = 1L)
#' sfn = moveOrigin(fn, c(2, 1))
#'
#' # Origin of the original function is now at (2, 1) in the shifted parameter space
#' fn(c(0, 0))
#' sfn(c(2, 1))
#'
#' @export
moveOrigin = function(fn, shift) {
  if (!requireNamespace("openssl", quietly = TRUE))
    stopf("Package \"openssl\" is required for this function.")

  if (!testClass(fn, "smoof_function") && !testClass(fn, "smoof_wrapped_function")) {
    stopf("The passed function needs to be a (wrapped) smoof function.")
  }
  assertNumeric(shift, 
                len = getNumberOfParameters(fn), 
                finite = TRUE, 
                any.missing = FALSE, 
                all.missing = FALSE)
  
  wrapped_fn = function(x, ...) {
    if (is.matrix(x)) {
      x = sweep(x, 2, shift)
    } else { # Vector case
      x = x - shift
    }
    fn(x, ...)
  }

  # We precalculate the new name and id because they are kind of expensive
  # to derive and may be accessed many times.
  id = getID(fn)
  if (!is.na(id)) {
    id = sprintf("%s-shifted_%s", id, openssl::sha1(serialize(shift, NULL)))
  }
  name = sprintf("%s shifted by (%s)", getName(fn), paste(shift, collapse=", "))

  ps = getParamSet(fn)
  # Adjust lower and upper bounds of parameter set
  ps$pars$x$lower <- ps$pars$x$lower + shift
  ps$pars$x$upper <- ps$pars$x$upper + shift
  structure(wrapped_fn,
            id = id,
            name = name,
            par.set = ps,
            class = c("smoof_shifted_function", "smoof_function", "function")
            )
}
