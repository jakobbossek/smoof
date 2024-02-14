#' @title 
#' Shekel functions
#'
#' @description 
#' Single-objective test function based on the formula
#' \deqn{f(\mathbf{x}) = -\sum_{i=1}^{m} \left(\sum_{j=1}^{4} (x_j - C_{ji})^2 + \beta_{i}\right)^{-1}}.
#' Here, \eqn{m \in \{5, 7, 10\}} defines the number of local optima, \eqn{C} is a \eqn{4 x 10} matrix
#' and \eqn{\beta = \frac{1}{10}(1, 1, 2, 2, 4, 4, 6, 3, 7, 5, 5)} is a vector. See \url{https://www.sfu.ca/~ssurjano/shekel.html}
#' for a definition of \eqn{C}.
#'
#' @param m [\code{numeric(1)}]\cr
#'   Integer parameter (defines the number of local optima).
#'   Possible values are 5, 7 or 10.
#'   
#' @return
#' An object of class \code{SingleObjectiveFunction}, representing the Shekel Functions.
#' 
#' @template ret_smoof_single
#' @export
makeShekelFunction = function(m) {
  if (m %nin% c(5, 7, 10))
    BBmisc::stopf("Shekel function only defined for m = 5, 7, 10, but not %i.", m)

  force(m)

  # naive implementation
  # fn = function(x) {
  #   s = 0
  #   for (i in 1:m) {
  #     t = 0
  #     for (j in 1:4) {
  #       t = t + ((x[j] - C[j, i])^2)
  #     }
  #     s = s + 1/(t + beta[i])
  #   }
  #   return(-s)
  # }

  fn = function(x) {
    checkNumericInput(x, 4L)

    C = matrix(
        c(4.0, 1.0, 8.0, 6.0, 3.0, 2.0, 5.0, 8.0, 6.0, 7.0,
          4.0, 1.0, 8.0, 6.0, 7.0, 9.0, 3.0, 1.0, 2.0, 3.6,
          4.0, 1.0, 8.0, 6.0, 3.0, 2.0, 5.0, 8.0, 6.0, 7.0,
          4.0, 1.0, 8.0, 6.0, 7.0, 9.0, 3.0, 1.0, 2.0, 3.6),
        nrow = 4L, byrow = TRUE)
    beta = c(1, 2, 2, 4, 4, 6, 3, 7, 5, 5) * 0.1

    x = matrix(rep(x, m), nrow = 4)

    return(-sum(1 / (colSums((x - C[, 1:m])^2) + beta[1:m])))
  }

  makeSingleObjectiveFunction(
    name = sprintf("4-d Shekel%i function", m),
    id = sprintf("shekel_4d_m%i", m),
    fn = fn,
    par.set = ParamHelpers::makeNumericParamSet(
      len = 4L,
      id = "x",
      lower = rep(0, 4L),
      upper = rep(10, 4L),
      vector = TRUE
    ),
    tags = attr(makeShekelFunction, "tags"),
    global.opt.params = rep(4, 4L),
    global.opt.value = if (m == 5) -10.1532 else if (m == 7) -10.4029 else -10.5364
  )
}

class(makeShekelFunction) = c("function", "smoof_generator")
attr(makeShekelFunction, "name") = c("Shekel function")
attr(makeShekelFunction, "type") = c("single-objective")
attr(makeShekelFunction, "tags") = c("single-objective", "continuous", "differentiable", "non-separable", "multimodal")
