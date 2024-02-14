#' @title
#' Bent-Cigar Function
#'
#' @description
#' Scalable test function \eqn{f} with
#' \deqn{f(\mathbf{x}) = x_1^2 + 10^6 \sum_{i = 2}^{n} x_i^2}
#' subject to \eqn{-100 \leq \mathbf{x}_i \leq 100} for \eqn{i = 1, \ldots, n}.
#' 
#' @return
#' An object of class \code{SingleObjectiveFunction}, representing the Bent-Cigar Function.
#'
#' @references See \url{https://al-roomi.org/benchmarks/unconstrained/n-dimensions/164-bent-cigar-function}.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeBentCigarFunction = function(dimensions) {
  checkmate::assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = "Bent-Cigar Function",
    id = "bentCigar_2d",
    fn = function(x) {
      checkNumericInput(x, dimensions)
      x[1]^2 + 1e+06 * sum(x[2:dimensions]^2)
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    tags = attr(makeBentCigarFunction, "tags"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

class(makeBentCigarFunction) = c("function", "smoof_generator")
attr(makeBentCigarFunction, "name") = c("Bent-Cigar")
attr(makeBentCigarFunction, "type") = c("single-objective")
attr(makeBentCigarFunction, "tags") = c("single-objective", "continuous", "differentiable", "non-separable", "scalable", "unimodal")
