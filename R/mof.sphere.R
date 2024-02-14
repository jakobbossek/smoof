#' @title
#' Bi-objective Sphere function
#'
#' @description
#' Builds and returns the bi-objective Sphere test problem:
#' \deqn{f(\mathbf{x}) = \left(\sum_{i=1}^{n} \mathbf{x}_i^2, \sum_{i=1}^{n} (\mathbf{x}_i - \mathbf{a})^2\right)}
#' where \deqn{\mathbf{a} \in R^n}.
#'
#' @param dimensions [\code{integer(1)}]\cr
#'   Number of decision variables.
#' @param a [\code{numeric(1)}]\cr
#'   Shift parameter for the second objective. Default is (0,...,0).
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the sphere function as a \code{smoof_multi_objective_function} object.
#' @export
makeBiSphereFunction = function(dimensions, a = rep(0, dimensions)) {
  force(dimensions)
  force(a)

  checkmate::assertInt(dimensions, lower = 2L)
  checkmate::assertNumeric(a, len = dimensions, any.missing = FALSE, all.missing = FALSE)

  # define the two-objective Dent function
  fn = function(x) {
    checkNumericInput(x, dimensions)
    c(sum(x^2), sum((x - a)^2))
  }

  makeMultiObjectiveFunction(
    name = "Biobjective Sphere Function",
    id = paste0("bisphere_", dimensions, "d_2o"),
    description = "Bi-objective Sphere Function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
    n.objectives = 2L,
    ref.point = c(1.5, 1.5)
  )
}

class(makeBiSphereFunction) = c("function", "smoof_generator")
attr(makeBiSphereFunction, "name") = c("BiSphere")
attr(makeBiSphereFunction, "type") = c("multi-objective")
attr(makeBiSphereFunction, "tags") = c("multi-objective")
