#' @title
#' Dent Function
#'
#' @description
#' Builds and returns the bi-objective Dent test problem, which is defined as
#' follows:
#' \deqn{f(\mathbf{x}) = \left(f_1(\mathbf{x}_1), f_2(\mathbf{x})\right)}
#' with
#' \deqn{f_1(\mathbf{x}_1) = 0.5 \left( \sqrt(1 + (x_1 + x_2)^2) + \sqrt(1 + (x_1 - x_2)^2) + x_1 - x_2\right) + d}
#' and
#' \deqn{f_1(\mathbf{x}_1) = 0.5 \left( \sqrt(1 + (x_1 + x_2)^2) + \sqrt(1 + (x_1 - x_2)^2) - x_1 + x_2\right) + d}
#' where \eqn{d = \lambda * \exp(-(x_1 - x_2)^2)} and \eqn{\mathbf{x}_i \in [-1.5, 1.5], i = 1, 2}.
#'
#' @return [\code{smoof_multi_objective_function}]
#' @export
makeDentFunction = function() {

  # define the two-objective Dent function
  fn = function(x) {
    checkNumericInput(x, 2L)
    lambda = 0.85
    d = lambda * exp(-1 * (x[1] - x[2])^2)

    f = c(
      0.5 * (sqrt(1 + (x[1] + x[2])^2) + sqrt(1 + (x[1] - x[2])^2) + x[1] - x[2]) + d,
      0.5 * (sqrt(1 + (x[1] + x[2])^2) + sqrt(1 + (x[1] - x[2])^2) - x[1] + x[2]) + d
    )
    return(f)
  }

  makeMultiObjectiveFunction(
    name = "Dent Function",
    id = paste0("dent_2d_2o"),
    description = "Dent Function",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-1.5, -1.5),
      upper = c(1.5, 1.5),
      vector = TRUE
    ),
    n.objectives = 2L,
    ref.point = c(4.5, 4.5)
  )
}

class(makeDentFunction) = c("function", "smoof_generator")
attr(makeDentFunction, "name") = c("Dent")
attr(makeDentFunction, "type") = c("multi-objective")
attr(makeDentFunction, "tags") = c("multi-objective")
