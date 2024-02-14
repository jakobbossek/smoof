#' @title
#' Viennet function generator
#'
#' @description
#' The Viennet test problem VNT is designed for three objectives only. It has
#' a discrete set of Pareto fronts. It is defined by the following formulae.
#' \deqn{f(\mathbf{x}) = \left(f_1(\mathbf{x}), f_2(\mathbf{x}, f_3(\mathbf{x}\right)}
#' with
#' \deqn{f_1(\mathbf{x}) = 0.5(\mathbf{x}_1^2 + \mathbf{x}_2^2) + \sin(\mathbf{x}_1^2 + \mathbf{x}_2^2)}
#' \deqn{f_2(\mathbf{x}) = \frac{(3\mathbf{x}_1 + 2\mathbf{x}_2 + 4)^2}{8} + \frac{(\mathbf{x}_1 - \mathbf{x}_2 + 1)^2}{27} + 15}
#' \deqn{f_3(\mathbf{x}) = \frac{1}{\mathbf{x}_1^2 + \mathbf{x}_2^2 + 1} - 1.1\exp(-(\mathbf{x}_1^1 + \mathbf{x}_2^2))}
#' with box constraints \eqn{-3 \leq \mathbf{x}_1, \mathbf{x}_2 \leq 3}.
#'
#' @references
#' Viennet, R. (1996). Multi-criteria optimization using a genetic algorithm for determining the
#' Pareto set. International Journal of Systems Science 27 (2), 255-260.
#'
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the Viennet function as a \code{smoof_multi_objective_function} object.
#' @export
makeViennetFunction = function() {

  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    .Call("mof_viennet", x)
  }

  makeMultiObjectiveFunction(
    name = "Viennet Function",
    id = "viennet",
    description = "Viennet function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = rep(-3, 2L),
      upper = rep(3, 2L),
      vector = TRUE
      ),
    n.objectives = 3L#,
    #ref.point = rep(11, n.objectives)
  )
}

class(makeViennetFunction) = c("function", "smoof_generator")
attr(makeViennetFunction, "name") = c("Viennet")
attr(makeViennetFunction, "type") = c("multi-objective")
attr(makeViennetFunction, "tags") = c("multi-objective")
