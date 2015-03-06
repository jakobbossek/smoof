#' Damavandi Function
#'
#' The Damavandi Function is the two-dimensional single-objective test function
#' \deqn{\left[1 - \left|\frac{\sin(\pi(\mathbf{x}_1 - 2))\sin(\pi(\mathbf{x}_2 - 2))}{\pi^2(\mathbf{x}_1 - 2)(\mathbf{x}_2 - 2)}\right|^5\right]\left(2 + (\mathbf{x}_1 - 7)^2 + 2(\mathbf{x}_2 - 7)^2\right)}
#' subject to \eqn{\mathbf{x}_i \in [0, 14]} for \eqn{i = 1, 2}.
#'
#' @references N. Damavandi, S. Safavi-Naeini, A Hybrid Evolutionary Programming
#' Method for Circuit Optimization, IEEE Transaction on Circuit and Systems I,
#' vol. 52, no. 5, pp. 902-910, 2005.
#'
#' @template ret_smoof_single
#' @export
#FIXME: global opt is in (2, 2), but function is not defined for this params!
makeDamavandiFunction = function() {
  makeSingleObjectiveFunction(
    name = "Damavandi Function",
    fn = function(x) {
      t1 = sin(pi * (x[1] - 2)) * sin(pi * (x[2] - 2))
      t2 = pi^2 * (x[1] - 2) * (x[2] - 2)
      a = 1
      b = abs(t1 / t2)^5
      c = 2 + (x[1] - 7)^2 + 2 * (x[2] - 7)^2
      return ((a - b) * c)
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(0, 0),
      upper = c(14, 14),
      vector = FALSE
    ),
    tags = attr(makeDamavandiFunction, "tags"),
    global.opt.params = c(2, 2),
    global.opt.value = 0
  )
}

class(makeDamavandiFunction) = c("function", "smoof_generator")
attr(makeDamavandiFunction, "name") = c("Damavandi Function")
attr(makeDamavandiFunction, "type") = c("single-objective")
attr(makeDamavandiFunction, "tags") = c("continuous", "differentiable", "non-separable", "non-scalable", "multimodal")
