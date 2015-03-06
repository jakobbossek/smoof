#' Deckkers-Aarts Function
#'
#' This continuous single-objective test function is defined by the formula
#' \deqn{f(\mathbf{x}) = 10^5\mathbf{x}_1^2 + \mathbf{x}_2^2 - (\mathbf{x}_1^2 + \mathbf{x}_2^2)^2 + 10^{-5} (\mathbf{x}_1^2 + \mathbf{x}_2^2)^4}
#' with the bounding box \eqn{-20 \leq \mathbf{x}_i \leq 20} for \eqn{i = 1, 2}.
#'
#' @references M. M. Ali, C. Khompatraporn, Z. B. Zabinsky, A Numerical Evaluation
#' of Several Stochastic Algorithms on Selected Continuous Global Optimization
#' Test Problems, Journal of Global Optimization, vol. 31, pp. 635-672, 2005.
#'
#' @template ret_smoof_single
#' @export
makeDeckkersAartsFunction = function() {
  makeSingleObjectiveFunction(
    name = "Deckkers-Aarts Function",
    fn = function(x) {
      a = x[1]^2
      b = x[2]^2
      1e+05 * a + b - (a + b)^2 + 1e-05 * (a + b)^4
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-20, -20),
      upper = c(20, 20),
      vector = FALSE
    ),
    tags = attr(makeDeckkersAartsFunction, "tags"),
    global.opt.params = matrix(
      c(0, 15,
        0, -15),
      ncol = 2L, byrow = TRUE),
    global.opt.value = -24777
  )
}

class(makeDeckkersAartsFunction) = c("function", "smoof_generator")
attr(makeDeckkersAartsFunction, "name") = c("Deckkers-Aarts Function")
attr(makeDeckkersAartsFunction, "type") = c("single-objective")
attr(makeDeckkersAartsFunction, "tags") = c("continuous", "differentiable", "non-separable", "non-scalable", "multimodal")
