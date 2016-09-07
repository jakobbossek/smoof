#' Hartmann 3 Function
#'
#' Unimodal single-objective test function with three local minima. 
#' The implementation is based on the mathematical formulation
#' \deqn{f(x) = - \sum_{i=1}^4 \alpha_i \ exp \left(-\sum_{j=1}^3 A_{ij}(x_j-P_{ij})^2 \right)}, where 
#' \deqn{\alpha = (1.0, 1.2, 3.0, 3.2)^T, \\
#'  A = \left( \begin{array}{rrrrrr}
#'   3.0 & 10 & 30 \\ 
#'   0.1 & 10 & 35 \\ 
#'   3.0 & 10 & 30 \\ 
#'   0.1 & 10 & 35 \\ 
#'  \end{array} \right), \\
#'   P = 10^{-4} \cdot \left(\begin{array}{rrr}
#'   3689 & 1170 & 2673 \\ 
#'   4699 & 4387 & 7470 \\ 
#'   1091 & 8732 & 5547 \\ 
#'    381 & 5743 & 8828
#'  \end{array} \right)}
#' The function is restricted to three dimensions with \eqn{\mathbf{x}_i \in [0,1], i = 1, \ldots, 3.}
#'
#' @references Picheny, V., Wagner, T., & Ginsbourger, D. (2012). A benchmark 
#' of kriging-based infill criteria for noisy optimization.
#'
#' @template ret_smoof_single
#' @export
makeHartmann3Function = function() {
  makeSingleObjectiveFunction(
    name = "Hartmann3 Function",
    id = "hartmann_3d",
    fn = function(x) {
      alpha = c(1.0, 1.2, 3.0, 3.2)
      A = matrix(
        c(3,  0.1, 3,  0.1, 
          10, 10,  10, 10, 
          30, 35,  30, 35),
        4L, 3L)
      P = matrix(
        c(0.36890, 0.46990, 0.10910, 0.03815, 
          0.11700, 0.43870, 0.87320, 0.57430, 
          0.26730, 0.74700, 0.55470, 0.88280),
        4L, 3L)
      
      x.mat = matrix(rep(x, 4L), 4, 3, byrow = TRUE)
      inner = rowSums(A * (x.mat - P)^2)
      outer = sum(alpha * exp(-inner))
      -outer
    },
    par.set = makeNumericParamSet(
      len = 3L,
      id = "x",
      lower = 0,
      upper = 1,
      vector = TRUE
    ),
    tags = attr(makeHartmann3Function, "tags"),
    global.opt.params = c(0.114614, 0.555649, 0.852547),
    global.opt.value = -3.86278
  )
}

class(makeHartmann3Function) = c("function", "smoof_generator")
attr(makeHartmann3Function, "name") = c("Hartmann3")
attr(makeHartmann3Function, "type") = c("single-objective")
attr(makeHartmann3Function, "tags") = c("single-objective", "continuous")
