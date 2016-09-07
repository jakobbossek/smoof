#' Hartmann 4 Function
#'
#' Unimodal single-objective test function with four local minima. 
#' It is designed to have a mean of zero and a variance of one.
#' The implementation is based on the mathematical formulation
#' \deqn{f(x) = - \sum_{i=1}^4 \alpha_i \ exp \left(-\sum_{j=1}^4 A_{ij}(x_j-P_{ij})^2 \right)}, where 
#' \deqn{\alpha = (1.0, 1.2, 3.0, 3.2)^T, \\
#'  A = \left( \begin{array}{rrrrrr}
#'   10   & 3    & 17   & 3.50 \\
#'   0.05 & 10   & 17   & 0.1  \\
#'   3    & 3.5  & 1.7  & 10   \\
#'   17   & 8    & 0.05 & 10   
#'  \end{array} \right), \\
#'   P = 10^{-4} \cdot \left(\begin{array}{rrrrrr}
#'   1312 & 1696 & 5569 & 124  \\
#'   2329 & 4135 & 8307 & 3736 \\
#'   2348 & 1451 & 3522 & 2883 \\
#'   4047 & 8828 & 8732 & 5743 
#'  \end{array} \right)}
#' The function is restricted to four dimensions with \eqn{\mathbf{x}_i \in [0,1], i = 1, \ldots, 4.}
#'
#' @references Picheny, V., Wagner, T., & Ginsbourger, D. (2012). A benchmark 
#' of kriging-based infill criteria for noisy optimization.
#'
#' @template ret_smoof_single
#' @export
makeHartmann4Function = function() {
  makeSingleObjectiveFunction(
    name = "Hartmann4 Function",
    id = "hartmann_4d",
    fn = function(x) {
      alpha = c(1.0, 1.2, 3.0, 3.2)
      A = matrix(
        c(10,  0.05, 3,   17, 
          3,   10,   3.5, 8, 
          17,  17,   1.7, 0.05, 
          3.5, 0.1,  10,  10),
        4L, 4L)
      P = matrix(
        c(0.1312, 0.2329, 0.2348, 0.4047, 
          0.1696, 0.4135, 0.1451, 0.8828, 
          0.5569, 0.8307, 0.3522, 0.8732, 
          0.0124, 0.3736, 0.2883, 0.5743),
        4L, 4L)
      
      x.mat = matrix(rep(x, 4L), 4, 4, byrow = TRUE)
      inner = rowSums(A * (x.mat - P)^2)
      outer = sum(alpha * exp(-inner))
      (1.1 - outer) / 0.839
    },
    par.set = makeNumericParamSet(
      len = 4L,
      id = "x",
      lower = 0,
      upper = 1,
      vector = TRUE
    ),
    tags = attr(makeHartmann4Function, "tags"),
    global.opt.params = c(0.19151, 0.19121, 0.56236, 0.26709),
    global.opt.value = -3.133465
  )
}

class(makeHartmann4Function) = c("function", "smoof_generator")
attr(makeHartmann4Function, "name") = c("Hartmann4")
attr(makeHartmann4Function, "type") = c("single-objective")
attr(makeHartmann4Function, "tags") = c("single-objective", "continuous")
