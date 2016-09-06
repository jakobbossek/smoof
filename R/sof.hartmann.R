#' Hartmann Function
#'
#' Unimodal single-objective test function with six local minima. The implementation is based on the
#' mathematical formulation
#' \deqn{f(x) = - \sum_{i=1}^4 \alpha_i \ exp \left(-\sum_{j=1}^6 A_{ij}(x_j-P_{ij})^2 \right)}, where 
#' \deqn{\alpha = (1.0, 1.2, 3.0, 3.2)^T, \\
#'  A = \left( \begin{array}{rrrrrr}
#'   10   & 3    & 17   & 3.50 & 1.7  & 8 \\
#'   0.05 & 10   & 17   & 0.1  & 8    & 14 \\
#'   3    & 3.5  & 1.7  & 10   & 17   & 8 \\
#'   17   & 8    & 0.05 & 10   & 0.1  & 14
#'  \end{array} \right), \\
#'   P = 10^{-4} \cdot \left(\begin{array}{rrrrrr}
#'   1312 & 1696 & 5569 & 124  & 8283 & 5886 \\
#'   2329 & 4135 & 8307 & 3736 & 1004 & 9991 \\
#'   2348 & 1451 & 3522 & 2883 & 3047 & 6650 \\
#'   4047 & 8828 & 8732 & 5743 & 1091 & 381
#'  \end{array} \right)}
#' The function is restricted to six dimensions with \eqn{\mathbf{x}_i \in [0,1], i = 1, \ldots, 6.}
#'
#' @references Picheny, V., Wagner, T., & Ginsbourger, D. (2012). A benchmark 
#' of kriging-based infill criteria for noisy optimization.
#'
#' @template ret_smoof_single
#' @export
makeHartmannFunction = function() {
  makeSingleObjectiveFunction(
    name = "Hartmann Function",
    id = "hartmann_6d",
    fn = function(x) {
      alpha = c(1.0, 1.2, 3.0, 3.2)
      A = matrix(
        c(10,  0.05, 3,   17, 
          3,   10,   3.5, 8, 
          17,  17,   1.7, 0.05, 
          3.5, 0.1,  10,  10, 
          1.7, 8,    17,  0.1, 
          8,   14,   8,   14),
        4L, 6L)
      P = matrix(
        c(0.1312, 0.2329, 0.2348, 0.4047, 
          0.1696, 0.4135, 0.1451, 0.8828, 
          0.5569, 0.8307, 0.3522, 0.8732, 
          0.0124, 0.3736, 0.2883, 0.5743, 
          0.8283, 0.1004, 0.3047, 0.1091, 
          0.5886, 0.9991, 0.665, 0.0381),
          4L, 6L)
      
      x.mat = matrix(rep(x, 4L), 4, 6, byrow = TRUE)
      inner = rowSums(A[,1:6]*(x.mat-P)^2)
      outer = sum(alpha * exp(-inner))
      (2.58 + outer) / 1.94
    },
    par.set = makeNumericParamSet(
      len = 6L,
      id = "x",
      lower = 0,
      upper = 1,
      vector = TRUE
    ),
    tags = attr(makeHartmannFunction, "tags"),
    global.opt.params = c(0.20169, 0.150011, 0.476874, 0.275332, 0.311652, 0.6573),
    global.opt.value = -3.32237
  )
}

class(makeBirdFunction) = c("function", "smoof_generator")
attr(makeBirdFunction, "name") = c("Hartmann")
attr(makeBirdFunction, "type") = c("single-objective")
attr(makeBirdFunction, "tags") = c("single-objective", "continuous") #FIXME: Maybe add Tags?
