#' @title
#' Hartmann Function
#'
#' @description
#' Uni-modal single-objective test function with six local minima.
#' The implementation is based on the mathematical formulation
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
#' The function is not normalized in contrast to some benchmark applications in the literature.
#' 
#' @return
#' An object of class \code{SingleObjectiveFunction}, representing the Hartmann Function.
#'
#' @references Picheny, V., Wagner, T., & Ginsbourger, D. (2012). A benchmark
#' of kriging-based infill criteria for noisy optimization.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeHartmannFunction = function(dimensions) {
  checkmate::assertChoice(dimensions, c(3L, 4L, 6L))
  force(dimensions)
  i = seq_len(dimensions)

  if (dimensions == 3) {
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
    global.opt.params = c(0.114614, 0.555649, 0.852547)
    global.opt.value = -3.86278
  } else {
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
    global.opt.params = c(0.20169, 0.150011, 0.476874, 0.275332, 0.311652, 0.6573)[i]
    global.opt.value = -3.32237
  }
  makeSingleObjectiveFunction(
    name = sprintf("%i-d Hartmann Function", dimensions),
    id = sprintf("hartmann_%id", dimensions),
    fn = function(x) {
      checkNumericInput(x, dimensions)
      alpha = c(1.0, 1.2, 3.0, 3.2)
      x.mat = matrix(rep(x, 4L), 4L, dimensions, byrow = TRUE)
      inner = rowSums(A[, i] * (x.mat - P[, i])^2)
      -sum(alpha * exp(-inner))
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = 0,
      upper = 1,
      vector = TRUE
    ),
    tags = attr(makeHartmannFunction, "tags"),
    global.opt.params = global.opt.params,
    global.opt.value = global.opt.value
  )
}

class(makeHartmannFunction) = c("function", "smoof_generator")
attr(makeHartmannFunction, "name") = c("Hartmann")
attr(makeHartmannFunction, "type") = c("single-objective")
attr(makeHartmannFunction, "tags") = c("single-objective", "scalable", "continuous", "multimodal", "differentiable")
