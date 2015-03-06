#' Price Function N. 3
#'
#' Third function by Price. The implementation is based on the defintion
#' \deqn{f(\mathbf{x}) = 100 (\mathbf{x}_2 - \mathbf{x}_1^2)^2 + 6 \left[6.4 (\mathbf{x}_2 - 0.5)^2 - \mathbf{x}_1 - 0.6\right]^2}
#' subject to \eqn{\mathbf{x}_i \in [-500, 500]}.
#'
#' @references W. L. Price, A Controlled Random Search Procedure for Global
#' Optimisation, Computer journal, vol. 20, no. 4, pp. 367-370, 1977.
#'
#' @seealso \code{\link{makePriceN1Function}}, \code{\link{makePriceN2Function}},
#' \code{\link{makePriceN4Function}}
#'
#' @template ret_smoof_single
#' @export
makePriceN3Function = function() {
  makeSingleObjectiveFunction(
    name = "Price Function N. 3",
    fn = function(x) {
      100 * (x[2] - x[1]^2)^2 + 6 * (6.4 * (x[2] - 0.5)^2 - x[1] - 0.6)^2
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-500, -500),
      upper = c(500, 500),
      vector = FALSE
    ),
    tags = attr(makePriceN3Function, "tags"),
    global.opt.params = matrix(
      c(5, 5,
        -5, 5,
        5, -5,
        -5, 5),
      ncol = 2L, byrow = TRUE),
    global.opt.value = 0
  )
}

class(makePriceN3Function) = c("function", "smoof_generator")
attr(makePriceN3Function, "name") = c("Price Function N. 3")
attr(makePriceN3Function, "type") = c("single-objective")
attr(makePriceN3Function, "tags") = c("continuous", "differentiable", "non-separable", "non-scalable", "multimodal")
