#' @title
#' Schwefel function
#'
#' @description
#' Highly multi-modal test function. The crucial thing about this function is, that
#' the global optimum is far away from the next best local optimum.
#' The function is computed via:
#' \deqn{f(\mathbf{x}) = \sum_{i=1}^{n} -\mathbf{x}_i \sin\left(\sqrt(|\mathbf{x}_i|)\right)}
#' with \eqn{\mathbf{x}_i \in [-500, 500], i = 1, \ldots, n.}
#' 
#' @return
#' An object of class \code{SingleObjectiveFunction}, representing the Schwefel Function.
#'
#' @references Schwefel, H.-P.: Numerical optimization of computer models.
#' Chichester: Wiley & Sons, 1981.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeSchwefelFunction = function(dimensions) {
  checkmate::assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Schwefel function", sep = ""),
    id = paste0("schwefel_", dimensions, "d"),
    fn = function(x) {
      checkNumericInput(x, dimensions)
      sum(-x * sin(sqrt(abs(x))))
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-500, dimensions),
      upper = rep(500, dimensions),
      vector = TRUE
    ),
    tags = c("continuous", "multimodal"),
    global.opt.params = rep(420.9687, dimensions),
    global.opt.value = -418.9829 * dimensions
  )
}

class(makeSchwefelFunction) = c("function", "smoof_generator")
attr(makeSchwefelFunction, "name") = c("Schwefel")
attr(makeSchwefelFunction, "type") = c("single-objective")
attr(makeSchwefelFunction, "tags") = c("single-objective", "continuous", "multimodal", "scalable")
