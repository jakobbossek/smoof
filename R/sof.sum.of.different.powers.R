#' @title
#' Sum of Different Squares Function
#'
#' @description
#' Simple uni-modal test function similar to the Sphere and Hyper-Ellipsoidal functions.
#' Formula:
#' \deqn{f(\mathbf{x}) = \sum_{i=1}^{n} |\mathbf{x}_i|^{i+1}.}
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeSumOfDifferentSquaresFunction = function(dimensions) {
  checkmate::assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Sum of Different Squares Function", sep = ""),
    id = paste0("sumOfDifferentSquares_", dimensions, "d"),
    fn = function(x) {
      checkNumericInput(x, dimensions)
      n = length(x)
      sum(abs(x)^(1:n + 1))
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-1, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
    tags = attr(makeSumOfDifferentSquaresFunction, "tags"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

class(makeSumOfDifferentSquaresFunction) = c("function", "smoof_generator")
attr(makeSumOfDifferentSquaresFunction, "name") = c("Sum of Different Squares")
attr(makeSumOfDifferentSquaresFunction, "type") = c("single-objective")
attr(makeSumOfDifferentSquaresFunction, "tags") = c("single-objective", "unimodal", "continuous", "scalable")
