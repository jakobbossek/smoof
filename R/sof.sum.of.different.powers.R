#' Sum of different squares function
#'
#' Simple unimodal test function similar to the Sphere and Hyper-Ellipsoidal functions.
#' Formula:
#' \deqn{f(\mathbf{x}) = \sum_{i=1}^{n} |\mathbf{x}_i|^{i+1}.}
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeSumOfDifferentSquaresFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Sum of different squares function", sep = ""),
    fn = function(x) {
      n = length(x)
      sum(abs(x)^(1:n + 1))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-1, dimensions),
      upper = rep(1, dimensions),
      vector = FALSE
    ),
    tags = c("unimodal", "continuous"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}
