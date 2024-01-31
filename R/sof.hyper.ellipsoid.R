#' @title
#' Hyper-Ellipsoid function
#'
#' @description
#' Uni-modal, convex test function similar to the Sphere function (see \code{\link{makeSphereFunction}}).
#' Calculated via the formula: \deqn{f(\mathbf{x}) = \sum_{i=1}^{n} i \cdot \mathbf{x}_i.}
#' 
#' @return
#' An object of class \code{SingleObjectiveFunction}, representing the Hyper-Ellipsoid Function.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeHyperEllipsoidFunction = function(dimensions) {
  checkmate::assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Hyper-Ellipsoid function", sep = ""),
    id = paste0("hyperEllipsoid_", dimensions, "d"),
    fn = function(x) {
      checkNumericInput(x, dimensions)
      #FIXME: check if this is correct. http://www.geocities.ws/eadorio/mvf.pdf has another definiton
      n = length(x)
      sum(1:n * x^2)
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5.12, dimensions),
      upper = rep(5.12, dimensions),
      vector = TRUE
    ),
    tags = attr(makeHyperEllipsoidFunction, "tags"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

class(makeHyperEllipsoidFunction) = c("function", "smoof_generator")
attr(makeHyperEllipsoidFunction, "name") = c("Hyper-Ellipsoid")
attr(makeHyperEllipsoidFunction, "type") = c("single-objective")
attr(makeHyperEllipsoidFunction, "tags") = c("single-objective", "unimodal", "convex", "continuous", "scalable")
