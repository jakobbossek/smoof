#' Generator for funnel-like functions.
#'
#' @param n.peaks [\code{integer(1)}]\cr
#'   Desired number of peaks, i.e., number of optima.
#' @param dimension [\code{integer(1)}]\cr
#'   Problem dimension. Integer value between 2 and 40.
#' @param topology [\code{integer(1)}]\cr
#'   Function identifier. Integer value between 1 and 24.
#' @param seed [\code{integer(1)}]\cr
#'   Seed for the random numbers generator. Default is \code{NULL}.
#' @return [\code{smoof_single_objective_function}]
#' @examples
#' fn = makeFunnelFunction(n.peaks = 10L, dimension = 2L, topology = "funnel", seed = 123)
#' if (require(plot3D)) {
#'   plot3D(fn, contour = TRUE)
#' }
#' @references Bla
#'
#' @author \R interface by Jakob Bossek. Original python code
#' provided by the Simon Wessing and Mike Preuss.
#'
# @export
makeFunnelFunction = function(n.peaks, dimension, topology, seed = NULL) {
  # do some sanity checks
  n.peaks = convertInteger(n.peaks)
  dimension = convertInteger(dimension)
  seed = convertInteger(seed)
  assertInt(n.peaks, lower = 1L, na.ok = FALSE)
  assertInt(dimension, lower = 2L, na.ok = FALSE)
  assertChoice(topology, choices = c("random", "funnel"))
  assertInt(seed, lower = 1L, na.ok = FALSE)

  # touch vars
  force(n.peaks)
  force(dimension)
  force(topology)
  force(seed)

  # build parameter set (bounds are [0, 1]^d)
  par.set = makeNumericParamSet("x", len = dimension, lower = 0, upper = 1)

  # import rPython
  BBmisc::requirePackages("rPython", why = "smoof::makeFunnelFunction")

  force(path.to.python)

  makeSingleObjectiveFunction(
    name = sprintf("Funnel_%i_%i_%i_%s", n.peaks, dimension, seed, topology),
    description = sprintf("Funnel-like function\n(n.peaks: %i, dimension: %i, topology: %s, seed: %i)",
      n.peaks, dimension, topology, seed),
    fn = function(x) {
      #FIXME: ugly that we always have to call this first. Maybe add checks to python
      # code similar to the BBOB and UF stuff
      rPython::python.call("evaluateProblem", as.numeric(x), n.peaks, dimension, topology, seed)
    },
    par.set = par.set,
    tags = character(0)
  )
}

class(makeBBOBFunction) = c("function", "smoof_generator")
attr(makeBBOBFunction, "name") = c("Funnel function generator")
attr(makeBBOBFunction, "type") = c("single-objective")
