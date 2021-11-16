#' Generator for function with multiple peaks following the multiple peaks model 2.
#'
#' @param n.peaks [\code{integer(1)}]\cr
#'   Desired number of peaks, i. e., number of (local) optima.
#' @template arg_dimensions
#' @param topology [\code{character(1)}]\cr
#'   Type of topology. Possible values are \dQuote{random} and \dQuote{funnel}.
#' @param seed [\code{integer(1)}]\cr
#'   Seed for the random numbers generator.
#' @param rotated [\code{logical(1)}]\cr
#'   Should the peak shapes be rotated? This parameter is only relevant in case
#'   of elliptically shaped peaks.
#' @param peak.shape [\code{character(1)}]\cr
#'   Shape of peak(s). Possible values are \dQuote{ellipse} and \dQuote{sphere}.
#' @return [\code{smoof_single_objective_function}]
#' @examples
#' \dontrun{
#' fn = makeMPM2Function(n.peaks = 10L, dimensions = 2L,
#'   topology = "funnel", seed = 123, rotated = TRUE, peak.shape = "ellipse")
#' if (require(plot3D)) {
#'   plot3D(fn)
#' }
#' }
#' \dontrun{
#' fn = makeMPM2Function(n.peaks = 5L, dimensions = 2L,
#'   topology = "random", seed = 134, rotated = FALSE)
#' plot(fn, render.levels = TRUE)
#' }
#'
#' @references See the technical report of multiple peaks model 2 for an in-depth
#' description of the underlying algorithm.
#'
#' @author \R interface by Jakob Bossek. Original python code provided by the Simon Wessing.
#'
#' @export
makeMPM2Function = function(n.peaks, dimensions, topology, seed, rotated = TRUE, peak.shape = "ellipse") {
  if (isWindows()) {
    stopf("No support for the multiple peaks model 2 generator at the moment.")
  }

  # n.peaks = 1L; dimensions = 2L; topology = "funnel"; seed = 3L; rotated = TRUE; peak.shape = "ellipse"
  # do some sanity checks
  n.peaks = convertInteger(n.peaks)
  dimensions = convertInteger(dimensions)
  seed = convertInteger(seed)
  assertInt(n.peaks, lower = 1L)
  assertInt(dimensions, lower = 1L)
  assertChoice(topology, choices = c("random", "funnel"))
  assertInt(seed, lower = 1L)
  assertLogical(rotated, any.missing = FALSE)
  assertChoice(peak.shape, choices = c("ellipse", "sphere"))

  # touch vars
  force(n.peaks)
  force(dimensions)
  force(topology)
  force(seed)
  force(rotated)
  force(peak.shape)

  # build parameter set (bounds are [0, 1]^d)
  par.set = makeNumericParamSet("x", len = dimensions, lower = 0, upper = 1)

  # import reticulate namespace
  BBmisc::requirePackages("_reticulate", why = "smoof::makeMultiplePeaksModel2Function")

  # initialize 3 functions from mpm2.py as NULL such that they have a visible binding when checking the pkg
  evaluateProblem = getGlobalOptimaParams = getLocalOptimaParams = NULL

  # load funnel generator to global environment
  eval(reticulate::py_run_file(system.file("mpm2.py", package = "smoof")), envir = .GlobalEnv)
  eval(reticulate::source_python(system.file("mpm2.py", package = "smoof"), envir = .GlobalEnv, convert = TRUE), envir = .GlobalEnv)

  # extract local and global optima
  local.opt.params = eval(getLocalOptimaParams(n.peaks, dimensions, topology, seed, rotated, peak.shape))
  local.opt.params = do.call(rbind, lapply(local.opt.params, unlist))

  global.opt.params = eval(getGlobalOptimaParams(n.peaks, dimensions, topology, seed, rotated, peak.shape), envir = .GlobalEnv)
  global.opt.params = matrix(global.opt.params[[1L]], nrow = 1L)

  smoof.fn = makeSingleObjectiveFunction(
    name = sprintf("Funnel_%i_%i_%i_%s_%s%s", n.peaks, dimensions, seed, topology, peak.shape, ifelse(rotated, "_rotated", "")),
    description = sprintf("Funnel-like function\n(n.peaks: %i, dimension: %i, topology: %s, seed: %i, rotated: %s, shape: %s)",
      n.peaks, dimensions, topology, seed, rotated, peak.shape),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      evaluateProblem(as.double(x), n.peaks, dimensions, topology, seed, rotated, peak.shape)
    },
    par.set = par.set,
    tags = c("non-separable", "scalable", "continuous", "multimodal"),
    local.opt.params = local.opt.params,
    global.opt.params = global.opt.params
  )
  return(smoof.fn)
}

class(makeMPM2Function) = c("function", "smoof_generator")
attr(makeMPM2Function, "name") = c("Multiple peaks model 2 function generator")
attr(makeMPM2Function, "type") = c("single-objective")
