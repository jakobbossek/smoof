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

  # import rPython namespace
  BBmisc::requirePackages("_rPython", why = "smoof::makeMultiplePeaksModel2Function")

  # load funnel generator to global environemt
  eval(rPython::python.load(system.file("mpm2.py", package = "smoof")), envir = .GlobalEnv)

  local.opt.params = eval(rPython::python.call("getLocalOptimaParams", n.peaks, dimensions, topology, seed, rotated, peak.shape))
  if (n.peaks == 1)
    local.opt.params = list(local.opt.params)
  local.opt.params = do.call(rbind, local.opt.params)
  global.opt.params = eval(rPython::python.call("getGlobalOptimaParams", n.peaks, dimensions, topology, seed, rotated, peak.shape))

  smoof.fn = makeSingleObjectiveFunction(
    name = sprintf("Funnel_%i_%i_%i_%s_%s%s", n.peaks, dimensions, seed, topology, peak.shape, ifelse(rotated, "_rotated", "")),
    description = sprintf("Funnel-like function\n(n.peaks: %i, dimension: %i, topology: %s, seed: %i, rotated: %s, shape: %s)",
      n.peaks, dimensions, topology, seed, rotated, peak.shape),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      smoof.python.call("evaluateProblem", as.double(x), n.peaks, dimensions, topology, seed, rotated, peak.shape)
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


smoof.python.call = function (py.foo, ..., simplify = TRUE, as.is = FALSE)
{
    foo.args <- list(...)
    if (is.null(names(foo.args)))
        which.dict <- rep(FALSE, length(foo.args))
    else which.dict <- names(foo.args) != ""
    n.args.vect <- sum(!which.dict)
    n.args.dict <- sum(which.dict)
    foo.args.dict <- RJSONIO::toJSON(foo.args[which.dict], digits = 12, collapse = " ",
        asIs = as.is)
    foo.args.vect <- RJSONIO::toJSON(foo.args[!which.dict], digits = 12, collapse = " ",
        asIs = as.is)
    python.command <- c(paste("_r_args_dict =r'''", foo.args.dict,
        "'''", sep = ""), paste("_r_args_vect =r'''", foo.args.vect,
        "'''", sep = ""), "_r_args_dict = json.loads( _r_args_dict )",
        "_r_args_vect = json.loads( _r_args_vect )", python.command <- paste("_r_call_return = ",
            py.foo, "(", ifelse(n.args.vect == 1, "_r_args_vect[0]",
                "*_r_args_vect"), ifelse(n.args.dict == 0, ")",
                ", **_r_args_dict)"), sep = ""))
    python.command <- paste(python.command, collapse = "\n")
    rPython::python.exec(python.command)
    ret <- rPython::python.get("_r_call_return")
    if (length(ret) == 1 && simplify)
        ret <- ret[[1]]
    ret
}
