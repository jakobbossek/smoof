#' Generator for the function set of the real-parameter Bi-Objective
#' Black-Box Optimization Benchmarking (BBOB).
#'
#' @note
#' Concatenation of single-objective BBOB functions into
#' a bi-objective problem.
#'
#' @param dimensions [\code{integer(1)}]\cr
#'   Problem dimensions. Integer value between 2 and 40.
#' @param fid [\code{integer(1)}]\cr
#'   Function identifier. Integer value between 1 and 55.
#' @param iid [\code{integer(1)}]\cr
#'   Instance identifier. Integer value greater than or equal 1.
#' @return [\code{smoof_multi_objective_function}]
#' @examples
#' # get the fifth instance of the concatenation of the
#' # 3D versions of sphere and Rosenbrock
#' fn = makeBiObjBBOBFunction(dimensions = 3L, fid = 4L, iid = 5L)
#' fn(c(3, -1, 0))
#' # compare to the output of its single-objective pendants
#' f1 = makeBBOBFunction(dimensions = 3L, fid = 1L, iid = 2L * 5L + 1L)
#' f2 = makeBBOBFunction(dimensions = 3L, fid = 8L, iid = 2L * 5L + 2L)
#' identical(fn(c(3, -1, 0)), c(f1(c(3, -1, 0)), f2(c(3, -1, 0))))
#' @references See the \href{http://numbbo.github.io/coco-doc/bbob-biobj/functions/}{COCO website}
#' for a detailed description of the bi-objective BBOB functions.
#' An overview of which pair of single-objective BBOB functions creates
#' which of the 55 bi-objective BBOB functions can be found \href{http://numbbo.github.io/coco-doc/bbob-biobj/functions/#the-bbob-biobj-test-functions-and-their-properties}{here}.
#' @export
makeBiObjBBOBFunction = function(dimensions, fid, iid) {
  # do some sanity checks
  dimensions = asCount(dimensions)
  fid = asCount(fid)
  iid = asCount(iid)
  assertInt(dimensions, lower = 2L, upper = 40L)
  assertInt(fid, lower = 1L, upper = 55L)
  assertInt(iid, lower = 1L)

  # touch vars
  force(dimensions)
  force(fid)
  force(iid)

  # single-objective BBOB functions, which are used by bi-objective BBOB
  fids = c(1L, 2L, 6L, 8L, 13L, 14L, 15L, 17L, 20L, 21L)
  
  # grid with all pairs of BBOB problems
  grid = expand.grid(fids1 = fids, fids2 = fids)
  grid = grid[grid[, 1L] <= grid[, 2L], ]
  grid = grid[order(grid[, 1L]),]
  rownames(grid) = NULL

  fid1 = grid[fid, "fids1"]
  fid2 = grid[fid, "fids2"]
  # according to the description from COCO (http://numbbo.github.io/coco-doc/bbob-biobj/functions/#instances)
  # the IID of the bi-objective BBOB problem is used for computing the IIDs of
  # the two underlying single-objective BBOB problems as given below
  iid1 = 2L * iid + 1L
  iid2 = iid1 + 1L

  # build parameter set (bounds are [-5, 5] for all BBOB funs)
  par.set = makeNumericParamSet("x", len = dimensions, lower = -5, upper = 5)

  makeMultiObjectiveFunction(
    name = sprintf("Bi-Objective BBOB_%i_%i_%i", dimensions, fid, iid),
    id = paste0("biobj_bbob_", dimensions, "d_2o"),
    description = sprintf("%i-th noiseless Bi-Objective BBOB function\n(FID: %i, IID: %i, DIMENSION: %i)",
      fid, fid, iid, dimensions),
    fn = function(x) {
      c(.Call("evaluateBBOBFunctionCPP", dimensions, fid1, iid1, x),
        .Call("evaluateBBOBFunctionCPP", dimensions, fid2, iid2, x))
    },
    par.set = par.set,
    n.objectives = 2L,
    # the single-objective BBOB functions are vectorized,
    # but not the combined one
    vectorized = FALSE
  )

}

class(makeBiObjBBOBFunction) = c("function", "smoof_generator")
attr(makeBiObjBBOBFunction, "name") = c("Set of noiseless Bi-Objective BOBB Function(s)")
attr(makeBiObjBBOBFunction, "type") = c("multi-objective")
attr(makeBiObjBBOBFunction, "tags") = c("multi-objective")
