#' Generator for the function set of the real-parameter Bi-Objective
#' Black-Box Optimization Benchmarking (BBOB) with Function IDs 1-55, as well as
#' its extended version (bbob-biobj-ext) with Function IDs 1-92.
#'
#' @note
#' Concatenation of single-objective BBOB functions into
#' a bi-objective problem.
#'
#' @param dimensions [\code{integer(1)}]\cr
#'   Problem dimensions. Integer value between 2 and 40.
#' @param fid [\code{integer(1)}]\cr
#'   Function identifier. Integer value between 1 and 92.
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
#' A full description of the extended suite with 92 functions can be found
#' \href{http://numbbo.github.io/coco-doc/bbob-biobj/functions/#the-extended-bbob-biobj-ext-test-suite-and-its-functions}{here}.
#' @export
makeBiObjBBOBFunction = function(dimensions, fid, iid) {
  
  # ==== Sanity Checks ====
  
  dimensions = asCount(dimensions)
  fid = asCount(fid)
  iid = asCount(iid)
  assertInt(dimensions, lower = 2L, upper = 40L)
  assertInt(fid, lower = 1L, upper = 92L)
  assertInt(iid, lower = 1L, upper = 15L) # restrict to documented "safe" range

  # touch vars
  force(dimensions)
  force(fid)
  force(iid)
  
  # ==== FID Mapping ====

  # single-objective BBOB functions, which are used by bi-objective BBOB
  fids = c(1L, 2L, 6L, 8L, 13L, 14L, 15L, 17L, 20L, 21L)
  
  # grid with all pairs of BBOB problems
  grid = expand.grid(fids1 = fids, fids2 = fids)
  grid = grid[grid[, 1L] <= grid[, 2L], ]
  grid = grid[order(grid[, 1L]),]
  
  # Add all off-diagonal combinations per BBOB group (excluding FID 16)
  # for the extended bi-objective suite
  # cf. http://numbbo.github.io/coco-doc/bbob-biobj/functions/#the-extended-bbob-biobj-ext-test-suite-and-its-functions
  for (group in list(1L:5L, 6L:9L, 10L:14L, setdiff(15L:19L, 16L), 20L:24L)) {
    group.grid = expand.grid(fids1 = group, fids2 = group)
    group.grid = group.grid[group.grid[, 1L] < group.grid[, 2L], ]
    group.grid = group.grid[order(group.grid[, 1L]),]
    
    grid = rbind(grid, group.grid)
    grid = grid[!duplicated(grid),]
  }
  
  rownames(grid) = NULL

  fid1 = grid[fid, "fids1"]
  fid2 = grid[fid, "fids2"]
  
  # ==== IID Mapping ====

  # Regularly, single objective IIDs are computed as
  # IID_1 = 2 * IID + 1, and
  # IID_2 = IID_1 + 1.
  # (http://numbbo.github.io/coco-doc/bbob-biobj/functions/#instances)
  # 
  # However, there are some exceptions either for historical reasons (IIDs 1, 2)
  # or because optima are too close to each other in decision or objective space
  # (IIDs 9, 15). Here, we restrict the list to the tested IIDs (up to IID 15)
  # from the COCO source code.
  # https://github.com/numbbo/coco/blob/29ac4063cea8cf74257e2a0671a6cafc4d5e7752/code-experiments/src/suite_biobj_utilities.c#L23-L39
  max_iid = 15L
  
  vec_iid_1 = 2 * (1:max_iid) + 1
  vec_iid_2 = vec_iid_1 + 1
  
  iid_mapping = cbind(vec_iid_1, vec_iid_2)
  
  # exceptions, cf. above
  iid_mapping[1L,] = c(2L, 4L)
  iid_mapping[2L,] = c(3L, 5L)
  iid_mapping[9L,] = c(19L, 21L)
  iid_mapping[15L,] = c(31L, 34L)
  
  iid1 = iid_mapping[iid,1]
  iid2 = iid_mapping[iid,2]
  
  # ==== Build smoof function ====
  
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
