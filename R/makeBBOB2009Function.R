#' @export
makeBBOB2009Function = function(dimension, fid, iid) {
  assertInt(dimension, lower = 2L, upper = 40L, na.ok = FALSE)
  assertInt(fid, lower = 1L, upper = 24L, na.ok = FALSE)
  assertInt(iid, lower = 1L, upper = 24L, na.ok = FALSE)

  # touch vars
  force(dimension)
  force(fid)
  force(iid)

  # get optimal values
  optimals = getOptimumForBBOBFunction(dimension, fid, iid)
  par.set = makeNumericParamSet("x", len = 2L, lower = -5, upper = 5)
  names(optimals$param) = getParamIds(par.set, with.nr = TRUE, repeated = TRUE)
  ooo <<- optimals

  makeSingleObjectiveFunction(
    name = sprintf("BBOB_%i_%i_%i", 2L, fid, iid),
    fn = function(x) {
      evaluateBBOBFunctionCPP(dimension, fid, iid, x)
    },
    # bounds are [-5, 5] for all BBOB funs
    par.set = par.set,
    #FIXME: add optimal params and global minimum function value
    global.opt.params = optimals$param,
    global.opt.value = optimals$value
  )
}

getOptimumForBBOBFunction = function(dimension, fid, iid) {
  getOptimumForBBOBFunctionCPP(dimension, fid, iid)
}
