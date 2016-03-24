#' @title
#' BK1 function generator
#'
#' @description
#' ...
#'
#' @references
#' ...
#'
#' @return [\code{smoof_multi_objective_function}]
#' @export
makeBK1Function = function() {

  # C++ implementation
  fn = function(x) {
    assertNumeric(x, len = 2L, any.missing = FALSE, all.missing = FALSE)
    return(.Call("mof_bk1", x))
  }

  makeMultiObjectiveFunction(
    name = "BK1 Function",
    id = "bk1",
    description = "BK1 function",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = rep(-5, 2L),
      upper = rep(10, 2L),
      vector = TRUE
      ),
    n.objectives = 2L#,
    #ref.point = rep(11, n.objectives)
  )
}

class(makeBK1Function) = c("function", "smoof_generator")
attr(makeBK1Function, "name") = c("BK1")
attr(makeBK1Function, "type") = c("multi-objective")
attr(makeBK1Function, "tags") = c("multi-objective")
