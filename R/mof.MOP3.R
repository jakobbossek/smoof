#' @title
#' MOP3 function generator.
#'
#' @description
#' MOP3 function from Van Valedhuizen's test suite.
#'
#' @references
#' C. Poloni, G. Mosetti, and S. Contessi, "Multi objective optimization by
#' GAs: Application to system and component design," in Proc. Comput.
#' Methods in Applied Sciences'96: Invited Lectures and Special Technological
#' Sessions of the 3rd ECCOMAS Comput. Fluid Dynamics Conf.
#' and the 2nd ECCOMAS Conf. Numerical Methods in Engineering, Sep.
#' 1996, pp. 258-264
#' @template arg_dimensions
#' @return [\code{smoof_multi_objective_function}]
#' @export
makeMOP3Function = function(dimensions = 2L) {
  assertInt(dimensions, lower = 1L)
  force(dimensions)

  # C implementation
  fn = function(x) {
    assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
    return(.Call("mof_MOP3", x))
  }

  makeMultiObjectiveFunction(
    name = "MOP3 function",
    id = sprintf("MOP3-%id-%io", dimensions, 2L),
    description = "MOP3 function",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = rep(pi, dimensions),
      upper = rep(pi, dimensions),
      vector = TRUE
    ),
    minimize = rep(FALSE, dimensions), # MOP3 is the only MOP which shall be maximized by default
    n.objectives = 2L
  )
}

class(makeMOP3Function) = c("function", "smoof_generator")
attr(makeMOP3Function, "name") = c("MOP3")
attr(makeMOP3Function, "type") = c("multi-objective")
attr(makeMOP3Function, "tags") = c("multi-objective")
