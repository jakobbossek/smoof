#' @title
#' MOP4 function generator.
#'
#' @description
#' MOP4 function from Van Valedhuizen's test suite based on Kursawe.
#'
#' @references
#' F. Kursawe, "A variant of evolution strategies for vector optimization,"
#' in Lecture Notes in Computer Science, H.-P. Schwefel and R. Maenner,
#' Eds. Berlin, Germany: Springer-Verlag, 1991, vol. 496, Proc. Parallel
#' Problem Solving From Nature. 1st Workshop, PPSN I, pp. 193-197.
#'
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the MOP4 function as a \code{smoof_multi_objective_function} object.
#' @export
makeMOP4Function = function() {

  # C implementation
  fn = function(x) {
    checkNumericInput(x, 3L)
    .Call("mof_MOP4", x)
  }

  makeMultiObjectiveFunction(
    name = "MOP4 function",
    id = sprintf("MOP4-%id-%io", 3L, 2L),
    description = "MOP4 function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = 3L,
      id = "x",
      lower = rep(-5, 3L),
      upper = rep(5, 3L),
      vector = TRUE
    ),
    n.objectives = 2L
  )
}

class(makeMOP4Function) = c("function", "smoof_generator")
attr(makeMOP4Function, "name") = c("MOP4")
attr(makeMOP4Function, "type") = c("multi-objective")
attr(makeMOP4Function, "tags") = c("multi-objective")
