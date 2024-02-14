#' @title
#' Swiler2014 function.
#'
#' @description Mixed parameter space with one discrete parameter \eqn{x_1 \in \{1, 2, 3, 4, 5\}}
#' and two numerical parameters \eqn{x_1, x_2 \in [0, 1]}. The function is defined
#' as follows:
#' \deqn{
#' f(\mathbf{x}) = \\
#' \sin(2\pi x_3 - \pi) + 7 \sin^2(2 \pi x_2 - \pi) \, if \, x_1 = 1 \\
#' \sin(2\pi x_3 - \pi) + 7 \sin^2(2 \pi x_2 - \pi) + 12 \sin(2 \pi x_3 - \pi) \, if \, x_1 = 2 \\
#' \sin(2\pi x_3 - \pi) + 7 \sin^2(2 \pi x_2 - \pi) + 0.5 \sin(2 \pi x_3 - \pi) \, if \, x_1 = 3 \\
#' \sin(2\pi x_3 - \pi) + 7 \sin^2(2 \pi x_2 - \pi) + 8.0 \sin(2 \pi x_3 - \pi) \, if \, x_1 = 4 \\
#' \sin(2\pi x_3 - \pi) + 7 \sin^2(2 \pi x_2 - \pi) + 3.5 \sin(2 \pi x_3 - \pi) \, if \, x_1 = 5.
#' }
#'
#' @template ret_smoof_single
#' @export
makeSwiler2014Function = function() {
  makeSingleObjectiveFunction(
    name = "Swiler2014 Function",
    id = paste0("swiler2014_3d"),
    fn = function(x) {
      checkmate::assertList(x)
      x1 = x$x1
      x2 = x$x2
      x3 = x$x3
      checkmate::assertChoice(x1, choices = c("1", "2", "3", "4", "5"))
      checkmate::assertNumber(x2, lower = 0, upper = 1)
      checkmate::assertNumber(x3, lower = 0, upper = 1)

      a = sin(2 * pi * x3 - pi)
      b = 7 * sin(2 * pi * x2 - pi)^2
      facs = c(0, 12.0, 0.5, 8.0, 3.5)

      fac = facs[as.integer(x1)]
      val = a + b + fac * sin(2 * pi * x3 - pi)
      return(val)
    },
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeDiscreteParam("x1", values = c("1", "2", "3", "4", "5")),
      ParamHelpers::makeNumericParam("x2", lower = 0, upper = 1),
      ParamHelpers::makeNumericParam("x3", lower = 0, upper = 1)
    ),
    tags = attr(makeSwiler2014Function, "tags"),
    has.simple.signature = FALSE
    # global.opt.params = c(1, 0),
    # global.opt.value = 0
  )
}

class(makeSwiler2014Function) = c("function", "smoof_generator")
attr(makeSwiler2014Function, "name") = c("Swiler2014")
attr(makeSwiler2014Function, "type") = c("single-objective")
attr(makeSwiler2014Function, "tags") = c("single-objective", "discontinuous", "multimodal")
