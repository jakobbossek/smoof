#' Michalewicz function
#'
#' Highly multimodal single-objective test function with \eqn{n!} local minima
#' with the formula:
#' \deqn{f(\mathbf{x}) = -\sum_{i=1}^{n} \sin(\mathbf{x}_i) \cdot \left(\sin\left(\frac{i \cdot \mathbf{x}_i}{\pi}\right)\right)^{2m}.}
#' The recommended value \eqn{m = 10}, which is used as a default in the
#' implementation. The location of the global optimum s varying based on both
#' the dimension and \eqn{m} parameter and is thus not provided in the
#' implementation.
#'
#' @references Michalewicz, Z.: Genetic Algorithms + Data Structures = Evolution
#' Programs. Berlin, Heidelberg, New York: Springer-Verlag, 1992.
#'
#' @template arg_dimensions
#' @param m [\code{integer(1)}]\cr
#'   \dQuote{Steepness} parameter.
#' @template ret_smoof_single
#' @export
makeMichalewiczFunction = function(dimensions, m = 10) {
  assertCount(dimensions, na.ok = FALSE)
  assertNumber(m, na.ok = FALSE)
  force(m)
  makeSingleObjectiveFunction(
    name = "Michalewicz function",
    fn = function(x) {
      i = 1:length(x)
      (-1) * sum(sin(x) * (sin((i * x^2) / pi)^(2 * m)))
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(0, 0),
      upper = c(pi, pi),
      vector = FALSE
    ),
    tags = c("continuous", "multimodal")
  )
}
