#' @title
#' Helper function to create numeric single-objective optimization test function.
#'
#' @description
#' This is a simplifying wrapper around \code{\link{makeSingleObjectiveFunction}}.
#' It can be used if the function to generte is purely numeric to save some lines
#' of code.
#'
#' @inheritParams makeSingleObjectiveFunction
#' @template arg_par.len
#' @template arg_par.id
#' @template arg_par.lower
#' @template arg_par.upper
#' @examples
#' # first we generate the 10d sphere function the long way
#' fn = makeSingleObjectiveFunction(
#'   name = "Testfun",
#'   fn = function(x) sum(x^2),
#'   par.set = makeNumericParamSet(
#'     len = 10L, id = "a",
#'     lower = rep(-1.5, 10L), upper = rep(1.5, 10L)
#'   )
#' )
#'
#' # ... and now the short way
#' fn = snof(
#'  name = "Testfun",
#'  fn = function(x) sum(x^2),
#'  par.len = 10L, par.id = "a", par.lower = -1.5, par.upper = 1.5
#' )
#' @export
snof = function(name = NULL,
  id = NULL,
  par.len = NULL,
  par.id = "x",
  par.lower = NULL,
  par.upper = NULL,
  description = NULL,
  fn,
  vectorized = FALSE,
  noisy = FALSE,
  fn.mean = NULL,
  minimize = TRUE,
  constraint.fn = NULL,
  tags = character(0),
  global.opt.params = NULL,
  global.opt.value = NULL,
  local.opt.params = NULL,
  local.opt.values = NULL) {

  checkmate::assertString(par.id, null.ok = TRUE)
  par.len = checkmate::asCount(par.len)

  # furhter checks are performed by ParamHelpers
  if (is.null(par.lower))
    par.lower = -Inf
  if (is.null(par.upper))
    par.upper = Inf
  checkmate::assertNumeric(par.lower, min.len = 1L)
  checkmate::assertNumeric(par.upper, min.len = 1L)

  makeSingleObjectiveFunction(
    name = name,
    id = id,
    description = description,
    fn = fn,
    has.simple.signature = TRUE, # numeric funs always have a simple signature
    vectorized = vectorized,
    noisy = noisy,
    fn.mean = fn.mean,
    minimize = minimize,
    constraint.fn = constraint.fn,
    par.set = ParamHelpers::makeNumericParamSet(
      len = par.len,
      id = par.id,
      lower = par.lower,
      upper = par.upper,
      vector = TRUE
    ),
    tags = tags,
    global.opt.params = global.opt.params,
    global.opt.value = global.opt.value,
    local.opt.params = local.opt.params,
    local.opt.values = local.opt.values
  )
}
