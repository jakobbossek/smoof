#' Combinatorial NK-landscapes
#'
#' Generate a rNK-landscape function
#'
#' @references
#'
#' @parma M [\code{integer(1)}]\cr
#'   Number of objectives (at least one).
#' @param N [\code{integer(1)}]\cr
#'   Length of the bit-string (decision space dimension).
#' @param K [\code{integer}]\cr
#'   Integer vector of the number of epistatic interactions.
#'   If a single value is passed a homogeneous NK-landscape is generated, i.e.
#'   \eqn{k_i = k \forall i = 1, \ldots, N}.
#' @param rho [\code{numeric(1)}]\cr
#'   Correlation between table contributions.
#' @param distr [\code{function}]\cr
#'   Not used at the moment.
#'
#' @return [\code{smoof_single_objective_function}]
#' @export
makeRNKFunction = function(N, K, rho, distr = NULL) {
  N = checkmate::asInt(N)
  checkmate::asssertInteger(K, lower = 0, upper = N - 1L)

  # for consistency
  if (length(K) == 1) {
    K_string = as.character(K)
    K = rep(K, N)
  } else {
    # FIXME: finish! Want: heterogeneous: 4, 5, 3, ...
    K_string = sprintf("heterogenous: %s", "test")
  }
  checkmate::assertNumber(rho)

  links_and_values = generateRMNKFunction(M = 1L, N, K, rho)
  links = links_and_values$links
  values = links_and_values$values

  force(N)
  force(K)
  force(links)
  force(values)

  fn = function(x) {
    res = sapply(seq_len(N), function(i) {
      # get active links, i.e., those that are 1
      active_links = as.integer(x[links[[i]] + 1] == 1)
      active_links = rev(active_links)
      offset = bitstringToInt(active_links)
      values[i, offset + 1]
    })
    sum(res) / N
  }

  makeSingleObjectiveFunction(
    name = sprintf("rMNK-landscape (M=%i, N=%i, k=%s", 1L, N, K_string),
    id = "rMNK",
    fn = fn,
    par.set = makeIntegerParamSet(
      len = N,
      id = "x",
      lower = rep(0, N),
      upper = rep(1, N),
      vector = TRUE
    ),
    tags = attr(makeRMNKFunction, "tags")
  )
}

# helper to calculate offset for values table
bitstringToInt = function(x) {
  packBits(rev(c(rep(FALSE, 32 - length(x) %% 32), as.logical(x))), "integer")
}

# helper to actually create links and values for given rMNK parameters
generateRMNKFunction = function(M, N, K, rho) {
  # generate epistatic links
  idxs = 0:(N - 1L) # FIXME: use 1, ..., N instead?
  links = lapply(seq_len(N), function(i) {
    sample(idxs[-i], size = K[i], replace = FALSE)
  })

  # correlation matrix
  Sigma = matrix(rho, M, M)
  diag(Sigma) = 1
  Sigma = 2 * sin(pi / 6 * Sigma)

  # vector of mean values
  mu = rep(0, M)

  # generate values
  # FIXME: need to take heterogeneous K into consideration
  values = pnorm(MASS::mvrnorm(n = N * 2^(K + 1), mu = mu, Sigma = Sigma))

  return(list(links = links, values = values))
}


makeRMNKFunction = function(funs) {

}

class(makeNKFunction) = c("function", "smoof_generator")
attr(makeNKFunction, "name") = c("rMNK")
attr(makeNKFunction, "type") = c("single-objective")
attr(makeNKFunction, "tags") = c("single-objective", "combinatorial")

exportNKFunction = function(path) {

}

importNKFunction = function(path) {

}
