# TODOS
# ===
# * Check convertBitstringToInt (took this from OPs implementation, but I beleive it is wrong now)
# * Finalise documentation:
# * attributes are not moved/copied and thus not accessible in wrappers. Thus, we cannot export
#   a wrapped function. Should we access the wrapped function in exportNKFunction?
# * makeRMNKFunction: we should be able to pass a list of vectors (each one k or even N different
#   K's for each objective).
# * Handle K_string stuff in make* and export* functions (i.e., how to display this information)
# * Polish code and add more meaningful explanations
# * Add comprehensive description/details sections for makeNKFunction and makeRMNKFunction
# * Add examples
# * Add tests (also with counting- and logging-wrappers)

#' Combinatorial NK-landscapes
#'
#' Generate a rNK-landscape function
#'
#' @references
#'
#' @param N [\code{integer(1)}]\cr
#'   Length of the bit-string (decision space dimension).
#' @param K [\code{integer}]\cr
#'   Integer vector of the number of epistatic interactions.
#'   If a single value is passed a homogeneous NK-landscape is generated, i.e.
#'   \eqn{k_i = k \forall i = 1, \ldots, N}.
#' @param distr [\code{function}]\cr
#'   Not used at the moment.
#'
#' @return [\code{smoof_single_objective_function}]
#' @export
makeNKFunction = function(N, K, distr = NULL) {
  N = checkmate::asInt(N)
  checkmate::assertInteger(K, lower = 0, upper = N - 1L)

  # for consistency
  if (length(K) == 1L) {
    K_string = as.character(K)
    K = rep(K, N)
  } else {
    # FIXME: finish! Want: heterogeneous: 4, 5, 3, ...
    K_string = sprintf("heterogenous: %s", "test")
  }

  # delegate to flexible generator
  links_and_values = generateRMNKFunction(M = 1L, N, K, rho = 0)
  links = links_and_values$links[[1L]]
  values = links_and_values$values[[1L]]

  fn = makeNKFunctionInternal(N, K, links, values)
  attr(fn, "links_and_values") = links_and_values
  return(fn)
}


makeNKFunctionInternal = function(N, K, links, values) {
  force(N)
  force(K)
  force(links)
  force(values)

  fn = function(x) {
    # for every bit position ...
    res = sapply(seq_len(N), function(i) {
      # ... get active links, i.e., those that have a value of 1
      active_links = as.integer(x[links[[i]] + 1] == 1) # +1 since epistatic links are zero-based
      # ... convert the bitstring to an integer offset of access the value table
      active_links = rev(active_links)
      offset = convertBitstringToInt(active_links)
      # ... and return the stored value
      values[[i]][offset + 1L]
    })
    # normalise
    sum(res) / N
  }

  # FIXME: id should be random since the function values depend on seed?
  # FIXME: also the name should include some kind of hash
  makeSingleObjectiveFunction(
    name = sprintf("NK-landscape (N=%i, k=%s", N, "K_string"),
    id = "NK_landscape",
    fn = fn,
    par.set = makeParamSet(makeIntegerVectorParam(
      len = N,
      id = "x",
      lower = rep(0, N),
      upper = rep(1, N)
    ))#,
    #tags = attr(makeRMNKFunction, "tags")
  )
}

# helper to calculate offset for values table
convertBitstringToInt = function(x) {
  packBits(rev(c(rep(FALSE, 32 - length(x) %% 32), as.logical(x))), "integer")
}

# Internal helper function to actually create links and values for given rMNK parameters
#
# @param M [\code{integer(1)}]\cr
#   Number of objectives (at least two).
# @param N [\code{integer(1)}]\cr
#   Length of the bit-string (decision space dimension).
# @param K [\code{integer}]\cr
#   Integer vector of the number of epistatic interactions.
#   If a single value is passed a homogeneous NK-landscape is generated, i.e.
#   \eqn{k_i = k \forall i = 1, \ldots, N}.
# @param rho [\code{numeric(1)}]\cr
#   Correlation between table contributions.
#   Defaults to zero, i.e., no correlation at all.
# @return [\code{named list}]
# \describe{
#   \item[links]{A list of lists of epistatic values (one sub-list per objective). I.e.,
#   links[[m]][n] returns a vector of zero-based bit positions that the n-th bit interacts with.}
#   \item[values]{A list of lists of function values (one sub-list per objective). I.e.,
#   values[[m]][n] returns a vector of 2^(K[n] + 1) function values for all possible expression
#.  of the nth bit and it K[n] links.}
# }
#
generateRMNKFunction = function(M, N, K, rho) {
  # generate epistatic links
  # for each objective ...
  links = lapply(seq_len(M), function(m) {
    # ...and for each bit position generate random epistatic links
    idxs = seq_len(N)
    lapply(idxs, function(n) {
      sample(idxs[-n], size = K[n], replace = FALSE) - 1L # zero-based
    })
  })

  # correlation matrix
  Sigma = matrix(rho, M, M)
  diag(Sigma) = 1
  Sigma = 2 * sin(pi / 6 * Sigma)

  # vector of mean values
  mu = rep(0, M)

  # Generate values
  # Determine the number of tabular values required:
  # Each bit position i has K[i] links
  # -> there are 2^K[i] possible expressions of these values
  # -> the bit itself has two values
  # -> 2^(K[i] + 1) possibilities for the ith bit
  n_values = sum(2^(K + 1))
  values = pnorm(MASS::mvrnorm(n = n_values, mu = mu, Sigma = Sigma))

  # convert to list of form values[[M]][[N]][[K[...]]]
  values = lapply(seq_len(M), function(m) {
    tmp = vector(length = N, mode = "list")
    sizes = 2^(K + 1L)
    start = 1L
    for (n in seq_len(N)) {
      end = start + sizes[n] - 1L
      tmp[[n]] = values[start:end, m]
      start = end
    }
    return(tmp)
  })

  return(list(
    links = links,
    values = values,
    rho = rho,
    M = M,
    N = N,
    K = K
  ))
}

#' rMNK-generator
#'
#' Generates an NK-landscapes with input dimension \code{N}, output dimension \code{M}
#' and epistatic links/interactions between bits specified by the vector \code{K}.
#' The correlation between the objectives can be adjusted via the correlation
#' parameter \eqn{\rho \in [-1,1]}.
#'
#' @param M [\code{integer(1)}]\cr
#'   Number of objectives (at least two).
#' @param N [\code{integer(1)}]\cr
#'   Length of the bit-string (decision space dimension).
#' @param K [\code{integer}]\cr
#'   Integer vector of the number of epistatic interactions.
#'   If a single value is passed a homogeneous NK-landscape is generated, i.e.
#'   \eqn{k_i = k \forall i = 1, \ldots, N}.
#' @param rho [\code{numeric(1)}]\cr
#'   Correlation between table contributions.
#'   Defaults to zero, i.e., no correlation at all.
#' @return [\code{smoof_multi_objective_function}]
#'
#' @seealso \code{\link{makeNKFunction}}
#' @export
makeRMNKFunction = function(M, N, K, rho = 0) {
  M = checkmate::asInt(M, lower = 2L, upper = 20L)
  checkmate::assertNumber(rho, lower = -1, upper = 1)

  # for consistency
  if (length(K) == 1L) {
    K_string = as.character(K)
    K = rep(K, N)
  } else {
    # FIXME: finish! Want: heterogeneous: 4, 5, 3, ...
    K_string = sprintf("heterogenous: %s", "test")
  }

  # generate links and values
  # * returns a list of lists of links (each one list for each objective)
  # * returns a list of lists of value tables (each one list for each objective)
  links_and_values = generateRMNKFunction(M, N, K, rho)

  links = links_and_values$links
  values = links_and_values$values

  # generate M single-objective NK-landscape functions
  fns = sapply(seq_len(M), function(m) {
    makeNKFunctionInternal(N, K, links[[m]], values[[m]])
  })

  force(fns)

  # actual objective function
  fn = function(x) {
    sapply(fns, function(fn) fn(x))
  }

  # FIXME: id should be random since the function values depend on seed?
  # FIXME: also the name should include some kind of hash
  mfn = makeMultiObjectiveFunction(
    name = sprintf("rMNK-landscape (M=%i, N=%i, k=%s, %.3f", M, N, "K_string", rho),
    id = "rMNK_landscape",
    fn = fn,
    par.set = makeParamSet(makeIntegerVectorParam(
      len = N,
      id = "x",
      lower = rep(0, N),
      upper = rep(1, N)
    )),
    n.objectives = M#,
    #tags = attr(makeRMNKFunction, "tags")
  )
  attr(fn, "links_and_values") = links_and_values
  return(mfn)
}

class(makeNKFunction) = c("function", "smoof_generator", "nk_generator")
attr(makeNKFunction, "name") = c("rMNK")
attr(makeNKFunction, "type") = c("single-objective")
attr(makeNKFunction, "tags") = c("single-objective", "combinatorial")

#' Export (rM)NK-landscape function to file
#'
#' NK-landscapes and rMNK-landscapes are randomly generated combinatorial structures.
#' In contrast to continuous benchmark function it thus makes perfect sense to store
#' the landscape definitions in a text-based format.
#'
#' Note: the function overwrites existing files without asking.
#'
#' @param x [\code{smoof_function}]\cr
#'   NK-landscape of rMNK-landscape.
#' @param path [\code{character(1)}]\cr
#'   Path to file where the landscape shall be stored.
#' @return Silently returns \code{TRUE} on success.
#'
#' @seealso importNKFunction
#' @export
exportNKFunction = function(x, path) {
  checkmate::assertPathForOutput(path, overwrite = TRUE)

  if (!BBmisc::hasAttributes(x, "links_and_values"))
    BBmisc::stopf("[exportNKFunction] x is missing attribute 'links_and_values'. Obviously, this is not an NK- or rMNK-landscape.")

  file = file(path, open = 'w')
  on.exit(close(file))

  links_and_values = attr(x, "links_and_values")
  links = links_and_values$links
  values = links_and_values$values

  # FIXME: save these things in the function?
  rho = links_and_values$rho
  M = links_and_values$M
  N = links_and_values$N
  K = links_and_values$K

  # FIXME: how to get the package version in here?
  cat(sprintf("COMMENT: file generated with R package smoof v1.6.0, %s\n", format(Sys.time(), "%m/%d/%Y %H:%M:%S")), file = file)
  cat("COMMENT: links are random and identical for every objective function\n", file = file)
  # FIXME: heterogenous K
  cat(sprintf("rMNK %.5f %i %i %i\n", rho, M, N, K[1L]))

  # export epistatic links
  for (m in seq_len(M)) {
    for (n in seq_len(N)) {
      if (K[n] == 0L) {
        cat("-\n")
        next
      }
      cat(BBmisc::collapse(links[[m]][[n]], sep = " "), "\n", file = file)
    }
  }

  # export tabluar function values
  for (m in seq_len(M)) {
    for (n in seq_len(N)) {
      cat(BBmisc::collapse(values[[m]][[n]], sep = " ")," \n", file = file)
    }
  }

  return(invisible(TRUE))
}

importNKFunction = function(path) {

}
