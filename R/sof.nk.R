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
#'
#' @examples
#' # generate homogeneous NK-landscape with each K=3 epistatic links
#' fn = makeNKFunction(10L, 3L)
#'
#' # generate heterogeneous NK-landscape where K is sampled from {2,3,4}
#' # uniformly at random
#' N = 20L
#' fn = makeNKFunction(N, K = sample(2:4, size = N, replace = TRUE))
#'
#' # evaluate function on some random bitstrings
#' bitstrings = matrix(sample(c(0, 1), size = 10 * N, replace = TRUE), ncol = N)
#' apply(bitstrings, fn)
#' @return [\code{smoof_single_objective_function}]
#' @export
makeNKFunction = function(N, K) {
  N = checkmate::asInt(N)
  checkmate::assertInteger(K, lower = 0, upper = N - 1L)
  K = prepareEpistaticLinks(M = 1L, N, K)

  # delegate to flexible generator
  links_and_values = generateRMNKFunction(M = 1L, N, K, rho = 0)

  makeNKFunctionInternal(links_and_values, m = 1L)
}

class(makeNKFunction) = c("function", "smoof_generator", "nk_generator")
attr(makeNKFunction, "name") = c("NK")
attr(makeNKFunction, "type") = c("single-objective")
attr(makeNKFunction, "tags") = c("single-objective", "combinatorial")


makeNKFunctionInternal = function(links_and_values, m) {
  M = links_and_values$M
  N = links_and_values$N
  K = links_and_values$K

  links = links_and_values$links[[m]]
  values = links_and_values$values[[m]]

  force(N)
  force(K)
  force(links)
  force(values)

  fn = function(x) {
    # for every bit position ...
    res = sapply(seq_len(N), function(i) {
      # ... get active links, i.e., those that have a value of 1
      the_links = c(x[i], x[links[[i]] + 1]) # +1 since epistatic links are zero-based
      # ... convert the bitstring to an integer offset of access the value table
      # FIXME: IMPLEMENT convertBitstringToInt in C
      offset = convertBitstringToInt(the_links)
      # ... and return the stored value
      values[[i]][offset + 1L]
    })
    # normalise
    sum(res) / N
  }

  # FIXME: id should be random since the function values depend on seed?
  # FIXME: also the name should include some kind of hash
  fn = makeSingleObjectiveFunction(
    name = sprintf("NK-landscape (N=%i, k=%s)", N, "K_string"),
    id = "NK_landscape",
    fn = fn,
    par.set = makeParamSet(makeIntegerVectorParam(
      len = N,
      id = "x",
      lower = rep(0, N),
      upper = rep(1, N)
    ))
  )
  attr(fn, "links_and_values") = links_and_values
  return(fn)
}

makeRMNKFunctionInternalFromFunctions = function(fns) {
  # print(fns)
  links_and_values = list(
    rho = 0, # uncorrelated!
    M = length(fns),
    N = attr(fns[[1L]], "links_and_values")$N,
    K = lapply(fns, function(fn) attr(fn, "links_and_values")$K[[1L]]),
    links = lapply(fns, function(fn) attr(fn, "links_and_values")$links[[1L]]),
    values = lapply(fns, function(fn) attr(fn, "links_and_values")$values[[1L]])
  )
  makeRMNKFunctionInternal(links_and_values)
}

makeRMNKFunctionInternal = function(links_and_values) {
  rho = links_and_values$rho
  M = links_and_values$M
  N = links_and_values$N
  K = links_and_values$K

  # generate M single-objective NK-landscape functions
  fns = sapply(seq_len(M), function(m) {
    makeNKFunctionInternal(links_and_values, m = m)
  })

  force(fns)

  # actual objective function
  fn = function(x) {
    sapply(fns, function(fn) fn(x))
  }

  # FIXME: id should be random since the function values depend on seed?
  # FIXME: also the name should include some kind of hash
  mfn = makeMultiObjectiveFunction(
    name = sprintf("rMNK-landscape (M=%i, N=%i, k=%s, rho=%.5f", M, N, "K_string", rho),
    id = "rMNK_landscape",
    fn = fn,
    par.set = makeParamSet(makeIntegerVectorParam(
      len = N,
      id = "x",
      lower = rep(0, N),
      upper = rep(1, N)
    )),
    n.objectives = M
  )
  attr(mfn, "links_and_values") = links_and_values
  return(mfn)
}

# helper to calculate offset for values table
convertBitstringToInt = function(x) {
  n = length(x)
  sum(x * 2^(0:(n-1)))
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
      sample(idxs[-n], size = K[[m]][n], replace = FALSE) - 1L # zero-based
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
  K2 = K[[1L]] # K is always a length-M list
  n_values = sum(2^(K2 + 1))
  values = pnorm(MASS::mvrnorm(n = n_values, mu = mu, Sigma = Sigma))

  # convert to list of form values[[M]][[N]][[K[...]]]
  values = lapply(seq_len(M), function(m) {
    tmp = vector(length = N, mode = "list")
    sizes = 2^(K2 + 1L)
    start = 1L
    for (n in seq_len(N)) {
      end = start + sizes[n] - 1L
      tmp[[n]] = values[start:end, m]
      start = end + 1L
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

prepareEpistaticLinks = function(M, N, K) {
  if (M == 1L & length(K) == 1L)
    K = list(rep(K, N)) # nevertheless a list of length 1
  else if (M == 1L & (length(K) == N))
    K = list(K)
  else if ((M > 1L) & (length(K) == 1L))
    K = lapply(seq_len(M), function(i) rep(K, N))
  else if ((M > 1L) & (length(K) == M))
    K = lapply(K, function(k) rep(k, N))
  else
    BBmisc::stopf("[prepareEpistaticLinks] Passed value of K is illegal.")

  return(K)
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
#' @param funs [\code{list} | \code{NULL}]\cr
#'   Allows for an alternative way to build a MNK-landscape by passing a list of
#'   at least two single-objective NK-landscapes. In this case all other parameters
#'   are ignored. Default is \code{NULL}.
#' @return [\code{smoof_multi_objective_function}]
#'
#' @examples
#' # generate homogeneous uncorrelated bi-objective rMNK-landscape with each
#' # three epistatic links
#' M = 2L
#' N = 20L
#' K = 3L
#' fn = makeRMNKFunction(M, N, K)
#'
#' # generate strongly correlated objectives
#' fn = makeRMNKFunction(rho = 0.9, M, N, K)
#'
#' # generate first function has 3 epistatic links while the second has 2
#' fn = makeRMNKFunction(rho = 0.9, M, N, K = c(3L, 2L))
#'
#' # alternative constructor: generate two single-objective NK-landscapes
#' # and combine into bi-objective problem
#' soofn1 = makeNKFunction(N, K) # homegeneous in K
#' K = sample(2:3, size = N, replace = TRUE)
#' soofn2 = makeNKfunction(N, K = K) # heterogeneous in K
#' moofn = makeRMNKFunction(funs = list(soofn1, soofn2))
#' getNumberOfObjectives(moofn)
#'
#' @seealso \code{\link{makeNKFunction}}
#' @export
makeRMNKFunction = function(M, N, K, rho = 0, funs = NULL) {
  if (!is.null(funs)) {
    if (checkmate::testCharacter(funs, min.len = 2L))
      funs = lapply(funs, importNKFunction)

    checkmate::assertList(funs, types = "smoof_function", min.len = 2L, any.missing = FALSE, all.missing = FALSE)
    Ns = lapply(funs, getNumberOfParameters)
    if (length(unique(Ns)) != 1L)
      BBmisc::stopf("[makeRMNKFunction] functions in 'funs' are incompatible! Note that all function need to have the same input dimension N.")

    Ms = lapply(funs, getNumberOfObjectives)
    if ((length(unique(Ms)) != 1L) | any(Ms > 1L))
      BBmisc::stopf("[makeRMNKFunction] functions in 'funs' do not fulfill the requirements. Note that all passed function need to be single-objective.")

    return(makeRMNKFunctionInternalFromFunctions(funs))
  }

  M = checkmate::asInt(M, lower = 2L, upper = 20L)
  checkmate::assertNumber(rho, lower = -1, upper = 1)
  K = prepareEpistaticLinks(M = M, N, K)

  # generate links and values
  # * returns a list of lists of links (each one list for each objective)
  # * returns a list of lists of value tables (each one list for each objective)
  links_and_values = generateRMNKFunction(M, N, K, rho)

  makeRMNKFunctionInternal(links_and_values)
}

class(makeRMNKFunction) = c("function", "smoof_generator", "nk_generator")
attr(makeRMNKFunction, "name") = c("rMNK")
attr(makeRMNKFunction, "type") = c("multi-objective")
attr(makeRMNKFunction, "tags") = c("multi-objective", "combinatorial")

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
  cat(sprintf("rMNK %.5f %i %i %i\n", rho, M, N, K[[1L]][1L]), file = file)

  # export epistatic links
  for (m in seq_len(M)) {
    for (n in seq_len(N)) {
      if (K[[m]][n] == 0L) {
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

#' Import a (rM)NK-landscape function from a file
#'
#' @param path [\code{character(1)}]\cr
#'   Path to file.
#' @return [\code{smoof_single_objective_function} | [\code{smoof_multi_objective_function}]
#'
#' @seealso \code{\link{exportNKFunction}}
#' @export
importNKFunction = function(path) {
  checkmate::assertFileExists(path)

  lines = readLines(path)

  # skip comments
  index = 1L
  while (substr(lines[index], 1, 1) == "C")
    index = index + 1L

  # read meta data
  meta = strsplit(lines[index], " ")[[1L]]

  rho = as.numeric(meta[2L])
  M = as.integer(meta[3L])
  N = as.integer(meta[4L])
  K = vector(length = M, mode = "list") # extracted from links

  index = index + 1L

  # read epistatic links
  links = vector(length = M, mode = "list")
  for (m in seq_len(M)) {
    links[[m]] = vector(length = N, mode = "list")
    for (n in seq_len(N)) {
      current_links = as.integer(strsplit(lines[index], " ", fixed = TRUE)[[1L]])
      links[[m]][[n]] = current_links
      index = index + 1L
    }
    # extract number of links for each bit
    K[[m]] = sapply(links[[m]], length)
  }

  # read table values
  values = vector(length = M, mode = "list")
  for (m in seq_len(M)) {
    values[[m]] = vector(length = N, mode = "list")
    for (n in seq_len(N)) {
      current_values = as.numeric(strsplit(lines[index], " ", fixed = TRUE)[[1L]])
      values[[m]][[n]] = current_values
      index = index + 1L
    }
  }

  links_and_values = list(
    rho = rho,
    M = M,
    N = N,
    K = K,
    links = links,
    values = values
  )

  if (M == 1L)
    return(makeNKFunctionInternal(links_and_values, m = 1L))
  return(makeRMNKFunctionInternal(links_and_values))
}
