#' Generator for NK-landscapes
#'
#' Generate a single-objective NK-landscape. NK-landscapes are combinatorial
#' problems with input space \eqn{\{0,1\}^N} (in their basic definition). The
#' value of each bit position \eqn{i \in \{1, \ldots, N\}} depends on \eqn{K}
#' other bits, the so-called \emph{(epistatic) links / interactions}.
#'
#' @references
#' Kauffman SA, Weinberger ED. The NK model of rugged fitness landscapes and
#' its application to maturation of the immune response. Journal of Theoretical
#' Biology 1989 Nov 21;141(2):211-45. doi: 10.1016/s0022-5193(89)80019-0.
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
#' N = 20
#' fn = makeNKFunction(N, 3)
#'
#' # evaluate function on some random bitstrings
#' bitstrings = matrix(sample(c(0, 1), size = 10 * N, replace = TRUE), ncol = N)
#' apply(bitstrings, 1, fn)
#'
#' # generate heterogeneous NK-landscape where K is sampled from {2,3,4}
#' # uniformly at random
#' fn = makeNKFunction(N, K = sample(2:4, size = N, replace = TRUE))
#'
#' @family nk_landscapes
#' @return [\code{smoof_single_objective_function}]
#' @export
makeNKFunction = function(N, K) {
  N = checkmate::asInt(N)
  K = checkmate::asInteger(K, lower = 0, upper = N - 1L)
  K = prepareEpistaticLinks(M = 1L, N, K)

  # delegate to flexible generator
  nk_properties = generateRMNKFunction(M = 1L, N, K, rho = 0)

  makeNKFunctionInternal(nk_properties, m = 1L)
}

class(makeNKFunction) = c("function", "smoof_generator", "nk_generator")
attr(makeNKFunction, "name") = c("NK")
attr(makeNKFunction, "type") = c("single-objective")
attr(makeNKFunction, "tags") = c("single-objective", "combinatorial")



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

# Prepare epistatic links K_i, i = 1, ..., N
#
# Background: we store K as a list of length-N integer vectors [[M]][N]
#
# @param M [\code{integer(1)}]\cr
#  Number of objectives.
# @param N [\code{integer(1)}]\cr
#  Number of bits, i.e., input space dimension.
# @param K [\code{integer}]\cr
#  Epistatic links
# @return List of in
prepareEpistaticLinks = function(M, N, K) {
  if (M == 1L & length(K) == 1L)
    K = list(rep(K, N)) # nevertheless a list of length 1
  else if (M == 1L & (length(K) == N))
    K = list(K)
  else if ((M > 1L) & (!is.list(K)) & (length(K) == 1L))
    K = lapply(seq_len(M), function(i) rep(K, N))
  else if ((M > 1L) & (!is.list(K)) & (length(K) == M))
    K = lapply(K, function(k) rep(k, N))
  else if ((M > 1L) & is.list(K)) {
    for (k in K) {
      checkmate::assertIntegerish(k, len = N, lower = 0, upper = N - 1L)
    }
  } else
    BBmisc::stopf("[prepareEpistaticLinks] Passed value of K is illegal.")

  return(K)
}

#' Generators for (r)MNK-landscapes
#'
#' Generators for multi-objective NK-landscapes, i.e., with at least two objectives.
#' Function \code{makeMNKLandscape(M, N, K)} create NK-landscapes with \code{M}
#' (\eqn{\geq 2}) objectives, input dimension \code{N}, and epistatic links specified
#' via parameter \code{K}. \code{K} can be a single integer value, a vector of
#' \code{M} integers or a list of length-\code{N} integer vectors (see parameter
#' description for details) which allow for maximum flexibility. It is also
#' possible to compose a MNK-landscape by passing a list of single-objective
#' NK-landscapes via argument \code{funs}.
#'
#' Function \code{makeRMNKLandscape(M, N, K, rho)} generates a MNK-landscape
#' with correlated objective function values. The correlation can be adjusted
#' by setting the \code{rho} parameter to a value between minus one and one.
#'
#' @references
#' H. E. Aguirre and K. Tanaka, Insights on properties of multiobjective MNK-landscapes,
#' Proceedings of the 2004 Congress on Evolutionary Computation, Portland, OR, USA, 2004,
#' pp. 196-203 Vol.1, doi: 10.1109/CEC.2004.1330857.
#'
#' @param M [\code{integer(1)}]\cr
#'   Number of objectives (at least two).
#' @param N [\code{integer(1)}]\cr
#'   Length of the bit-string (decision space dimension).
#' @param K [\code{integer}]\cr
#'   Epistatic links. Possible values are a a) single integer which is used for all
#'   bits and positions, b) an integer vector of \code{N} integers that are used
#'   across the objectives, c) a list of \code{M} length-\code{N} integers to
#'   define specific epistatic links for every bit position of every objective.
#' @param rho [\code{numeric(1)}]\cr
#'   Correlation between objectives (value between -1 and 1).
#' @param funs [\code{list} | \code{NULL}]\cr
#'   Allows for an alternative way to build a MNK-landscape by passing a list of
#'   at least two single-objective NK-landscapes. In this case all other parameters
#'   \code{M}, \code{N}, \code{K} and \code{rho} are ignored. Note that the passed
#'   functions must be compatible, i.e., a) the input dimension \code{N} needs to match
#'   and b) all pased functions need to be multi-objective.
#'   Default is \code{NULL}.
#' @return [\code{smoof_multi_objective_function}]
#'
#' @examples
#' # generate homogeneous uncorrelated bi-objective MNK-landscape with each
#' # three epistatic links
#' M = 2L
#' N = 20L
#' K = 3L
#' fn = makeMNKFunction(M, N, K)
#'
#' # generate MNK-landscape where the first function has 3 epistatic links
#' # per bit while the second function has 2
#' fn = makeMNKFunction(M, N, K = c(3L, 2L))
#'
#' # sample the number of epistatic links individually from {1, ..., 5} for
#' # every bit position and every objective
#' K = lapply(seq_len(M), function(m) sample(1:(N-1), size = N, replace = TRUE))
#' fn = makeMNKFunction(M, N, K = K)
#'
#' #' # generate strongly positively correlated objectives
#' fn = makeRMNKFunction(M, N, K, rho = 0.9)
#'
#' # alternative constructor: generate two single-objective NK-landscapes
#' # and combine into bi-objective problem
#' soofn1 = makeNKFunction(N, K = 2L) # homegeneous in K
#' K = sample(2:3, size = N, replace = TRUE)
#' soofn2 = makeNKFunction(N, K = K) # heterogeneous in K
#' moofn = makeMNKFunction(funs = list(soofn1, soofn2))
#' getNumberOfObjectives(moofn)
#'
#' @family nk_landscapes
#' @seealso \code{\link{makeNKFunction}}
#' @export
makeMNKFunction = function(M, N, K, funs = NULL) {
  if (!is.null(funs)) {
    if (checkmate::testCharacter(funs, min.len = 2L))
      funs = lapply(funs, importNKFunction)

    checkmate::assertList(funs, types = "smoof_function", min.len = 2L, any.missing = FALSE, all.missing = FALSE)
    Ns = lapply(funs, getNumberOfParameters)
    if (length(unique(Ns)) != 1L)
      BBmisc::stopf("[makeMNKFunction] functions in 'funs' are incompatible! Note that all function need to have the same input dimension N.")

    Ms = lapply(funs, getNumberOfObjectives)
    if ((length(unique(Ms)) != 1L) | any(Ms > 1L))
      BBmisc::stopf("[makeMNKFunction] functions in 'funs' do not fulfill the requirements. Note that all passed function need to be single-objective.")

    return(makeMNKFunctionInternalFromFunctions(funs))
  }

  M = checkmate::asInt(M, lower = 2L, upper = 20L)
  if (!(length(K) %in% c(1L, M)))
    BBmisc::stopf("[makeMNKFunction] Epistatic links 'K' must be an integer vector of length one or M (set to %i).", M)
  K = prepareEpistaticLinks(M = M, N, K)

  funs = lapply(seq_len(M), function(m) {
    makeNKFunction(N, K[[m]])
  })

  makeMNKFunctionInternalFromFunctions(funs)
}

class(makeMNKFunction) = c("function", "smoof_generator", "mnk_generator")
attr(makeMNKFunction, "name") = c("MNK")
attr(makeMNKFunction, "type") = c("multi-objective")
attr(makeMNKFunction, "tags") = c("multi-objective", "combinatorial")


#' @rdname makeMNKFunction
#' @export
makeRMNKFunction = function(M, N, K, rho = 0) {
  M = checkmate::asInt(M, lower = 2L, upper = 20L)
  checkmate::assertNumber(rho, lower = -1, upper = 1)
  K = prepareEpistaticLinks(M = M, N, K)

  nk_properties = generateRMNKFunction(M, N, K, rho)

  makeMNKFunctionInternal(nk_properties)
}

class(makeRMNKFunction) = c("function", "smoof_generator", "rmnk_generator")
attr(makeRMNKFunction, "name") = c("rMNK")
attr(makeRMNKFunction, "type") = c("multi-objective")
attr(makeRMNKFunction, "tags") = c("multi-objective", "combinatorial")


makeNKFunctionInternal = function(nk_properties, m) {
  M = nk_properties$M
  N = nk_properties$N
  K = nk_properties$K

  links = nk_properties$links[[m]]
  values = nk_properties$values[[m]]

  force(N)
  force(K)
  force(links)
  force(values)

  fn = function(x) {
    # call C++ function for fast evaluation
    evaluate_nk_landscape(values, links, x)
  }

  # FIXME: id should be random since the function values depend on seed?
  fn = makeSingleObjectiveFunction(
    name = sprintf("NK-landscape (N=%i)", N),
    id = "NK_landscape",
    fn = fn,
    minimize = FALSE,
    par.set = makeParamSet(makeIntegerVectorParam(
      len = N,
      id = "x",
      lower = rep(0, N),
      upper = rep(1, N)
    ))
  )
  attr(fn, "nk_properties") = nk_properties
  return(fn)
}

makeMNKFunctionInternalFromFunctions = function(fns) {
  # print(fns)
  nk_properties = list(
    rho = 0, # uncorrelated!
    M = length(fns),
    N = attr(fns[[1L]], "nk_properties")$N,
    K = lapply(fns, function(fn) attr(fn, "nk_properties")$K[[1L]]),
    links = lapply(fns, function(fn) attr(fn, "nk_properties")$links[[1L]]),
    values = lapply(fns, function(fn) attr(fn, "nk_properties")$values[[1L]])
  )
  makeMNKFunctionInternal(nk_properties)
}

makeMNKFunctionInternal = function(nk_properties) {
  rho = nk_properties$rho
  M = nk_properties$M
  N = nk_properties$N
  K = nk_properties$K

  # generate M single-objective NK-landscape functions
  fns = sapply(seq_len(M), function(m) {
    makeNKFunctionInternal(nk_properties, m = m)
  })

  force(fns)

  # actual objective function
  fn = function(x) {
    sapply(fns, function(fn) fn(x))
  }

  # FIXME: also the name should include some kind of hash
  mfn = makeMultiObjectiveFunction(
    name = sprintf("rMNK-landscape (rho = %.5f, M=%i, N=%i)", rho, M, N),
    id = "rMNK_landscape",
    fn = fn,
    minimize = rep(FALSE, M), # always to be maximised
    par.set = makeParamSet(makeIntegerVectorParam(
      len = N,
      id = "x",
      lower = rep(0, N),
      upper = rep(1, N)
    )),
    n.objectives = M
  )
  attr(mfn, "nk_properties") = nk_properties
  return(mfn)
}


#' Export/import (rM)NK-landscapes
#'
#' NK-landscapes and rMNK-landscapes are randomly generated combinatorial structures.
#' In contrast to continuous benchmark function it thus makes perfect sense to store
#' the landscape definitions in a text-based format.
#'
#' The format uses two comment lines with basic information like the package version,
#' the date of storage etc. The third line contains \eqn{\rho}, \eqn{M} and \eqn{N}
#' separated by a single-whitespace. Following that follow epistatic links from
#' which the number of epistatic links can be attracted. There are \eqn{M * N}
#' lines for a MNK-landscape with \eqn{M} objectives and input dimension \code{N}.
#' The first \eqn{N} lines contain the links for the first objective an so on.
#' Following that the tabular values follow in the same manner. For every position
#' \eqn{i = 1, \ldots, N} there is a line with \eqn{2^{K_i + 1}} values.
#'
#' Note: \code{exportNKFunction} overwrites existing files without asking.
#'
#' @param x [\code{smoof_function}]\cr
#'   NK-landscape of rMNK-landscape.
#' @param path [\code{character(1)}]\cr
#'   Path to file.
#' @return Silently returns \code{TRUE} on success.
#'
#' @family nk_landscapes
#' @seealso importNKFunction
#' @export
exportNKFunction = function(x, path) {
  checkmate::assertPathForOutput(path, overwrite = TRUE)

  if (!BBmisc::hasAttributes(x, "nk_properties"))
    BBmisc::stopf("[exportNKFunction] x is missing attribute 'nk_properties'. Obviously, this is not an NK- or rMNK-landscape.")

  file = file(path, open = 'w')
  on.exit(close(file))

  nk_properties = attr(x, "nk_properties")
  links = nk_properties$links
  values = nk_properties$values

  # FIXME: save these things in the function?
  rho = nk_properties$rho
  M = nk_properties$M
  N = nk_properties$N
  K = nk_properties$K

  # FIXME: how to get the package version in here?
  cat(sprintf("COMMENT: file generated with R package smoof v1.6.0, %s\n", format(Sys.time(), "%m/%d/%Y %H:%M:%S")), file = file)
  cat("COMMENT: links are random and identical for every objective function\n", file = file)
  cat(sprintf("%.5f %i %i\n", rho, M, N), file = file)

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

#' @rdname exportNKFunction
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

  rho = as.numeric(meta[1L])
  M = as.integer(meta[2L])
  N = as.integer(meta[3L])
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

  nk_properties = list(
    rho = rho,
    M = M,
    N = N,
    K = K,
    links = links,
    values = values
  )

  if (M == 1L)
    return(makeNKFunctionInternal(nk_properties, m = 1L))
  return(makeMNKFunctionInternal(nk_properties))
}
