#' Returns a character vector of possible function tags.
#'
#' @return [\code{character}]
#' @export
getAvailableTags = function() {
  c("unimodal", "multimodal",
    "separable", "non-separable",
    "convex", "non-convex",
    "continuous", "discrete",
    "scalable", "non-scalable",
    "differentiable", "non-differentiable")
}
