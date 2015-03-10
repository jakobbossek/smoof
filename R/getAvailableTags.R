#' Returns a character vector of possible function tags.
#'
#' @return [\code{character}]
#' @export
getAvailableTags = function() {
  c("unimodal", "multimodal",
    "separable", "non-separable",
    "convex", "non-convex",
    "continuous", "discontinuous",
    "scalable", "non-scalable",
    "differentiable", "non-differentiable",
    "low-conditioned", "moderate-conditioned", "highly-conditioned",
    "adequate-global-structure", "weak-global-structure")
}
