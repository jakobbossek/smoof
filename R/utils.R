# Utility function to check numeric input strings
#
# @param x [\code{numeric}]\cr
#.  Input string.
# @param dimensions [\code{integer(1)}]\cr
#.  Expected length of the input string.
# @param ... [any]\cr
#.  Further parameters passed down to \code{checkmate::assertNumeric}.
# @return Nothing. Side effect is an assertion on \code{x}.
checkNumericInput = function(x, dimensions, ...) {
  if (as.logical(getOption("smoof.check_input_before_evaluation", TRUE)))
    checkmate::assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE, finite = TRUE, ...)
}
