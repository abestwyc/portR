#' @title
#' Winsorize vector
#'
#' @description
#' Make the value of a variable within a given level, often used to eliminate extreme values.
#'
#' @param x
#' A numeric vector, always contains some extreme values.
#'
#' @param Probs
#' Numeric vector of length 2, the level of winsorize (must smaller than 1
#' and bigger than 0).
#'
#' @param Cutpoints
#' Numeric vector of length 2, the range of input x. Sets the values of a given variable
#' that are above or below a certain Cutpoint to that Cutpoint.
#'
#' @param Integer
#' Logical, if TRUE, the Integer variable will also be \strong{winsorized}; if
#' False, \code{winsor} just return the input itself.
#'
#' @return
#' A numeric vector with less extreme values.
#'
#' @examples
#' summary(stock$return)
#' summary(winsor(stock$return, c(0.05, 0.95)))
#'
#' identical(winsor(stock$industry), stock$industry)
#'
#' @importFrom stats quantile
#'
#' @export
winsor <- function(x,
                   Probs,
                   Cutpoints,
                   Integer = F) {
  if (is.character(x))
    return(x)
  stopifnot("Cannot use Probs and Cutpoints at the same time!" = (missing(Probs) + missing(Cutpoints)) == 1)
  if (missing(Cutpoints)) {
    stopifnot("Please check the length of Probs!" = length(Probs) == 2)
    if (is.integer(x)) {
      warning("Should winsorize the INTEGER variable?")
      if (Integer) {
        message("Using Discontinuous Quantile type 2 (SAS default).")
        ranges <-
          quantile(
            x,
            probs = Probs,
            na.rm = TRUE,
            names = FALSE,
            type = 2
          )
      } else{
        return(x)
      }
    } else{
      ranges <-
        quantile(
          x,
          probs = Probs,
          na.rm = TRUE,
          names = FALSE,
          type = 7
        )
    }
  } else{
    stopifnot("Please check the length of Cutpoints!" = length(Cutpoints) ==
                2)
    ranges <- Cutpoints
  }
  mi <- min(ranges)
  ma <- max(ranges)
  x[x <= mi] <- mi
  x[x >= ma] <- ma
  x
}
