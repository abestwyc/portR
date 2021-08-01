#' @title
#' Group-rolling Apply
#'
#' @description
#' A apply-like function for computing the group-rolling values of time-series data.
#'
#' @details
#' Simple rolling calculation functions usually use a fixed number of observations.
#' We provide a way to perform rolling calculations with a fixed number of groups, not
#' observations.
#'
#' Currently, we can only use a single variable to calculate a single value.
#'
#' The difference between this function and other rolling calculation functions is that
#' we do not roll with a fixed number of observations, but with a fixed specified
#' grouping variable. For example, in build-in data \code{stock}, we want to calculate
#' recent five years' average return by using monthly data. Then we can use this
#' function, in \code{examples}.
#'
#' Usage of this function in empirical asset pricing research also includes calculating
#' MACD, idiosyncratic volatility, idiosyncratic skewness, etc.
#'
#' @param DT
#' A data.table or data.frame, must contains X and Group.
#'
#' @param X
#' Single Character, temporarily.
#'
#' @param Group
#' Single Character, specify the rolling-group variable (usually a time variable like
#' year, month, week, etc).
#'
#' @param Width
#' Positive integer, the rolling calculation window length.
#'
#' @param MinObs
#' Positive integer, with minimum number of observation that are required in a window.
#'
#' @param FUN
#' The function to be applied to each rolling widow, this function can only return a
#' single value. Please note that the name of \code{FUN} cannot be the same as the
#' variable name in the data!!!
#'
#' @param ...
#' Additional parameters of the FUN function.
#'
#' @return
#' A numeric vector of the same length as the input.
#'
#' @examples
#' # Use rolling function with data.table
#' stock1 <- stock
#' stock1[ , rollMean := roll_f1(DT = .SD,
#'                               X = "return",
#'                               Group = "year",
#'                               Width = 5,
#'                               MinObs = 45,
#'                               FUN = mean, na.rm = TRUE), by = stkcd]
#'
#' @importFrom data.table setorderv
#' @importFrom data.table :=
#' @importFrom data.table rleid
#'
#' @export
roll_f1 <- function(DT, X, Group, Width, MinObs, FUN, ...) {
  `_grps` = .N = NULL
  dt <- dropnas(DT, Group)
  setorderv(dt, Group)
  dt[, `_grps` := rleid(.SD[[Group]])]
  grp_n <- dt[, .N, by = "_grps"][["N"]]
  grp_M <- max(dt[["_grps"]])
  grp_N <- grp_M - Width + 1
  grp_i <- dt[["_grps"]]
  dt_s <- dt[[X]]
  if (grp_M < Width) {
    res <- rep(NA, length(dt[[X]]))
    return(res)
  }
  res1 <- lapply(1:grp_N, function(n) {
    pair <- roll_grp_id(n, grp_n = grp_n, width = Width)
    x_s <- dt_s[pair]
    if (sum(!is.na(x_s)) < MinObs) {
      NA
    } else{
      FUN(x_s, ...)
    }
  })
  res2 <- unlist(res1)
  len <- grp_M - grp_N
  if (len >= 0) {
    res3 <- rep(NA, len)
    res <- rep(c(res3, res2), grp_n)
  } else{
    res <- rep(NA, length(DT[[X]]))
  }
  res
}
