#' @title
#' Group-rolling Regression
#'
#' @description
#' A function for running group-rolling regression and apply function to the
#' \code{\link[stats]{lm.fit}} result.
#'
#' @details
#' We use \code{stats::lm.fit} to run OLS regression, which returns a \strong{list}
#' with components (e.g. coefficients, residuals). So the \code{FUN} parameter can
#' be very flexible.
#'
#' @seealso
#' \code{\link{roll_f1}}, the same group-rolling window;
#'
#' \code{\link[stats]{lm.fit}} see how to use the regression result to create FUN.
#'
#' @param DT
#' A data.table or data.frame.
#'
#' @param Y
#' Single character, the name of dependent / explained variable.
#'
#' @param Xs
#' Character vector, the name of independent / explanatory variable and control variables.
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
#' The function to be applied to the object \code{lm.fit}, this function can only
#' return a single value. Please note that the name of \code{FUN} cannot be the
#' same as the variable name in the data!!!
#'
#' @param ...
#' Additional parameters of the FUN function.
#'
#' @return
#' A numeric vector of the same length as the input.
#'
#' @examples
#' # Flexible functions
#' rsd <- function(x, ...) sd(residuals(x), ...)
#'
#' # Main variables in DT cannot contains NAs
#' data(stock)
#' stock1 <- dropnas(stock, c("return", "size", "explain"))
#'
#' # Usually, we run this rolling function in a data.table
#' stock1[, rsd1 := roll_reg(DT = .SD,
#'                           Y = "return",
#'                           Xs = c("size", "explain"),
#'                           Group = "year",
#'                           Width = 5,
#'                           MinObs = 45,
#'                           FUN = rsd), by = stkcd]
#'
#' @importFrom data.table setorderv
#' @importFrom data.table :=
#' @importFrom data.table rleid
#'
#' @export
roll_reg <- function(DT, Y, Xs, Group, Width, MinObs, FUN, ...) {
  nms_check(DT, roll_reg)
  `_grps` = .N = NULL
  dt <- dropnas(DT, Group)
  setorderv(dt, Group)
  dt[, `_grps` := rleid(.SD[[Group]])]
  grp_n <- dt[, .N, by = "_grps"][["N"]]
  grp_M <- max(dt[["_grps"]])
  grp_N <- grp_M - Width + 1
  grp_i <- dt[["_grps"]]
  dt_y <- dt[[Y]]
  dt_x <- cbind(1L, as.matrix(dt[, Xs, with = FALSE]))
  if (grp_M < Width) {
    res <- rep(NA, nrow(dt))
    return(res)
  }
  res1 <- lapply(1:grp_N, function(n) {
    pair <- roll_grp_id(n, grp_n = grp_n, width = Width)
    y_s <- dt_y[pair]
    x_s <- dt_x[pair, ]
    if (sum(!is.na(y_s)) < MinObs) {
      NA
    } else{
      fit <- stats::lm.fit(x = x_s, y = y_s)
      FUN(fit,...)
    }
  })
  res2 <- unlist(res1)
  len <- grp_M - grp_N
  if (len > 0) {
    res3 <- rep(NA, len)
    res <- rep(c(res3, res2), grp_n)
  } else{
    res <- rep(NA, length(DT[[Xs[1]]]))
  }
  res
}
