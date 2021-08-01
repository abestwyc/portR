#' @title
#' Cross-section Summary
#'
#' @description
#' Cross-section description statistics is useful to understand the cross-sectional
#' distribution of the variables.
#'
#' @details
#' Robust summary because we average descriptive statistics, can effectively avoid the
#' influence of extreme values on descriptive statistics.
#'
#' @seealso
#' Bali, T. ,  Engle, R. , &  Murray, S. . (2017). Empirical Asset Pricing: The Cross
#' Section of Stock Returns.
#'
#' @param DT
#' A data.frame or data.table, contains all data.
#'
#' @param Date
#' A single character, variable name which indicate cross-section.
#'
#' @param NoUse
#' Character vector, some variables we don't care (including all character variables).
#'
#' @param Use
#' Character vector, reverse of NoUse, some variables we care. Can not use with
#' parameter NoUse.
#'
#' @return
#' A data.table have 15 columns, for example "Observations" / "Missings", etc.
#' Maybe we can use \code{t(...)} to make it more easy to present.
#'
#' @examples
#' crs_sum(DT = stock, Use = c("return", "size", "explain"), Date = "month")
#'
#' crs_sum(DT = stock, NoUse = c("stkcd", "industry", "factor1", "factor2"), Date = "month")
#'
#' @importFrom data.table copy
#' @importFrom data.table setDT
#' @importFrom data.table :=
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stats sd
#'
#' @export
crs_sum <- function(DT, Use, NoUse, Date) {
  .N = sta = N = NULL
  stopifnot("Cannot use two parameters at the same time!" = (missing(NoUse) + missing(Use)) == 1)
  if (missing(Use)) {
    if (!missing(NoUse)) {
      nm <- setdiff(colnames(DT), NoUse)
    } else{
      stop("Please select variables to summary.")
    }
  } else{
    nm <- Use
  }
  dt <- setDT(copy(DT))[, .SD, .SDcols = c(Date, nm)]
  sta_nm <-
    c(
      "Obs",
      "Missing",
      "Mean",
      "StdDev",
      "Skewness",
      "Kurtosis",
      "Min",
      "p1",
      "p5",
      "p25",
      "p50",
      "p75",
      "p95",
      "p99",
      "Max"
    )
  cal <- function(x) {
    if (is.numeric(x)) {
      Obs <- length(x)
      x1 <- na.omit(x)
      Missing <- Obs - length(x1)
      pctl <-
        quantile(x1, probs = c(0, 1, 5, 25, 50, 75, 95, 99, 100) / 100)
      Mean <- mean(x1)
      StdDev <- sd(x1)
      # SAS type skewness and kurtosis
      Skewness <- e1071::skewness(x1, type = 2)
      Kurtosis <- e1071::kurtosis(x1, type = 2)
      c(
        Obs = Obs,
        Missing = Missing,
        Mean = Mean,
        StdDev = StdDev,
        Skewness = Skewness,
        Kurtosis = Kurtosis,
        pctl
      )
    } else{
      # warning("Can not summary character variable.")
      NA
    }
  }
  time_n <- function(x, y, n) {
    if (is.character(y))
      return(y)
    z <- vector(length = length(x))
    for (i in seq_along(x)) {
      if (i <= n) {
        z[i] <- round(x[i] * y[i], digits = 0)
      } else{
        z[i] <- y[i]
      }
    }
    z
  }
  sum1 <-
    dt[, c(list(sta = sta_nm), lapply(.SD, cal)), by = Date][, c(.N, lapply(.SD, function(x)
      round(mean(x), 3))),
      by = sta,
      .SDcols = !Date][, lapply(.SD, function(x)
        time_n(N, x, 2))][, N := NULL]
  sum1[]
}
