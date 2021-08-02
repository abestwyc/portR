#' @title
#' Cross-section Persistence
#'
#' @description
#' Calculate some variables' Cross-section Persistence in a data.table.
#'
#' @details
#' Every \strong{Date} (a cross-section) we calculate the Pearson correlation between
#' variable A and its lag A_L_n (n is the lag periods), use different stocks' data.
#' Finanly we have a time series of correlation. Then we calculate its mean.
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
#' @param Id
#' A single character, variable name which indicate different stocks. Because we should
#' get the lag variable of different stock, this Id can make sure every stock matchs itself.
#' If you don't have an Id indicator, just let \code{Id = NULL}
#'
#' @param Var
#' Character vector, some variables we want to calculate Cross-section Persistence.
#'
#' @param Lag
#' Numeric vector, the lag periods use to compute correlation.
#'
#' @return
#' A data.table contains variables and their persistence.
#'
#' @examples
#' crs_pers(DT = stock,
#'          Id = "stkcd",
#'          Date = "month",
#'          Var = c("return", "size"),
#'          Lag = c(1, 3, 5))
#'
#' @importFrom data.table setDT
#' @importFrom data.table copy
#' @importFrom data.table data.table
#' @importFrom data.table :=
#' @importFrom data.table shift
#' @importFrom data.table as.data.table
#'
#' @export
crs_pers <- function(DT, Date, Id, Var, Lag) {
  nms_check(DT, crs_pers)
  dt <- setDT(copy(DT))
  Lags <- paste0("L", Lag)
  persis1 <- function(DT, Var) {
    dt1 <-
      copy(DT)[, (Lags) := shift(.SD[[Var]], n = Lag, type = "lag"), by = Id]
    res1 <- crs_corr(DT = dt1,
                     Date = Date,
                     Use = c(Var, Lags))[[Var]][-1]
    names(res1) <- Lags
    # c(Lag_Var = Var, round(res1, 3))
    round(res1, 3)
  }
  names(Var) <- Var
  res <-
    as.data.table(c(list(Lag = Lags), lapply(Var, persis1, DT = dt)))
  res
}
