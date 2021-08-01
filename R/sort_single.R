#' @title
#' Single Sort Portfolio Analysis
#'
#' @description
#' The objective of portfolio analysis is to examine the cross-sectional relation
#' between two or more variables.
#'
#' @details
#' At first we sort variable \code{RHS} and generate its rank to construct
#' portfolio, then we calculate the average value of \code{LHS} within each portfolio
#' for each period. Finally we examine whether these average values is statistically
#' distinguishable from zero.
#'
#' @seealso
#' Bali, T. ,  Engle, R. , &  Murray, S. . (2017). Empirical Asset Pricing: The Cross
#' Section of Stock Returns.
#'
#' @param DT
#' A data.frame or data.table, contains all data.
#'
#' @param LHS
#' Single character, the name of outcome / independent variable.
#'
#' @param RHS
#' Single character, the name of sort variable.
#'
#' @param Date
#' Single character, variable name which indicate cross-section.
#'
#' @param Weight
#' Single character, variable name which used to calculate value-weighted mean.
#'
#' @param Risk
#' Character vector, risk adjust using factors like Fama-French (MKT, SMB, HML), etc.
#'
#' @param Newey
#' Logical, whether the t-values are adjusted for heteroskedasticity and
#' auto-correlations (Newey and West, 1987).
#'
#' @param GroupN
#' Single numeric, how many portfolios are formed on each cross-section.
#'
#' @param Detail
#' Logical, whether show the High-Low (zero-investment / difference) portfolio.
#'
#' @return
#' A data.table contains average value of \code{LHS} within each portfolio and the
#' t-statistics.
#'
#' @examples
#' crs_port(DT = stock,
#'          LHS = "return",
#'          RHS = "size",
#'          Date = "month",
#'          Weight = "size",
#'          Risk = c("factor1", "factor2"),
#'          Newey = TRUE,
#'          GroupN = 10,
#'          Detail = FALSE)
#'
#' @importFrom data.table :=
#' @importFrom data.table data.table
#' @importFrom data.table setnames
#' @importFrom stats weighted.mean
#'
#' @export
crs_port <- function(DT,
                     LHS,
                     RHS,
                     Date,
                     Weight = NULL,
                     Risk = NULL,
                     Newey = T,
                     GroupN = 5,
                     Detail = F) {
  rank1 = temp_date = ret_H = ret_L = NULL
  dt <- dropnas(DT, c(LHS, RHS, Date, Weight, Risk))
  dt1 <-
    dt[, rank1 := dplyr::ntile(.SD[[RHS]], GroupN), by = Date]
  if (is.null(Weight)) {
    dt2 <-
      dt1[, list(`_temp` = mean(.SD[[LHS]])), keyby = c(Date, "rank1")]
  } else{
    dt2 <-
      dt1[, list(`_temp` = weighted.mean(.SD[[LHS]], w =  .SD[[Weight]])),
          keyby = c(Date, "rank1")]
  }
  setnames(dt2, "_temp", LHS)
  dif_H <-
    dt2[rank1 == GroupN, list(temp_date = .SD[[Date]], ret_H = .SD[[LHS]])]
  dif_L <-
    dt2[rank1 == 1, list(temp_date = .SD[[Date]], ret_L = .SD[[LHS]])]
  dif <-
    dif_H[dif_L, on = "temp_date"][, list(temp_date, temp_ret = ret_H - ret_L, rank1 = "HighLow")]
  setnames(
    dif,
    old = c("temp_date", "temp_ret", "rank1"),
    new = c(Date, LHS, "rank1")
  )
  if (Detail == T) {
    return(dif)
  }
  dt3 <- rbind(dt2, dif, use.names = TRUE)
  if (is.null(Risk)) {
    f <- ifelse(Newey, newey, t_fun)
    dt4 <- dt3[, list(`_temp` = f(.SD, LHS)), by = "rank1"]
  } else{
    fac <- dt[, .SD[1], by = Date, .SDcols = Risk]
    adj_dt3 <- dt3[fac, on = Date]
    f <- ifelse(Newey, newey, adj_lm)
    dt4 <-
      adj_dt3[, list(`_temp` = f(.SD, y = LHS, x = Risk)), by = "rank1"]
  }
  dt5 <-
    dt4[seq(from = 2,
            to = 2 * (GroupN + 1),
            by = 2), rank1 := ""]
  setnames(dt5,
           old = c("rank1", "_temp"),
           new = c("RANK", LHS))
  dt5[]
}
