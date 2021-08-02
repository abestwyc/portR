#' @title
#' Double Sort Portfolio Analysis
#'
#' @description
#' The objective of portfolio analysis is to examine the cross-sectional relation
#' between two or more variables.
#'
#' @details
#' At first we sort variable \code{Control} and generate its rank to construct
#' initial portfolio, then we sort \code{RHS} within (if \code{independent = F})
#' each initial portfolio and form final pofolio. Then we calculate the average value
#' of \code{LHS} within each portfolio for each period. Finally we examine whether
#' these average values is statistically distinguishable from zero.
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
#' @param Control
#' Single character, the name of first sort variable.
#'
#' @param RHS
#' Single character, the name of second sort variable.
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
#' @param Independent
#' Logical, whether use a unconditional sort.
#'
#' @param GroupN
#' Numeric vector of length 2, how many portfolios are formed on each cross-section.
#'
#' @return
#' A data.table contains average value of \code{LHS} within each portfolio and the
#' t-statistics.
#'
#' @examples
#' crs_port2(DT = stock,
#'           LHS = "return",
#'           Control = "explain",
#'           RHS = "size",
#'           Date = "month",
#'           Weight = "size",
#'           Risk = c("factor1", "factor2"),
#'           Newey = TRUE,
#'           Independent = FALSE,
#'           GroupN = c(3,5))
#'
#' @importFrom data.table :=
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#' @importFrom stats weighted.mean
#'
#' @export
crs_port2 <-
  function(DT,
           LHS,
           Control,
           RHS,
           Date,
           Weight = NULL,
           Risk = NULL,
           Newey = T,
           Independent = F,
           GroupN = c(3, 5)) {
    nms_check(DT, crs_port2)
    rank1 = rank2 = `_tempD` = ret_H = ret_L = NULL
    stopifnot("Length of GroupN must equal to 2." = length(GroupN) == 2)
    dt <-
      dropnas(DT, c(LHS, Control, RHS, Date, Weight, Risk))
    dt1 <-
      dt[, rank1 := dplyr::ntile(.SD[[Control]], n = GroupN[1]), by = Date]
    if (Independent) {
      dt1[, rank2 := dplyr::ntile(.SD[[RHS]], n = GroupN[2]), by = Date]
    } else{
      dt1[, rank2 := dplyr::ntile(.SD[[RHS]], n = GroupN[2]), by = c(Date, "rank1")]
    }
    dt2 <- dt1[, list(`_temp` = {
      if (is.null(Weight)) {
        mean(.SD[[LHS]], na.rm = T)
      } else{
        weighted.mean(.SD[[LHS]], .SD[[Weight]], na.rm = T)
      }
    }), keyby = c(Date, "rank1", "rank2")]
    # setnames(dt2, "_temp", LHS)
    avg <-
      dt2[, list(`_temp` = mean(.SD[["_temp"]], na.rm = T), rank1 = "Avg"),
          by = c(Date, "rank2")]
    # setnames(avg, "_temp", LHS)
    dt2_1 <- rbind(dt2, avg, use.names = TRUE)
    dif_H <-
      dt2_1[rank2 == GroupN[2], list(`_tempD` = .SD[[Date]], ret_H = .SD[["_temp"]], rank1)]
    dif_L <-
      dt2_1[rank2 == 1, list(`_tempD` = .SD[[Date]], ret_L = .SD[["_temp"]], rank1)]
    dif <-
      merge(dif_H, dif_L, by = c("_tempD", "rank1"))[, list(`_tempD`,
                                                         "_temp" = ret_H - ret_L,
                                                         rank1,
                                                         rank2 = "HighLow")]
    setnames(dif, c("_tempD", "_temp"), c(Date, LHS))
    setnames(dt2_1, "_temp", LHS)
    dt3 <- rbind(dt2_1, dif, use.names = TRUE)
    if (is.null(Risk)) {
      f <- ifelse(Newey, newey, t_fun)
      dt4 <-
        dt3[, list(`_temp` = f(.SD, LHS)), keyby = c("rank1", "rank2")]
    } else{
      # warning("The \"Risk\" variables must change every \"Date\".")
      fac <- dt[, .SD[1], by = Date, .SDcols = Risk]
      adj_dt3 <- dt3[fac, on = Date]
      f <- ifelse(Newey, newey, adj_lm)
      dt4 <-
        adj_dt3[, list(`_temp` = f(.SD, LHS, Risk)), by = c("rank1", "rank2")]
    }
    dt5 <- Reduce(cbind, lapply(c(1:GroupN[2], "HighLow"), function(x) {
      dt4[rank2 == x,][["_temp"]]
    }))
    colnames(dt5) <- c(paste0(RHS, "_", 1:GroupN[2]), "HighLow")
    rnm <- paste0(Control, "_", rep(c(1:GroupN[1], "Avg"), each = 2))
    rnm[seq(from = 2, to = length(rnm), by = 2)] <- ""
    as.data.table(cbind(Control = rnm, dt5))
  }
