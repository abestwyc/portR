#' @title
#' Cross-section Correlation
#'
#' @description
#' Calculate Cross-section Correlation between variables in a data.table.
#'
#' @details
#' Every \strong{Date} (a cross-section) we calculate the Spearman / Pearson correlation
#' between variables, so we have a time series of correlation. Then we can calculate
#' the time series' mean.
#'
#' Warning: because we use \code{stats::cor} to calculate correlations each cross-section,
#' and we use \code{cor(x, use = "na.or.complete")}, so if there exist some NAs, the result
#' of NoUse and Use may be quite different.
#'
#' @seealso \code{\link[stats]{cor}}
#'
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
#' Character vector, reverse of NoUse, some variables we care. Cannot be used at the
#' same time as NoUse.
#'
#' @return
#' A matrix-like asymmetric data.table, upper is Spearman Correlation and lower
#' is Pearson Correlation.
#'
#' @examples
#' data(stock)
#' crs_corr(DT = stock,
#'          Date = "month",
#'          NoUse = c("stkcd", "industry", "year", "factor1", "factor2"))
#'
#' crs_corr(DT = stock, Date = "month", Use = c("return", "size"))
#'
#' @importFrom data.table setDT
#' @importFrom data.table copy
#' @importFrom data.table data.table
#' @importFrom stats cor
#'
#' @export
crs_corr <- function(DT, Date, NoUse, Use) {
  nms_check(DT, crs_corr)
  dt <- setDT(copy(DT))
  cor2 <- function(x) {
    cor_p <- cor(x, use = "na.or.complete", method = "pearson")
    cor_s <- cor(x, use = "na.or.complete", method = "spearman")
    cor_p[upper.tri(cor_p)] <- cor_s[upper.tri(cor_s)]
    diag(cor_p) <- 0L
    data.table(cor_p, keep.rownames = T)
  }
  stopifnot("Cannot use two parameters at the same time!" = (missing(NoUse) + missing(Use)) == 1)
  if (!missing(NoUse)) {
    cals <- setdiff(names(dt), c(Date, NoUse))
  } else if (!missing(Use)) {
    cals <- Use
  } else {
    stop("Please specify which columes would be calculated.")
  }
  rmean <- function(x){
    res0 <- mean(x, na.rm  = TRUE)
    round(res0, digits = 3)
  }
  dt1 <-
    dt[, cor2(.SD), by = Date, .SDcols = cals][, lapply(.SD, rmean), by = "rn", .SDcols = cals]
  dt1
}

# we have to do this to avoid R-CMD check
.SD = NULL
