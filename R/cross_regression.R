#' @title
#' Cross-section Regression
#'
#' @description
#' Run a Fama-MacBeth (1973) cross-section regression and summary the coefficients.
#'
#' @details
#' Every \strong{Date} (a cross-section) we run a OLS regression like \code{Y ~ X1 + X2}
#' (including intercept), the result of \code{crs_reg} summary average of the coefficients.
#' All the t-values are adjusted for heteroskedasticity and auto-correlations
#' (Newey and West, 1987).
#'
#' @seealso
#' Bali, T. ,  Engle, R. , &  Murray, S. . (2017). Empirical Asset Pricing: The Cross
#' Section of Stock Returns.
#'
#' @param DT
#' A data.table or data.frame contains variables.
#'
#' @param Y
#' Single character, the name of dependent / explained variable.
#'
#' @param Xs
#' Character vector, the name of independent / explanatory variable and control variables.
#'
#' @param Date
#' Single character, variable name which indicate cross-section.
#'
#' @return
#' A data.table contains regression coefficients and t-value (in the bracket). Currently we
#' can't run a lot of regressions at the same time.
#'
#' @examples
#' crs_reg(DT = stock, Y = "return", Xs = c("explain", "size"), Date = "month")
#'
#' @importFrom data.table copy
#' @importFrom data.table setDT
#' @importFrom data.table :=
#' @importFrom stats as.formula
#' @importFrom stats lm
#'
#' @export
crs_reg <- function(DT, Y, Xs, Date) {
  variable = NULL
  dt <-
    dropnas(setDT(copy(DT)), c(Y, Xs, Date))[, .SD, .SDcols = c(Y, Xs, Date)]
  nms <- c("alpha", Xs, "Rsq")
  fml <- as.formula(paste0(Y, "~", paste0(Xs, collapse = "+")))
  fm <- dt[, {
    fit <- lm(fml, data = .SD)
    coefs <- as.list(fit$coef)
    r2 <- summary(fit)$r.sq
    coefs_r <- c(coefs, Rsq = r2)
    names(coefs_r) <- nms
    coefs_r
  }
  , by = Date]
  res <-
    fm[, lapply(nms, function(xs)
      newey(.SD, xs)), .SDcols = -Date]
  data.table::setnames(res, nms)
  res1 <-
    data.table::melt(
      res,
      id.vars = NULL,
      measure.vars = patterns("\\w"),
      value.name = Y
    )
  res1[seq(from = 2,
           to = nrow(res1),
           by = 2), variable := ""]
  res1[]
}

patterns <- function(..., cols = character(0L)) {
  p = unlist(list(...), use.names = any(nzchar(names(...))))
  if (!is.character(p))
    stop("Input patterns must be of type character.")
  lapply(p, grep, cols)
}
