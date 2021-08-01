#' @importFrom stats lm.fit
#' @importFrom stats pt
#' @importFrom stats t.test
t_fun <-
  function(data, x) {
    x1 <- data[[x]]
    tt <- t.test(x1)
    tm <-
      paste0(sprintf("%0.3f", tt$estimate), star(tt$p.value))
    ts <- paste0('(', sprintf("%0.2f", tt$statistic), ')')
    c(tm, ts)
  }

newey <- function(data, y, x = NULL) {
  dt <- dropnas(data, c(y, x), KeepAllCols = FALSE)
  if (is.null(x)) {
    y1 <- dt[[y]]
    fm <- lm(y1 ~ 1L)
  } else{
    fml <- as.formula(paste0(y, "~", paste0(x, collapse = "+")))
    fm <- lm(fml, data = dt)
  }
  L <- 4 * ((nrow(dt) / 100) ^ (2 / 9))
  nw_cov <- sandwich::NeweyWest(fm, lag = L, prewhite = FALSE)
  nw_std <- sqrt(diag(nw_cov))
  nw_t <- fm$coefficients[1] / nw_std[1]
  nw_p <- 2 * pt(abs(nw_t), fm$df.residual, lower.tail = FALSE)
  t_m <-
    paste0(sprintf("%.3f", fm$coefficients[1]), star(nw_p))
  t_s <- paste0('(', sprintf("%.2f", nw_t), ')')
  c(t_m, t_s)
}

adj_lm <- function(dt, y, x) {
  df <- dropnas(dt, c(y, x), KeepAllCols = FALSE)
  y <- df[[y]]
  x <- as.matrix(df[, x, with = FALSE])
  x1 <- cbind(1L, x)
  fit <- lm.fit(x = x1, y = y)
  r <- fit$residuals
  rss <- sum(r ^ 2)
  rdf <- fit$df.residual
  resvar <- rss / rdf
  p <- 1:(fit$rank)
  R <- chol2inv(fit$qr$qr[p, p, drop = FALSE])
  se <- sqrt(diag(R) * resvar)
  est <- fit$coefficients[fit$qr$pivot[p]]
  tval <- est / se
  pvalue <-
    2 * pt(abs(tval), rdf, lower.tail = FALSE)
  i <-
    paste0(sprintf("%.3f", est[1]), star(pvalue[1]))
  t <-
    paste0("(", sprintf("%.2f", tval[1]), ")")
  c(i, t)
}
