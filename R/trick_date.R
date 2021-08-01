#' @title
#' Year-Month Shifter
#'
#' @description
#' In empirical research, to avoid Former-Bias, we need to adjust the date
#' frequently to stagger the time of different variables.
#'
#' @param YYYYMM
#' Character vector in YYYY-MM (e.g. 2021-06, 1999-09) or YYYY-MM-DD format
#' (e.g. 2020-09-26, 1999-06-16)
#'
#' @param Shift
#' Integer, how many month ago (positive integer) or later (negative integer).
#'
#' @return
#' A character vector in YYYY-MM or YYYY-MM-DD format (depends on input format,
#' and DD will not change).
#'
#' @examples
#' # 5 months after January 2021
#' "2021-01" %ym-% -5
#'
#' # 12 months before September 26, 2021
#' "2021-09-26" %ym-% 12
#'
#' # The input can be a character vector
#' c("2021-01-15", "2021-02-25") %ym-% 5
#'
#' @export
`%ym-%` <- function(YYYYMM, Shift) {
  yy1 = as.numeric(substr(YYYYMM, 1, 4))
  mm1 = as.numeric(substr(YYYYMM, 6, 7))
  dd <- ""
  if (all(nchar(YYYYMM) == 10)) {
    dd <- paste0("-", substr(YYYYMM, 9, 10))
  }
  dif = mm1 - Shift
  yy2 = yy1 + (dif %/% 12)
  mm2 = (dif %% 12)
  ifelse(mm2 != 0,
         paste0(yy2, "-", sprintf("%02.0f", mm2), dd),
         paste0(yy2 - 1, "-", 12, dd))
}
