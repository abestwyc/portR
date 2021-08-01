#' @title
#' Remove NA observations
#'
#' @description
#' Remove NA values at the same time, in many columns.
#'
#' @param DT
#' A data.table or a data.frame.
#'
#' @param Cols
#' Character vector, contains the columns' name we want to remove NA values.
#'
#' @param KeepAllCols
#' Logical, if FALSE then the return value will only contain columns in \code{Cols}.
#' Equivalent to \code{select} and \code{na.omit} at the same time.
#'
#' @return
#' A data.table without NA value (in \code{Cols}).
#'
#' @examples
#' nrow(stock)
#' nrow(dropnas(stock, Cols = c("return", "size")))
#'
#' dropnas(stock, Cols = c("return", "size"), KeepAllCols = FALSE)
#'
#' @importFrom data.table copy
#' @importFrom data.table setDT
#'
#' @export
dropnas <- function(DT, Cols, KeepAllCols = T) {
  dt <- setDT(copy(DT))
  if (!KeepAllCols) {
    dt <- dt[, Cols, with = FALSE]
  }
  idx <- lapply(Cols, function(x) {
    is.na(dt[[x]])
  })
  idx1 <- Reduce("|", idx)
  dt[!idx1]
}
