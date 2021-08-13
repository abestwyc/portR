#' @title
#' Colnames Shortcut
#'
#' @description
#' The package always use column names, when there are many variables in
#' the data, this function will reduce the workload.
#'
#' @param DT
#' Anything with names, usually a data.table.
#'
#' @param NoUse
#' Character vector, the names we don't use. Notice that when
#' \code{Regex = TRUE} the argument should be regular expression.
#'
#' @param Regex
#' Logical, if TRUE then we will use regular expression to filter
#' undesired variable names.
#'
#' @return
#' A character vector without undesired variable names.
#'
#' @examples
#' names(stock)
#' # we want to get all variable names without "factor1" and "factor2"
#' use(DT = stock, NoUse = c("factor1", "factor2"), Regex = F)
#' # which is equivalent to following
#' use(DT = stock, NoUse = "^fac", Regex = T)
#'
#' @export
use <- function(DT, NoUse, Regex = F){
  nm <- names(DT)
  if (!Regex) {
    nm[!nm %in% NoUse]
  } else {
    grep(NoUse, nm, invert = T, value = T)
  }
}
