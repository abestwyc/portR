#' @title
#' Folder Files Input Shortcut
#'
#' @description
#' Read some delimited files (csv, txt) in specified path.
#'
#' @param Dir
#' Character folder name. These files should have same column names.
#'
#' @param Regex
#' Regular expression, the pattern of files you want read.
#'
#' @param ...
#' Additional arguments for \code{data.table::fread}.
#'
#' @return
#' A data.table contains all delimited files in specified folder.
#'
#' @seealso
#' \code{\link[data.table]{fread}}
#'
#' @examples
#' # files(Dir = "/Users/xxx/Data/Daily", Regex = "Daily")
#'
#' @importFrom data.table rbindlist
#' @importFrom data.table fread
#'
#' @export
files <- function(Dir, Regex, ...) {
  rbindlist(lapply(list.files(
    Dir, pattern = Regex, full.names = TRUE
  ), fread, ...), use.names = TRUE)
}


