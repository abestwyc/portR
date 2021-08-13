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
#' @return
#' A data.table contains all delimited files in specified folder.
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
    Dir, pattern = Regex, full.names = T
  ), fread, ...), use.names = TRUE)
}


