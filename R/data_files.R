#' @title
#' Folder Files Input Shortcut
#'
#' @description
#' Read some delimited files (csv, txt) in specified path.
#'
#' @param Dir
#' Character folder name. These files should have same column names and
#' in txt or csv format.
#'
#' @param Regex
#' Regular expression, the pattern of files you want read.
#'
#' @param Parallel
#' Logical, if TRUE then we will use a multi-core algorithm to read
#' files. Default FALSE, because it also takes some time to allocate
#' cores for parallel algorithms.
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
#' # files(Dir = "/Users/xxx/Data/Daily", Regex = "Daily", Parallel = F)
#'
#' @importFrom data.table rbindlist
#' @importFrom data.table fread
#' @importFrom bettermc mclapply
#'
#' @export
files <- function(Dir, Regex, Parallel = FALSE, ...) {
  fl_nm <- list.files(Dir, pattern = Regex, full.names = TRUE)
  if (Parallel) {
    fls <- mclapply(fl_nm, fread, ...)
  } else {
    fls <- lapply(fl_nm, fread, ...)
  }
  rbindlist(fls, use.names = TRUE)
}
