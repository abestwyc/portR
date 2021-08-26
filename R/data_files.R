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
#' @param FileType
#' Single character, the file type of these data, usually txt / csv.
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
#' # files(Dir = "/Users/xxx/Data/Daily", Regex = "Daily", FileType = "txt", Parallel = F)
#'
#' @importFrom data.table rbindlist
#' @importFrom data.table fread
#' @importFrom bettermc mclapply
#' @importFrom readxl read_excel
#'
#' @export
files <- function(Dir, Regex, FileType = "txt", Parallel = FALSE, ...) {
  fl_nm <- list.files(Dir, pattern = Regex, full.names = TRUE)
  if (FileType %in% c("txt", "csv")) {
    fun <- data.table::fread
  } else if (tolower(FileType) == "rds") {
    fun <- base::readRDS
  } else if (FileType %in% c("xls", "xlsx")) {
    fun <- readxl::read_excel
  } else {
    stop("Sorry, we currently do not support importing data of this type")
  }
  if (Parallel) {
    fls <- bettermc::mclapply(fl_nm, fun, ...)
  } else {
    fls <- lapply(fl_nm, fun, ...)
  }
  data.table::rbindlist(fls, use.names = TRUE)
}
