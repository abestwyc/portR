% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_files.R
\name{files}
\alias{files}
\title{Folder Files Input Shortcut}
\usage{
files(Dir, Regex, FileType = "txt", Parallel = FALSE, ...)
}
\arguments{
\item{Dir}{Character folder name. These files should have same column names and
in txt or csv format.}

\item{Regex}{Regular expression, the pattern of files you want read.}

\item{FileType}{Single character, the file type of these data, usually txt / csv.}

\item{Parallel}{Logical, if TRUE then we will use a multi-core algorithm to read
files. Default FALSE, because it also takes some time to allocate
cores for parallel algorithms.}

\item{...}{Additional arguments for \code{data.table::fread}.}
}
\value{
A data.table contains all delimited files in specified folder.
}
\description{
Read some delimited files (csv, txt) in specified path.
}
\examples{
# files(Dir = "/Users/xxx/Data/Daily", Regex = "Daily", FileType = "txt", Parallel = F)

}
\seealso{
\code{\link[data.table]{fread}}
}
