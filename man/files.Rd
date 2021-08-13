% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_files.R
\name{files}
\alias{files}
\title{Folder Files Input Shortcut}
\usage{
files(Dir, Regex, ...)
}
\arguments{
\item{Dir}{Character folder name. These files should have same column names.}

\item{Regex}{Regular expression, the pattern of files you want read.}

\item{...}{Additional arguments for \code{data.table::fread}.}
}
\value{
A data.table contains all delimited files in specified folder.
}
\description{
Read some delimited files (csv, txt) in specified path.
}
\examples{
# files(Dir = "/Users/xxx/Data/Daily", Regex = "Daily")

}
\seealso{
\code{\link[data.table]{fread}}
}