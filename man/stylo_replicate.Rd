% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stylo_replicate.R
\name{stylo_replicate}
\alias{stylo_replicate}
\title{Replicate a logged stylometric analysis or create and log 
a reproducible analysis with \code{stylo}.}
\usage{
stylo_replicate(
  date_time = NULL,
  log_label = NULL,
  add_dir_date = FALSE,
  log_date = Sys.Date(),
  ...
)
}
\arguments{
\item{date_time}{The date and time of a previously-logged
analysis to reproduce. If \code{date_time} is 
undefined, \code{stylo_replicate} will run 
\code{stylo(...)}, passing remaining parameters to that 
function.}

\item{log_label}{A label added to the directory holding
data for replication. Default value is "stylo_log".}

\item{add_dir_date}{Add the date to the log_label. 
Default value is \code{FALSE}}

\item{log_date}{Specify a date to use when logging the 
current work. Default value is the current date.}

\item{...}{other arguments passed to \code{stylo}}
}
\description{
Replicate a logged stylometric analysis or create and log 
a reproducible analysis with \code{stylo}.
}
\details{
\code{stylo_replicate()} might add directories and files 
within the working directory to cache parameters.
}
\examples{
\dontrun{
# Run stylo() and log the process
my_data <- stylo_replicate() # in lieu of stylo()
my_data \%>\% stylo2gg()

# Reproduce previous work from logged parameters 
reproduced_data <- stylo_replicate("2023-01-27 13:46:26")
}

}
