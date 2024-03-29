% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarize FARS data by month and year}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A vector of the years you'd like to collect data for}
}
\value{
A summary data.frame showing the total number of fatal road incidents by month / year.
}
\description{
The functions loads multiple years of FARS csv data
and shows the total number of fatal road incidents
by month (rows) and years (columns).
The FARS csv files you wish to read must be in the
working directory otherwise the function will give you a \code{warning: invalid year}.
Requires the dplyr and tidyr packages to be loaded.
Example data is only available for 2013:2015.
}
\examples{
\dontrun{
  setwd("data")
  years <- 2013:2015
  df <- fars_summarize_years(years)
  print(class(df))
  print(head(df))
}

}
