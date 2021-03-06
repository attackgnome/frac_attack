% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_function.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{FARS summarize by year and month}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A vector of numbers representing years}
}
\value{
A data.frame containing the number of accidents with years as columns and months as rows
}
\description{
\code{fars_read_years} returns a data.frame containing the number of accidents with years as columns and months as rows
}
\note{
requires dplyr package
}
\examples{
fars_summarize_years(c(2013,2014))
fars_summarize_years(2014:2015)
}
