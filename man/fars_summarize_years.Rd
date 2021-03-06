% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarizes fars data by year}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A vector of years.}
}
\value{
A tbl_df of the the number of records for each month by year. Prints the tbl_df as a side-effect.
}
\description{
This function creates a tbl_df object with the
number of records for each month by year. Returns
an error message of Invalid year: <value> if the
year value is not between 2013-2015.
}
\examples{
\dontrun{
fars_summarize_years(2013)
fars_summarize_years(c(2013, 2014, 2015))
}

}

