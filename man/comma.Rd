% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comma.R
\name{comma}
\alias{comma}
\title{Formats rounded characters nicely for print purposes}
\usage{
comma(x, digits = 1, trailing = FALSE)
}
\arguments{
\item{x}{A numeric object to modify}

\item{digits}{Desired number of digits after the decimal point}

\item{trailing}{Should trailing zeroes be preserved? Default = FALSE}
}
\value{
A properly formatted character object
}
\description{
This function is useful if used in Rmarkdown documents or when used to print nice tables.
}
\examples{

x <- 200.00
comma(x, digits = 1)
# "200"
comma(x, digits = 1, trailing = TRUE)
# "200.0"
}
