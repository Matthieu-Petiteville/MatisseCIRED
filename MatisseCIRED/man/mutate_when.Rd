% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_functions_util.R
\name{mutate_when}
\alias{mutate_when}
\title{mutate_when}
\usage{
mutate_when(data, ...)
}
\arguments{
\item{data}{A condition and a list of changes to apply (see example)}

\item{...}{}
}
\value{

}
\description{
Similar to mutate from dplyr, to allow mutate if condition is OK
}
\examples{
 a <- data.frame(num = 1:20, isover10 = FALSE)
 a <- a \%>\% mutate_when(num > 10, isover10 = TRUE)
 a
}
