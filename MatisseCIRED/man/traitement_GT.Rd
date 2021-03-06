% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/A_4_traitement_GT.R
\name{traitement_GT}
\alias{traitement_GT}
\title{traitement_GT}
\usage{
traitement_GT(menage, save_intermed_file = FALSE)
}
\arguments{
\item{menage}{The standard menage dataframe}

\item{save_intermed_file}{Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_forme_A4_rd}
}
\value{

}
\description{
This function handles the 'gros travaux' (large housework) calculation to differentiate between regular houshold
work and the one that are considered 'renovation'. Not exported by default.
}
\examples{
traitement_GT(menage, FALSE)

}
