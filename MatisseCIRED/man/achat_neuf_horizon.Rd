% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/D_1_Achat_neuf_horizon.R
\name{achat_neuf_horizon}
\alias{achat_neuf_horizon}
\title{achat_neuf_horizon}
\usage{
achat_neuf_horizon(menage, save_intermed_file = F)
}
\arguments{
\item{menage}{A menage dataframe}

\item{save_intermed_file}{Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_echelle_D1_rd}
}
\value{
A menage dataframe
}
\description{
Function that handles the buying of new houses at horizon year
}
\examples{
achat_neuf_horizon(menage, FALSE)

}
