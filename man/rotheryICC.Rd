% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rotheryICC.R
\name{rotheryICC}
\alias{rotheryICC}
\title{Rothery's rank-based intraclass correlation}
\usage{
rotheryICC(df, var, id)
}
\arguments{
\item{df}{Data frame containing var and id, in long form (many rows per id).}

\item{var}{Column containing the measure of interest.}

\item{id}{Column containing the cluster identifier.}
}
\value{
Data frame containing point estimate on native (r_c) and linear (rho) scales, standard error, 95% CI, and p-value of Rothery's ICC.
}
\description{
This function computes a rank-based intraclass correlation per Rothery (1979).
The normal approximation is used to estimate the variance and the p-value,
assuming an ICC value under the null of 2/3.
}
\references{
Rothery, P. (1979). A nonparametric measure of intraclass correlation. *Biometrika* **66**, 629-639. \href{doi:10.1093/biomet/66.3.629}{doi:10.1093/biomet/66.3.629}.
}
\keyword{correlation}
\keyword{emj,}
\keyword{icc,}
\keyword{intraclass,}
