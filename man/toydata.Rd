\name{toydata}
\alias{toydata}
\docType{data}
\title{
Artificial 4-level data set
}
\description{
A 4-level data set with two responses and two covariates to illustrate the package mvctm. The classroom is nested within the school which is nested within the region. 
}
\usage{data(toydata)}
\format{
  A data frame with 150 observations on the following 7 variables.
  \describe{
    \item{\code{y1}}{The first response}
    \item{\code{y2}}{The second response}
    \item{\code{x1}}{The first covariate}
    \item{\code{x2}}{The second covariate}
    \item{\code{region}}{The level 1 cluster (outer)}
    \item{\code{school}}{The level 2 cluster (middle)}
    \item{\code{classroom}}{The level 3 cluster (inner)}
  }
}
\examples{
data(toydata)
}
\keyword{datasets}
