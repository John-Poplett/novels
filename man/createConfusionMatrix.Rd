% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createConfusionMatrix.R
\name{createConfusionMatrix}
\alias{createConfusionMatrix}
\title{From stony-brook results, compute a confusion matrix}
\usage{
createConfusionMatrix(predictions, threshold = 0.5)
}
\arguments{
\item{predictions}{data frame with fields "prediction" and "response"}

\item{threshold}{threshold value for success}
}
\description{
This function returns a data frame with a confusion matrix for the given
predictions, having the fields "prediction" and "response".
}
\keyword{confusion}
\keyword{evaluation,}
\keyword{matrix}
\keyword{metrics,}
