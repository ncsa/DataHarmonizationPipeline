% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataSeparation_New.R
\name{addIdentifyingInfo}
\alias{addIdentifyingInfo}
\title{Function that adds identifying info (unique to each row) to the discrete and continuous datasets}
\usage{
addIdentifyingInfo(dataset, indicesList)
}
\arguments{
\item{dataset}{a dataframe}

\item{indiceslist}{a list of the discrete and continuous indices, of the form
list("discrete" = discrete_indices, "continuous" = continuous_indices) (to be obtained from \code{getDiscreteAndContinuousIndices})}
}
\value{
a list of the discrete and continuous datasets of the form 
list("discrete" = discrete_dataset, "continuous" = continuous_dataset)
}
\description{
Function that adds identifying info (unique to each row) to the discrete and continuous datasets
}
