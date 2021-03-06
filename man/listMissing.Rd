% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listMissing.R
\name{listMissing}
\alias{listMissing}
\title{flags variables and samples exceeding certain level of missingness}
\usage{
listMissing(x, maxMissVar = 0.25, maxMissSamp = 0.25,
  rmMissing = FALSE)
}
\arguments{
\item{x}{(matrix or data.frame) rows are variables, columns are samples
rownames must be var names and colnames must be sample names}

\item{maxMissVar}{(numeric (0.01,1)) vars with greater than maxMiss samples
missing will be flagged}

\item{maxMissSamp}{(numeric (0.01,1)) samples with greater than maxMissSamp 
vars missing will be flagged}

\item{rmMissing}{(logical) if TRUE remove vars and samples that are missing}
}
\value{
if rmMissing is set to TRUE, returns the matrix after removing offending
variables and samples.
Otherwise prints names of such variables and samples to console.
}
\description{
flags variables and samples exceeding certain level of missingness
}
