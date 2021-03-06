% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcCorr_Pheno.R
\name{PCcorr_pheno}
\alias{PCcorr_pheno}
\title{Correlate PC projections with phenotype}
\usage{
PCcorr_pheno(dat, pheno, showTop = 10, outFile, cex = 1.3)
}
\arguments{
\item{dat}{(matrix or data.frame) data matrix - rows are variables, columns
are samples. Can also be output of prcomp() call (object of "prcomp" class)}

\item{pheno}{(matrix of numeric or factor) variables to correlate with 
PC projections. each column should correspond to a different variable
Must have same ordering as sample values in phenotype table or 
correlation won't work}

\item{showTop}{(integer) top PCs to show}

\item{outFile}{(char) path to graphic output file (e.g. png or pdf file)}
}
\description{
Correlate PC projections with phenotype
}
