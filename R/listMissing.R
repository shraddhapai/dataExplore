#' flags variables and samples exceeding certain level of missingness
#' 
#' @param x (matrix or data.frame) rows are variables, columns are samples
#' rownames must be var names and colnames must be sample names
#' @param maxMissVar (numeric (0.01,1)) vars with greater than maxMiss samples
#' missing will be flagged
#' @param maxMissSamp (numeric (0.01,1)) samples with greater than maxMissSamp 
#' vars missing will be flagged
#' @param rmMissing (logical) if TRUE remove vars and samples that are missing
#' @return 
#' if rmMissing is set to TRUE, returns the matrix after removing offending
#' variables and samples.
#' Otherwise prints names of such variables and samples to console.
#' @export
listMissing <- function(x, maxMissVar=0.25, maxMissSamp=0.25,rmMissing=FALSE) {
	if (missing(x)) stop("supply x\n")
	# exclude highly missing
	tmp <- is.na(x)
	badvar <- which(rowSums(tmp)>round(maxMissVar*ncol(x)))
	if (length(badvar)>0){
		cat(sprintf("\t%i vars exceed missing cutoff (%1.2f)\n", 
			length(badvar),maxMissVar))
		cat(sprintf("\t{%s}\n",paste(rownames(tmp)[badvar],collapse=",")))
	} else {
		cat("\t no missing variables\n")
	}

	badsamp <- which(colSums(tmp)>round(maxMissSamp*nrow(x)))
	if (length(badsamp)>0){
		cat(sprintf("\t%i samples exceed missing cutoff\n", 
			length(badsamp),maxMissSamp))
		cat(sprintf("\t{%s}\n",paste(colnames(tmp)[badsamp],collapse=",")))
	} else {
		cat("\t no missing samples\n")
	}

	if (rmMissing) {
		if (length(badvar)>0) x <- x[-badvar,]
		if (length(badsamp)>0) x <- x[,-badsamp]
		return(x)
	}
}
