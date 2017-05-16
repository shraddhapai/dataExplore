#' compute row-wise variance
#'
#' @param b (matrix)
#' @export
getVariance <- function(b) {
	mu	<- rowMeans(b)
	n	<- ncol(b)
	if (any(is.na(b))) cat("NA detected ; removing for numerator\n")
	# corrected sd estimator
	x	<- rowSums((b-mu)^2,na.rm=TRUE)/(n-1)
	x
}
