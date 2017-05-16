#' plot dendrogram
#'
#' @param m (matrix) 
#' @param distFun (function) if NULL, uses 
#' @param linkageMethod (char) for hclust
#' @param circ (logical) make circular
#' @param ... for plot() method
#' @import ape
#' @export
plotDendro <- function(m,distFun=function(x){1-cor(x)},
	linkageMethod="average",circ=FALSE,...) {
	
	d <- as.dist(distFun(m))
	h <- hclust(d,method=linkageMethod)
	if (circ) {
		plot(as.phylo(h),type="fan",...)
	} else {
		plot(h,...)
	}
}
