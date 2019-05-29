#' show data missingness as a chequered matrix
#' 
#' @param x (matrix) data matrix.
#' @param outFile (char) path to file for printing graph
#' @param wd (numeric) width in inches
#' @param ht (numeric) height in inches
#' @return plots missingness matrix to file
#' @import plotrix
#' @export
plotMissMat <- function(x,outFile="miss.pdf",wd=8,ht=8,xlab="x",
		ylab="y",border="grey50") {
	require(plotrix)
	x <- !is.na(x)
	class(x) <- "numeric"

	tryCatch({
	pdf(outFile,width=wd,height=ht)
	color2D.matplot(x,show.values=FALSE,axes=FALSE,
		cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),border=border,
		main=sub(".pdf","",basename(outFile)),cex=0.8,
		xlab=xlab,ylab=ylab)
	},error=function(ex){
		print(ex)
	},finally={
		dev.off()
	})
}
