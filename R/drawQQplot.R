#' generate qqplot
#'
#' @details from http://www.gettinggeneticsdone.com/2010/07/qq-plots-of-p-values-in-r-using-base.html
#'
#' @param pvector (numeric) pvalue vector
#' @param main (char) title
#' @param ... for plotting
#' @examples 
#' drawQQplot(runif(2000),main="random")
#' 
#' # this one has signal
#' set.seed(42)
#' pvalues=runif(10000)
#' pvalues[sample(10000,10)]=pvalues[sample(10000,10)]/5000
#' drawQQplot(pvalues,"has signal")
#' 
#' @export
drawQQplot <- function(pvector,main="title",...) {
  o = -log10(sort(pvector,decreasing=F))
  e = -log10( 1:length(o)/length(o) )
    plot(e,o,pch=19,cex=1, main=main, ...,
        xlab=expression(Expected~~-log[10](italic(p))),
        ylab=expression(Observed~~-log[10](italic(p))),
        xlim=c(0,max(e)), ylim=c(0,max(o)))
    lines(e,e,col="red")
}


