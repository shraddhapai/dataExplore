#' plot unscaled and Z-scaled variable values as boxplot
#' 
#' @param x (data.frame or matrix) rows are variables, columns are samples
#' @param cex.x (numeric) font size for x-axis text
#' @return (list) length 2, with names "unscaled" and "scaled". Contains
#' ggplot objects for the unscaled and scaled boxplot sets respectively.
#' @import ggplot2
#' @import reshape2
#' @export
plotVariables_boxplot <- function(x,cex.x=8) {
	df <- x
	x <- suppressMessages(melt(df))

	pList <- list()

	x[,1] <- as.character(x[,1])
	p <- ggplot(x,aes(x=variable,y=value))
	p <- p + geom_boxplot(aes(group=variable))
	p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,size=cex.x))
	p <- p + ggtitle("data values - unscaled") + ylab("original value")
	pList[["unscaled"]] <- p

	x <- apply(df,2, function(x) { x <- (x-mean(x,na.rm=T))/sd(x,na.rm=T); x})
	x <- suppressMessages(melt(x))[,-1]
 	x[,1] <- as.character(x[,1])

	p <- ggplot(x,aes(x=Var2,y=value))
	p <- p + geom_boxplot(aes(group=Var2))
	p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,size=cex.x))
	p <- p + ggtitle("data values - scaled") + ylab("Z-score")
	pList[["scaled"]] <- p
	
	return(pList)
}

