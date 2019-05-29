#' show % samples with missing data, with one bar per variable
#' 
#' @param x (data.frame) rows are variables, columns are samples
#' @param ttl (char) plot title
#' @param showPct (logical) if TRUE shows value from 0-100 (fraction samples
#' missing each var). If FALSE shows sample count
#' @param tpose (logical) if TRUE transposes matrix, so that % missing samples
#' is shown
#' @return ggplot barplot showing % missing for each row (variable)
#' @import reshape2
#' @import ggplot2
#' @export
plotMissing_bar <- function(x,ttl,showPct=TRUE,tpose=FALSE) {
	if (tpose) x <- t(x)
	tmp <- colSums(is.na(x))
	if (showPct) {
		tmp <- (tmp/nrow(x))*100
		maxval <- 100
	} else {
		maxval <- ncol(x)
	}

	nummiss <- suppressMessages(melt(tmp))
	nm <-ncol(nummiss)
	if (nm==1) {
		nummiss$name <- rownames(nummiss)
		nummiss <- nummiss[,c("name","value")]
	} else if (nm==2) {
		colnames(nummiss) <- c("name","value")
	} else {
		stop("currently doesn't handle 3+ columns. You may need to delete rownames in x");
	}

	p <- ggplot(nummiss,aes(x=name,y=value)) 
	p <- p + geom_bar(stat="identity")
	p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,size=8))
	p <- p + geom_hline(yintercept=seq(0.25,1,0.25)*maxval)
	p <- p + ylim(c(0,maxval))
	p <- p + ggtitle(ttl)
	return(p)
}
