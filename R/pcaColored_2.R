#' PCA with samples coloured by group properties
#'
#' @param m (matrix) 
#' @param mdata (data.frame) metadata. rownames(mdata)=colnames(m)
#' @param groupToPlot (char) vector of metadata variables
#' using which to colour-code samples
#' @param plotType (char, full or point) if "full" plots sample name. If 
#' "point", simply plots a dot
#' @param bufType (numeric) buffer around the points, by which to extend x/y
#' lim. Increase this value to allow space to see the full sample name.
#' @param legendCex (numeric) cex for legend
#' @param center,scale (logical) options to center/scale matrix before PCA
#'	See prcomp()
#' @param pal (char) RColorBrewer2 palette name
#' @import GGally
#' @import ggplot2
#' @import RColorBrewer
#' @return No value. side effect of plotting pca graph
#' @export
pcaColored_2 <- function(m,mdata,groupToPlot,cex=0.5,plotType="full",
	bufSpace=0.01,legendCex=0.8,center=TRUE,scale=TRUE,
	showTop=3,
	pal="Dark2",...) {

	showTop <- max(showTop,3);
	pr <- prcomp(t(m),center=center,scale=scale)

	for (currGroup in groupToPlot) {
			cat(sprintf("%s ****\n", currGroup))
		cur <- mdata[,currGroup]
		print(table(cur))
		if (class(cur) %in% c("character","integer")) 
			cur <- as.factor(cur)
		lv <- levels(cur)
		print(lv)
		pal <- suppressWarnings(brewer.pal(n=length(lv), name=pal))

		clrs <- pal[as.integer(cur)]
	
		tmp <- as.data.frame(pr$x[,1:showTop])
		tmp$pheno <- cur 
		if (showTop >5) size <- 0.7 else size <- 0.1

		p <- ggpairs(tmp,
			mapping=ggplot2::aes(colour=pheno),
			lower=list(continuous=wrap("points",alpha=0.5,size=size))
		)
		p <- p + theme(axis.line=element_blank());
				#axis.text=element_blank()axis.ticks=element_blank())
		print(p,progress=FALSE)

###		# plot pct variance explained by each PC
###		var <- pr$sdev^2
###		cat("Variance by PC:\n")
###		print((var[1:10]/sum(var))*100)
###		pct_var <- (cumsum(var)/sum(var))*100
###		barplot(pct_var[1:10],ylim=c(0,100),
###			main="% variance explained (cum)",
###			ylab="% var (cum)")
###		cat("% Variance explained by top 10 PCs\n")
###		print(round(pct_var[1:10]))
###		cat("\n")
	}
return(pr)
	
}
