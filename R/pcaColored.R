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
#' @import RColorBrewer
#' @return No value. side effect of plotting pca graph
#' @export
pcaColored <- function(m,mdata,groupToPlot,cex=0.5,plotType="full",
	bufSpace=0.01,legendCex=0.8,center=TRUE,scale=TRUE,
	pal="Dark2",...) {
	pr <- prcomp(t(m),center=center,scale=scale)

	for (currGroup in groupToPlot) {
			cat(sprintf("%s ****\n", currGroup))
		cur <- mdata[,currGroup]
		print(table(cur))
		if (class(cur) %in% c("character","integer")) cur <- as.factor(cur)
		lv <- levels(cur)
		print(lv)
		pal <- suppressWarnings(brewer.pal(n=length(lv), name=pal))

		bf1 <- (max(pr$x[,1])-min(pr$x[,1]))*bufSpace
		bf2 <- (max(pr$x[,2])-min(pr$x[,2]))*bufSpace
		bf3 <- (max(pr$x[,3])-min(pr$x[,3]))*bufSpace
		clrs <- pal[as.integer(cur)]

		par(mfrow=c(2,2),mar=c(4,4,1,1))
		#for (plotType in c("full")) {
		# prob could be more succinctly written with pairs()
		plot(pr$x[,1],pr$x[,2],type='n',xlab='PC1',ylab='PC2',
			 xlim=c(min(pr$x[,1])-bf1,max(pr$x[,1])+bf1),
			 ylim=c(min(pr$x[,2])-bf1,max(pr$x[,2])+bf2),...)
		if (plotType %in% "full") 
			text(pr$x[,1],pr$x[,2],colnames(m),col=clrs,cex=cex)
		else  {
			points(pr$x[,1],pr$x[,2],col=clrs,cex=cex,
				   pch=16)
		}

		legend("topright",fill=NA,border=NA,pch=16,col=pal,
			   legend=lv,cex=legendCex)
		title(sprintf("%s",currGroup))

		plot(pr$x[,2],pr$x[,3],type='n',xlab='PC2',ylab='PC3',
			 xlim=c(min(pr$x[,2])-bf2,max(pr$x[,2])+bf2),
			 ylim=c(min(pr$x[,3])-bf3,max(pr$x[,3])+bf3),...)
		if (plotType %in% "full") 
			text(pr$x[,2],pr$x[,3],colnames(m),col=clrs,cex=cex)
		else {
			points(pr$x[,2],pr$x[,3],col=clrs,cex=cex,
				   pch=16)
		}

		plot(pr$x[,1],pr$x[,3],type='n',xlab='PC1',ylab='PC3',
			 xlim=c(min(pr$x[,1])-bf1,max(pr$x[,1])+bf1),
			 ylim=c(min(pr$x[,3])-bf3,max(pr$x[,3])+bf3),...)
		if (plotType %in% "full")
			text(pr$x[,1],pr$x[,3],colnames(m),col=clrs,cex=cex)
		else {
			points(pr$x[,1],pr$x[,3],col=clrs,cex=cex,
				   pch=16)
		}

		# plot pct variance explained by each PC
		var <- pr$sdev^2
		cat("Variance by PC:\n")
		print((var[1:10]/sum(var))*100)
		pct_var <- (cumsum(var)/sum(var))*100
		barplot(pct_var[1:10],ylim=c(0,100),
			main="% variance explained (cum)",
			ylab="% var (cum)")
		cat("% Variance explained by top 10 PCs\n")
		print(round(pct_var[1:10]))
		cat("\n")
	}

return(pr)
	
}
