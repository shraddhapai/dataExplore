#' PCA with samples coloured by group properties
#'
#' @param m (matrix) 
#' @param mdata (data.frame) metadata. rownames(mdata)=colnames(m)
#' @param groupToPlot (char) vector of metadata variables
#' using which to colour-code samples
#' @import RColorBrewer
#' @return No value. side effect of plotting pca graph
#' @export
pcaColored <- function(m,mdata,groupToPlot,cex=0.5,...) {
	pr <- prcomp(t(m))

	for (currGroup in groupToPlot) {
			cat(sprintf("%s ****\n", currGroup))
		cur <- mdata[,currGroup]
		print(table(cur))
		if (class(cur) %in% c("character","integer")) cur <- as.factor(cur)
		lv <- levels(cur)
		print(lv)
		pal <- brewer.pal(n=length(lv), name="Dark2")

		bf1 <- max(pr$x[,1])-min(pr$x[,1])*0.05
		bf2 <- max(pr$x[,2])-min(pr$x[,2])*0.05
		bf3 <- max(pr$x[,3])-min(pr$x[,3])*0.05
		clrs <- pal[as.integer(cur)]

		par(mfrow=c(2,2),mar=c(4,4,1,1))
		for (plotType in c("full")) {
		# prob could be more succinctly written with pairs()
		plot(pr$x[,1],pr$x[,2],type='n',xlab='PC1',ylab='PC2',
			 xlim=c(min(pr$x[,1])-bf1,max(pr$x[,1])+bf1),
			 ylim=c(min(pr$x[,2])-bf1,max(pr$x[,2])+bf2),...)
		if (plotType %in% "full") 
			text(pr$x[,1],pr$x[,2],colnames(m),col=clrs,cex=cex)
		else 
			points(pr$x[,1],pr$x[,2],colnames(m),col=clrs,cex=cex,
				   pch=16)

		legend("topright",fill=NA,border=NA,pch=16,col=pal,
			   legend=lv,cex=0.5)
		title(sprintf("%s",currGroup))

		plot(pr$x[,2],pr$x[,3],type='n',xlab='PC2',ylab='PC3',
			 xlim=c(min(pr$x[,2])-bf2,max(pr$x[,2])+bf2),
			 ylim=c(min(pr$x[,3])-bf3,max(pr$x[,3])+bf3),...)
		if (plotType %in% "full") 
			text(pr$x[,2],pr$x[,3],colnames(m),col=clrs,cex=cex)
		else
			points(pr$x[,2],pr$x[,3],colnames(m),col=clrs,cex=cex,
				   pch=16)

		plot(pr$x[,1],pr$x[,3],type='n',xlab='PC1',ylab='PC3',
			 xlim=c(min(pr$x[,1])-bf1,max(pr$x[,1])+bf1),
			 ylim=c(min(pr$x[,3])-bf3,max(pr$x[,3])+bf3),...)
		if (plotType %in% "full")
			text(pr$x[,1],pr$x[,3],colnames(m),col=clrs,cex=cex)
		else
			points(pr$x[,1],pr$x[,3],colnames(m),col=clrs,cex=cex,
				   pch=16)

		plot(pr$x[,2],pr$x[,3],type='n',xlab='PC2',ylab='PC3',
			 xlim=c(min(pr$x[,2])-bf2,max(pr$x[,2])+bf2),
			 ylim=c(min(pr$x[,3])-bf3,max(pr$x[,3])+bf3),...)
		if (plotType %in% "full")
			text(pr$x[,2],pr$x[,3],colnames(m),col=clrs,cex=cex)
		else 
			points(pr$x[,2],pr$x[,3],colnames(m),col=clrs,cex=cex,
				   pch=16)
		}
	}
	
}
