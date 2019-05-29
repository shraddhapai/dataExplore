#' Correlate PC projections with phenotype
#
#' @param dat (matrix or data.frame) data matrix - rows are variables, columns
#' are samples. Can also be output of prcomp() call (object of "prcomp" class)
#' @param pheno (matrix of numeric or factor) variables to correlate with 
#' PC projections. each column should correspond to a different variable
#' Must have same ordering as sample values in phenotype table or 
#' correlation won't work
#' @param showTop (integer) top PCs to show
#' @param outFile (char) path to graphic output file (e.g. png or pdf file)
#' @export
PCcorr_pheno <- function(dat,pheno,showTop=10,outFile,cex=1.3) {

if (class(dat)!="prcomp") {
	cat("running PCA\n")
	pr <- prcomp(t(dat),scale=TRUE,center=TRUE)
} else {
	pr <- dat
}
crmat <- matrix(NA,nrow=showTop,ncol=ncol(pheno))
crp <- matrix(NA,nrow=showTop,ncol=ncol(pheno))
rownames(crmat) <- paste("PC",1:showTop,sep="")
colnames(crmat) <- colnames(pheno)
	pcs <- pr$x
	for (k in 1:ncol(pheno)) {
     for (i in 1:showTop) {
            cat(sprintf("PC%i\n",i))
            cr <- cor.test(pcs[,i],as.integer(pheno[,k]))
            crmat[i,k] <- cr$estimate
            crp[i,k] <- cr$p.value
        }
	}

	pdf(outFile)
    tryCatch({
    color2D.matplot(crmat,show.values=TRUE,axes=FALSE,
        xlab="",ylab="",vcex=2,vcol="black",
        cs1=c(1,1,0),cs2=c(0,1,0),cs3=c(0,1,1))

	p2str <- function(p) {
		if (p < 0.01) return(sprintf("%1.2e",p))
		else return(sprintf("%1.2f",p))
	}	
###	for (i in 1:nrow(crmat)) { 
###        for (j in 1:ncol(crmat)) { 
###		cat(sprintf("%i,%i\n",i,j))
###			str <- sprintf("%1.2f (%s)", crmat[i,(ncol(j)-j)+1],
###								p2str(crp[i,(ncol(j)-j)+1]))
###             text(i-0.5,ncol(crmat)-(j-0.5),str,col="black",cex=cex) 
###        } 
###	} 
###	browser()
    axis(3,at=seq_len(ncol(pheno))-0.5,labels=colnames(crmat),
            tick=F,cex.axis=1,line=-1)
    axis(2,at=seq_len(nrow(crmat))-0.5,
            rev(rownames(crmat)),tick=F,
        las=1,cex.axis=1)
    },error=function(ex){ print(ex)
    },finally={dev.off()})
}
