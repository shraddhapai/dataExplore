#' plot contour map for 2D data
#' 
# @details started with code from here:
# https://groups.google.com/forum/#!topic/ggplot2/DobUJXawI08
#' @param df (data.frame) with 'x' and 'y' values
#' @param fill (char) "lines" (contour lines) or "fill" (contour fill)
#' @param gradLow, gradHigh (char) gradient low and high colour values
#' @param contourBins (integer) number of bins in contour
#' @param addPoints (logical) if T, shows individual observations
#' @param downSampls (numeric between 0-1)Downsamples data points before
#' plotting (e.g. 0.1 shows 10% of points)                           
#' @param pt_colour, pt_alpha (character) colour for data points; only 
#' useful if addPoints=TRUE
#' @param addFit (logical) add line fit with CI
#' @param fitCol (char) colour for line fit
#' @param ebCol (char) colour for errorbar
#' @param xlab, ylab (char) x/y axis labels
#' @param xl, yl (numeric length 2) xlim and ylim. If NULL, shows [1,99]th 
#' percentile
#' @param addZeroRef (logical) add zero as a reference line
#' @param getObj (logical) if TRUE returns ggplot object. 
#' @param logLog (logical) if TRUE plots log-log scale
#' @import MASS
#' @import ggplot2
#' @import scales
#' @export
gradient_contour <- function(df,ttl="", 
  type="fill", gradLow="lightgoldenrodyellow", gradHigh="mediumblue", 
  contourBins=30L, addPoints=TRUE, downSample=1, 
  pt_colour="black", pt_alpha=1, addFit=TRUE, fitCol="maroon",
  ebCol="mistyrose3", xlab="x",ylab="y",xl=NULL, yl=NULL, addZeroRef=TRUE,
  getObj=FALSE,logLog=FALSE) {

  ddf <- .mkContour(df)
  
#  cat("got in\n")
  # theme 
  setup <- theme(#panel.background = element_blank(),
                 title=element_text(size=10),
                 axis.text.x=element_text(size=16),
                 axis.text.y=element_text(size=16),
                 axis.title.x=element_text(face="bold",size=20),
                 axis.title.y=element_text(face="bold",size=20),
                 text = element_text(size=20)
    )
  
  # Option 1 - contour lines
  if (type=="lines") {
  p <- ggplot( df, aes(x = x, y = y));
  if (addPoints) {
    idx <- sample(1:nrow(df),round(downSample*nrow(df)),F )
    df2 <- df[idx,]
    p  <- p + geom_point(data=df2, aes(x=x,y=y), size = 1, 
                         alpha = pt_alpha, color=pt_colour) #add scatter    
  }
    
  p <- p + stat_contour( data=ddf, # adds contour
      aes(x = xd, y = yd, z = z, color = ..level..),
      size = 1, bins=contourBins)  +  
    scale_color_gradient(low = gradLow, high = gradHigh,
                                guide = 'colorbar' ) 
  # Option 2: Contour fill
  } else { 
    #  or alternatively,
      
      p <- ggplot() +
        stat_contour(data = ddf, geom = 'polygon', bins=contourBins,
                     aes(x = xd, y = yd, z = z, fill = ..level..));
    #  cat("ggplot ok\n")
      if (addPoints) {
        idx <- sample(1:nrow(df),round(downSample*nrow(df)),F )
        df2 <- df[idx,]
        p  <- p + geom_point(data=df, aes(x=x,y=y), size = 1, alpha = pt_alpha, 
                             color=pt_colour) #add scatter    
      }
      p <- p + scale_fill_gradient(low = gradLow, high = gradHigh,
                            guide = 'colorbar')  
 #   cat("scale fill ok\n")
  }
  if (addFit) p <- p + stat_smooth(data=df, aes(x=x,y=y), 
                                   color=fitCol, size=1.5, 
                                   fill=ebCol, se=TRUE, alpha=pt_alpha)
  if (addZeroRef) {
    p <- p + geom_vline(xintercept=0) + geom_hline(yintercept=0)
  }
  
  if (is.null(xl)) xl <- quantile(df$x, c(0.01,0.99))
  if (is.null(yl)) yl <- quantile(df$y, c(0.01,0.99))
  cat("x,y quantile ok.\n")
  p <- p +   xlab(xlab) + ylab(ylab) +  
    xlim(xl[1],xl[2]) + ylim(yl[1],yl[2]) + 
    ggtitle(ttl) + setup
  
  if (logLog) p <- p + scale_y_log10() + scale_x_log10()
  #cat("added title x,y lims ok.\n")
  if (getObj) return(p)
  p
  
}

contourDemo <- function(
  n=1000L ##<<(integer) number of (x,y) pairs for rnorm distribution
  ) {
  noise <- data.frame(x=rnorm(n,sd=0.2),y=rnorm(n, sd=0.2))
  g <- gradient_contour(noise,ttl=sprintf("rnorm(%i) - noise", n), type="fill",addPoints=FALSE, getObj=TRUE)
  ggsave(filename="noise.pdf", plot=g)
  
  bimodal <- rbind(noise, cbind(x=rnorm(n, mean=1, sd=0.2), y=rnorm(n,mean=0,sd=0.2)))
  g<- gradient_contour(bimodal,ttl="Bimodal",type="fill",addPoints=FALSE, getObj=TRUE)
  ggsave(filename="bimodal.pdf",plot=g)
  
  
}

.mkContour <- function(df, nbin=200) {
  # 2D density estimate using MASS::kde2d with
  # conversion to data frame:
  require(MASS)
  dens <- with(df, kde2d(x, y,n=nbin# n controls granularity of bins
  ))
  # cat("kde2d ok\n")
  ddf <- data.frame(expand.grid(xd = dens$x, yd = dens$y),
                    z = as.vector(dens$z))
  
  return(ddf)
}

