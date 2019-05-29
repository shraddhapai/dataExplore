#' draw circle with area proportional to number
#'
#' @param num (numeric) vector of numbers. currently doesn't put labels
#' @param sc (numeric) scaling factor
#' @param ylim (numeric) plot extent
#' @param texty (numeric) position of label above circle
#' @param ... params for plotrix::draw.circle()
#' @return plots circles with area proportional to number 
#' @import plotrix
#' @export
drawPropCircles <- function(num,sc=1,ylim=c(-1,1),texty=0.5,...) {

	## Calculate radius for given area
	get_radius = function(area = 1) sqrt(area/pi)

ln <- length(num)
plot(-ln:ln,seq(-ln,ln,length=(2*ln)+1),type="n",xlab="",ylab="",bty='n',
 xaxt="n",yaxt="n",ylim=ylim)

onum <- num
num <- (num/max(num))*sc

curx <- -ln
for (x in 1:ln) {
	if (num[x] > 0) { # ignore zero and negative
		draw.circle(x=curx,y=0,get_radius(num[x]),...)
		text(x=curx,y=texty,onum[x])
	}
	curx <- curx+2
}
}

