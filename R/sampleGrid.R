#' samples simulated data from a grid of X and Y locations and applies observation error and catchability to produce a sample
#' @description applies unbiased lognormal observation error and catchability to produce observed values
#' @par xy data.frame of locations along the X and Y axes with names 'X' and 'Y'
#' @par dat a simulated data.frame with columns for X, Y, and eta (the simulated value for that cell) that will be sampled from
#' @par obsCV observation coefficient of variation
#' @par catchabilityPars a list of mean and variance for gamma catchability. Must have names 'mean' and 'var'. var less than or equal to 0 fixes catchability at the mean parameter. NULL sets catchability to 1.
#' @examples
#' dat <- data.frame(X=c(1,1,1,2,2,2,3,3,3),Y=c(1,2,3,1,2,3,1,2,3),eta=c(1:9))
#' xy <- data.frame(X=c(1,1,2,3,3,3),Y=c(2,3,2,1,2,3))
#' obsCV <- 0.1
#' catchabilityPars <- list(mean=1,var=0.01)
#' sampleGrid.fn(xy, dat, obsCV, catchabilityPars)

sampleGrid.fn <- function(xy, dat, obsCV, catchabilityPars) {
	gridLocs <- paste(dat$X,dat$Y)
	sampLocs <- paste(xy$X,xy$Y)
	out <- dat[gridLocs%in%sampLocs,]

	out$observed <- observationsLN.fn(n=length(out$eta), median=out$eta, obsCV, catchabilityPars)$observed
	return(out)
}



