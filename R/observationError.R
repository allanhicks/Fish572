#' samples simulated data and applies observation error and catchability
#' @description applies unbiased lognormal observation error and catchability to produce observed values
#' I^{\hat}=qIe^{\epsilon}
#' @par n number of samples to produce
#' @par median of the simulated (population) value
#' @par cv coefficient of variation of observation error
#' @par catchabilityParams a list of mean and variance for gamma catchability. Must have names 'mean' and 'var'. var less than or equal to 0 fixes catchability at the mean parameter. NULL sets catchability to 1.
#' @examples
#' observationsLN.fn(3, 21, 0.2, list(mean=1.0,var=0.0000001))
#' observationsLN.fn(3, 21, 0.2, list(mean=1.0,var=0.01))
#' observationsLN.fn(3, 21, 0.2, list(mean=1.0,var=0.0))
#' observationsLN.fn(3, 21, 0.2, list(mean=0.8,var=-1))
#' observationsLN.fn(3, 21, 0.2, NULL)

observationsLN.fn <- function(n, median, cv, catchabilityParams=list(mean=1,var=0.1)) {
	logSigma <- sqrt(log(cv^2+1))
	samp <- rlnorm(n, log(median), logSigma)
	if(is.null(catchabilityParams)) {
		qq <- 1
	} else {
		if(catchabilityParams$var<=0) {
			qq <- catchabilityParams$mean
		} else {
			qq <- catchability.gamma(n, catchabilityParams$mean, catchabilityParams$var)			
		}
	}
	out <- list(observed=qq*samp, catchability=qq, samp=samp)
	return(out)
}



