#' Function to draw a single random catchability from a gamma distribution with mean (mm) and varainnce (vv)
#' @author Allan Hicks, Stan Kotwicki
#' @param mm mean of the distribution
#' @param vv variance of the distribution
#' @return a vector of random draws for catchability
#' @export
#' @examples
#' catchability.gamma(10,.8,.01)
catchability.gamma=function(nn,mm,vv){
  shape=mm^2/vv
  scale=vv/mm
  catchability=rgamma(nn,shape=shape, scale=scale)
  return(catchability)
}


