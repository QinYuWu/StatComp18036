#' @title Rayleigh distribution function
#' @name MC.dRay
#' @description Compute the Rayleigh distribution function by Monte Carlo method
#' @param x vector of quantiles
#' @param sigma scale parameter of Rayleigh distribution
#' @param R size of Monte Carlo method
#' @param antithetic antithetic, if TRUE, use antithetic method
#' @return vector of Rayleigh distribution function\code{n}
#' @examples
#' \dontrun{
#' MC1<-numeric(1000)
#' MC2<-numeric(1000)
#' for(i in 1:1000) {
#' MC1[i]<-MC.dRay(1,antithetic = FALSE)
#' MC2[i]<-MC.dRay(1)
#' }
#' print(c(sd(MC1),sd(MC2)))
#' }
#' @export
MC.dRay <- function(x,sigma=1, R = 10000, antithetic = TRUE) {
  cdf <- numeric(length(x))
  u <- runif(R/2)
  if (!antithetic) v <- runif(R/2) else
    v <- 1 - u
  u <- c(u, v)
  for (i in 1:length(x)) {
    g <- x[i]^2*u*exp(-x[i]^2*u^2/(2*sigma^2))/sigma^2
    cdf[i] <- mean(g)
  }
  cdf
}
