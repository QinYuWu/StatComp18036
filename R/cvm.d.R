#' @title Cramer-von Mises test
#' @name cvm.d
#' @description compute the Cramer-von Mises statistic for two samples
#' @param x one sample
#' @param y one sample
#' @return a numeric of Cramer-von Mises statistic\code{n}
#' @examples
#' \dontrun{
#' attach(cars)
#' cvm.d(speed,dist)
#' detach(cars)
#' }
#' @export
cvm.d<-function(x,y){ #compute the Cramer-von Mises statistic
  n<-length(x);m<-length(y)
  Fn<-ecdf(x);Gm<-ecdf(y)
  W2<-((m*n)/(m+n)^2)*
    (sum((Fn(x)-Gm(x))^2)+sum((Fn(y)-Gm(y))^2))
  W2
}
