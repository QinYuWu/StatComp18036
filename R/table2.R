#' @title Table
#' @name table2
#' @description build a contingency table of the counts at each combination of factor levels with two objects
#' @param x one object
#' @param y one object
#' @return a table \code{n}
#' @examples
#' \dontrun{
#' x<-c(1,2,2,3,4)
#' y<-c(1,2,3,3,4)
#' table2(x,y)
#' }
#' @export
table2 <- function(x,y){

  x_val <- unique(x)
  y_val <- unique(y)
  mat <- matrix(0L, length(x_val), length(y_val))
  for (i in seq_along(x)) {
    mat[which(x_val == x[[i]]), which(y_val == y[[i]])] <-
      mat[which(x_val == x[[i]]),  which(y_val == y[[i]])] + 1L
  }
  dimnames <- list(x_val, y_val)
  names(dimnames) <- as.character(as.list(match.call())[-1])  # R has names for dimnames... :/
  tab <- array(mat, dim = dim(mat), dimnames = dimnames)
  class(tab) <- "table"
  tab
}
