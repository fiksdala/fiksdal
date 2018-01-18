#' roundan
#'
#' This function will round all numeric columns of a data frame or list of data frames.
#' @param x data frame or a list of data frames
#' @param  roundto digits to round to 
#' @keywords round
#' @export
#' @examples
#' roundan()

roundan <- function(x,roundto) {
  if(class(x)=='list') {
    for(i in 1:length(x)) {
      for(j in 1:ncol(x[[i]])) {
        if(is.numeric(x[[i]][,j])==TRUE) {
          x[[i]][,j] <- round(x[[i]][,j],roundto)
        }
      }
    }
  }
  if(class(x)=='data.frame') {
    for(i in 1:ncol(x)) {
      if(is.numeric(x[,i])==TRUE) {
        x[,i] <- round(x[,i],roundto)
      }
    }
  } 
  x
}