#' N
#'
#' This function produces the n of a sample or subgroup. Similar to length(), but optimized for tabstat. It can be used outside of tabstat as well.
#' @param x A vector or column of a matrix or dataframe.
#' @keywords n, count
#' @export
#' @examples
#' n()

N <- function(x, na.rm=TRUE) {length(x[!is.na(x)])}
