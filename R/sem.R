#' SEM
#'
#' This function returns the standard error of the mean.
#' @param x A vector.
#' @keywords sem, standard error
#' @export
#' @examples
#' Sem()

SEM <- function(x,na.rm=TRUE) {
    sd(x,na.rm=TRUE)/
        sqrt(length(x[!is.na(x)]))
}