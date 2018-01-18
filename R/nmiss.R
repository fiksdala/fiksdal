#' nmiss
#'
#' This simple function outputs the number of missing scale items for each observation summarized by idmiss. 
#' @param x Individual observation (produced by idmiss)
#' @keywords n, count
#' @export
#' @examples
#' n()

nmiss <- function(x) {
    output <- c()
    for (i in 1:length(x)) {
        output  <- append(output, length(strsplit(x[i], ",")[[1]]))
    }
    output[is.na(x)==TRUE] <- NA
    output
}