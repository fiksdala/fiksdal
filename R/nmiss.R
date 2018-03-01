#' nmiss
#'
#' This function outputs the number of missing items for a specified list of
#' variables.
#' @param df Dataframe that has the scale items
#' @param vars Character list (c()) indicating names of individual scale items.
#' @keywords n, count, missing, NA
#' @export
#' @examples
#' nmiss()

nmiss <- function(df, vars) {
  fdf <- df[vars]
  # tfdf <- t(fdf)
  idmis <- function(x) {
    outval <- c()
    for(i in 1:length(x)) {
      if(is.na(x[i])==TRUE) {outval <- append(outval,i)}
    }
    outval <- toString(outval)
    outval
  }
  misvect <- apply(fdf, 1, idmis)
  idmiss <- function(x) {
    output <- c()
    for (i in 1:length(x)) {
      output  <- append(output, length(strsplit(x[i], ",")[[1]]))
    }
    output[is.na(x)==TRUE] <- NA
    output
  }
  idmiss(misvect)
}
