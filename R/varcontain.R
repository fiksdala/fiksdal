#' varcontain
#'
#' This function returns any variable names in a given data set that contain
#' the a provided string.
#' @param df A dataframe
#' @param matching A string that the returned variables in the dataframe will
#' contain
#' @param except A string that the returned variables in the dataframe will NOT
#' contain
#' @param ignorecase Ignore case of the string? Default is TRUE
#' @keywords variable names
#' @export
#' @examples
#' tdf <- data.frame(x=4,t=6,e=2,b=4,yep=9,Nope=10,cort1=3,cort4=5,Cort_55=9)
#' varcontain(tdf,'cort')
#' [1] "cort1"   "cort4"   "Cort_55"

varcontain <- function(df, matching='all',
                       except='none', ignorecase=TRUE) {
<<<<<<< HEAD
  x <- ifelse(matching=='all','',matching)
=======
  x <- ifelse(matching=='all',colnames(df),matching)
>>>>>>> b53fb3772c8244afd87a705f4f533e8715a053dd
  y <- ifelse(except=='none',runif(1))
  output <- colnames(df)[grepl(x,
                               colnames(df),
                               ignore.case = ignorecase)]
  output <- output[!grepl(y,output,ignore.case=ignorecase)]
}
