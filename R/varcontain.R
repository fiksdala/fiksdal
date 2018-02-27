#' varcontain
#'
#' This function returns any variable names in a given data set that contain
#' the a provided string.
#' @param df A dataframe
#' @param x A string that the returned variables in the dataframe will contain
#' @param ignorecase Ignore case of the string? Default is TRUE
#' @keywords variable names
#' @export
#' @examples
#' tdf <- data.frame(x=4,t=6,e=2,b=4,yep=9,Nope=10,cort1=3,cort4=5,Cort_55=9)
#' varcontain(tdf,'cort')
#' [1] "cort1"   "cort4"   "Cort_55"

varcontain <- function(df, x, ignorecase=TRUE) {
  colnames(df)[grepl(x,
                     colnames(df),
                     ignore.case = ignorecase)]
}
