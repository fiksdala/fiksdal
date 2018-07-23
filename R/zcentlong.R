#' zcentlong
#'
#' This function produces centered and/or standardized variables for long format
#' dataframes. Merged must=TRUE for now.
#' @param df dataframe
#' @param id Name of grouping variable.
#' @param vars character vector of variables to be transformed
#' @param outtype standardized (std), centered (cent), or both (both)
#' @param merged specifies whether output is merged dataset or just selected
#' transformations (in a dataframe): MUST BE TRUE FOR zcentlong.
#' @keywords centered, standardized, transformation
#' @export
#' @examples
#' zcent()

zcentlong <- function(df,vars,id,outtype='both',merged=TRUE) {
  tdf <- unique(df[c(id,vars)])
  tdf <- zcent(tdf,vars,outtype,merged)
  if(merged==FALSE) {
    print('zcentlong only works with merged=TRUE')
  }
  if(merged==TRUE) {
    merge(df,tdf[,!colnames(tdf)%in%vars],by=id)
  }
}
