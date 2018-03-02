#' addcds
#'
#' This function returns a dataframe with cook's distance stats obtained through
#' influence.ME's influence() function.
#' @param df dataframe
#' @param influence.object An object returned from influence.ME's influence()
#' @param ID ID name of df
#' @keywords influence, cook's d, influence.ME
#' @export
#' @examples
#' addcds()

addcds <- function(df,influence.object,ID) {
  cds <- cooks.distance(influence.object)
  cdf <- data.frame(tempvar=rownames(cds),
                    cd=cds)
  colnames(cdf)[1] <- ID
  df <- merge(df,cdf,by=ID)
  df
}
