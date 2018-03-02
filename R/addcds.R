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
  cdf <- data.frame(paste0(ID)=rownames(cds),
                    cd=cds)
  df <- merge(df,cdf,by=ID)
  df
}
