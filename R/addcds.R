#' addcds
#'
#' This function returns a dataframe with cook's distance stats obtained through
#' influence.ME's influence() function
#' @param df dataframe
#' @param influence.object An object returned from influence.ME's influence()
#' @keywords influence, cook's d, influence.ME
#' @export
#' @examples
#' addcds()

addcds <- function(df,influence.object) {
  cds <- cooks.distance(influence.object)
  cdf <- data.frame(ID=rownames(cds),
                    cd=cds)
  df <- merge(df,cdf,by='ID')
  df
}
