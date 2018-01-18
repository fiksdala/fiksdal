#' stdcent
#'
#' This function produces centered and/or standardized variables. Output is either full dataset with standardized/centered vars or just the selected vars in a dataframe.
#' @param df dataframe
#' @param vars character vector of variables to be transformed
#' @param outtype standardized (std), centered (cent), or both (both)
#' @param merged specifies whether output is merged dataset or just selected transformations (in a dataframe)
#' @keywords centered, standardized, transformation
#' @export
#' @examples
#' stdcent()

stdcent <- function(df,vars,outtype='both',merged=TRUE) {
  tdf <- df[vars]
  stdlist <- vector('list',length(tdf))
  centlist <- vector('list',length(tdf))
  if(outtype=='cent'|outtype=='both') {
    for(i in 1:length(tdf)){
      centlist[[i]] <- tdf[i]-mean(tdf[,i],na.rm=TRUE)
      names(centlist[[i]]) <- paste(names(centlist[[i]]), '_c', sep='')
    }
  }
  if(outtype=='std'|outtype=='both') {
    for(i in 1:length(tdf)){
      stdlist[[i]] <- (tdf[i]-mean(tdf[,i], na.rm=TRUE))/sd(tdf[,i],na.rm=TRUE)
      names(stdlist[[i]]) <- paste('std', names(stdlist[[i]]), sep='')
    }
  }
  if(merged==TRUE) {
    if(outtype=='both') {
      output <- data.frame(df,stdlist,centlist)
    }
    if(outtype=='std') {
      output <- data.frame(df,stdlist)
    }
    if(outtype=='cent') {
      output <- data.frame(df,centlist)
    }
  }
  if(merged==FALSE) {
    if(outtype=='both') {
      output <- data.frame(stdlist,centlist)
    }
    if(outtype=='std') {
      output <- data.frame(stdlist)
    }
    if(outtype=='cent') {
      output <- data.frame(centlist)
    }
  }
  output
}

