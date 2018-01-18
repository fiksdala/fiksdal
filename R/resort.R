#' resort
#'
#' This function reorganizes tabstat output to be more readale. Temporary measure.
#' @param output tabstat output, must be specific (not the whole list).
#' @param nby The number of by-variables in the tabstat output.
#' @keywords summary statistics, tabstat
#' @export
#' @examples
#' resort

resort <- function(output, nby) {
    nby <- nby+1
    oldorder <- c(1:nby)
    neworder <- sort(oldorder, decreasing=TRUE)
    newdf <- data.frame(matrix(nrow=nrow(output), ncol=nby))
    cnames <- c(1:nby)
    
    
    for(i in 1:nby) {
        newdf[,i] <- output[,neworder[i]]
        cnames[i] <- colnames(output)[neworder[i]]
    }
    
    
    replength <- nrow(newdf)/length(unique(newdf[,1]))-1
    newv <- c()
    vars <- as.character(unique(newdf[,1]))
    for(j in 1:length(vars)) {
        newv <- append(newv, c(vars[j], rep("", replength)))
    }
    newdf[1] <- newv
    colnames(newdf) <- cnames
    outdf <- data.frame(newdf, output[-1:-nby])
    outdf
}