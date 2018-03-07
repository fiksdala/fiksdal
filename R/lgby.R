#' lgby
#'
#' This function quickly executes the aggregate and ggplot functions to produce
#' a simple line graph with separate lines based on an indicated group.
#' @param df A dataframe.
#' @param x The variable to appear on the x-axis.
#' @param y The variable to appear on the y-axis.
#' @param by The grouping variable.
#' @param facet_var The facet-grouping variable.
#' @param grid_var The grid-grouping variable. When using grid_var, it is
#' essentially interchangable with facet_var, as together they determine the
#' order and axis of the grid. grid_var cannot be used independently of
#' facet_var for obvious reasons.
#' @param error_bars Include standard error bars?
#' @keywords line graph
#' @export
#' @examples
#' lgby()

lgby <- function(df, x, y, by, facet_var, grid_var,error_bars=TRUE) {
    if(missing(facet_var)==TRUE & missing(grid_var)==FALSE) {
        stop("grid_var only allowed in conjunction with facet_var")}
    vars <- c(x, y)
    if(missing(by)==FALSE) {vars[length(vars)+1] <- by}
    if(missing(facet_var)==FALSE) {vars[length(vars)+1] <- facet_var}
    if(missing(grid_var)==FALSE) {vars[length(vars)+1] <- grid_var}
    dfin <- df[vars]
    s.names <- names(dfin)[-2]
    dfin.names <- c("x", "y", "by", "facet_var", "grid_var")
    names(dfin) <- dfin.names[1:length(dfin)]

    bylist.df <- dfin[-2]
    bylist <- list()
    for(i in 1:length(bylist.df)) {
        bylist[length(bylist)+1] <- bylist.df[i]
    }

    dfout <- aggregate(dfin$y, bylist, mean)
    ses <- aggregate(dfin$y,bylist,SEM)
    dfout <- cbind(dfout,ses[,ncol(ses)])
    names(dfout) <- c(s.names, 'y','se')

    if(missing(by)==TRUE & missing(facet_var)==TRUE & missing(grid_var)==TRUE)
    {output <- ggplot(dfout, aes_string(x=colnames(dfout)[1],
                                        y=colnames(dfout)[2])) +
        geom_line()
    }
    if(missing(by)==FALSE & missing(facet_var)==TRUE & missing(grid_var)==TRUE)
    {output <- ggplot(dfout, aes_string(x=colnames(dfout)[1],
                                        y=colnames(dfout)[3],
                                        color=colnames(dfout)[2])) +
        geom_line()
    }
    if(missing(by)==TRUE & missing(facet_var)==FALSE & missing(grid_var)==TRUE)
    {output <- ggplot(dfout, aes_string(x=colnames(dfout)[1],
                                        y=colnames(dfout)[3])) +
        geom_line() +
        facet_wrap(as.formula(paste("~", colnames(dfout)[2])))
    }
    if(missing(by)==FALSE & missing(facet_var)==FALSE &
       missing(grid_var)==TRUE) {
        output <- ggplot(dfout, aes_string(x=colnames(dfout)[1],
                                           y=colnames(dfout)[4],
                                           color=colnames(dfout)[2])) +
            geom_line() +
            facet_wrap(as.formula(paste("~", colnames(dfout)[3])))
    }
    if(missing(by)==FALSE & missing(facet_var)==FALSE &
       missing(grid_var)==FALSE) {
        output <- ggplot(dfout, aes_string(x=colnames(dfout)[1],
                                           y=colnames(dfout)[5],
                                           color=colnames(dfout)[2])) +
            geom_line() +
            facet_grid(as.formula(paste(colnames(dfout)[3],
                                        "~",
                                        colnames(dfout)[4])))
    }
    if(error_bars==TRUE) {
      output <- output +
        geom_errorbar(aes(ymin=y-se, ymax=y+se), width=.1)
    }
    output <- output +
      ylab(y)
    output
}
