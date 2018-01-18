#' lmervis
#'
#' This function produces line graphs based on lmer() models. It can incorporate multiple slope variables, especially useful for time series analyses.
#' @param df Datafram.
#' @param model A lmer() model object.
#' @param x Variable to appear on the x axis.
#' @param xlevels A character vector of levels. "identity" utilizes existing levels.
#' @param timevary Any variable that varies in conjunction with time.
#' @param timevarylevels List of levels of timevary variables. Must be in a list and of equal length to x.
#' @param byvars A list of by variables.
#' @param bylevels A list containing the corresponding levels of the by variables. "identity" utilizes existing levels, "sd" returns -1 and +1 SD relative to mean.
#' @param combinedx Numerical vector. If the time series (x and timevary) are split in ways that do not overlap, combinedx should be included. It must be a list of equal length to x and timevary.
#' @param combinedxname The name of the combinedx variable.
#' @keywords lmer, ggplot, line graph
#' @export
#' @examples
#' n()

lmervis <- function(df, model, x, xlevels=NULL, byvars=NULL, bylevels=NULL, 
                    covars=NULL, covars.lv=NULL, cint=FALSE, nsim=500,
                    seed=1234, outtype="plot", xcov=NULL, xcov.lv=NULL) {
    # Convenience function
    combfunk <- function(x, y) {
        output <- c()
        for (i in 1:length(y)) {
            output <- append(output, rep(y[i], length(x)))
        }
        output
    }
    
    # Drop unused levels
    df <- droplevels(df)
    
    # Make empty lists for predict: x, by, covars
    modeldf <- get_all_vars(model, df) # DF with model vars only
    ID <- modeldf[length(modeldf)] # Save ID vector just in case
    y <- modeldf[1]
    yname <- names(y)
    modeldf <- modeldf[1:length(modeldf)-1] # Get rid of ID var
    modeldf <- modeldf[2:length(modeldf)] # Get rid of Y
    varlist <- colnames(modeldf) # Complete varlist
    
    ## covars.length
    covars.ind <- length(varlist)-1-length(xcov)
    
    ## xlist
    xlist <- vector("list", 1)
    
    ## bylist if necessary
    if(is.null(byvars)[1]==FALSE) {
        bylist <- vector("list", length(byvars))
        names(bylist) <- byvars
        ### bylist.g
        bylist.g <- vector("list", length(byvars))
        names(bylist.g) <- byvars
        covars.ind <- covars.ind-length(byvars)
    }
    
    ## covars.list (if necessary)
    if(covars.ind>0) {
        covars.list <- list()
    }
    
    # Fill xlist
    if(is.null(xcov)==TRUE) { # Make sure x is in the modeldf
        for (i in 1:length(varlist)){
            if (identical(varlist[i], x)==TRUE) {
                if(is.null(xlevels)==TRUE) {
                    xlist <- levels(as.factor(modeldf[[i]]))
                    if(is.factor(modeldf[[i]])==FALSE) {
                        xlist <- as.numeric(xlist)
                    }
                }
                else {
                    if(xlevels[1] == "sd") {
                        xlist <- c(mean(modeldf[,i],na.rm=TRUE) -
                                       sd(modeldf[,i],na.rm=TRUE),
                                   mean(modeldf[,i],na.rm=TRUE) +
                                       sd(modeldf[,i],na.rm=TRUE))
                    }
                    else {xlist <- xlevels}
                }
            }
        }
        xlist <- list(xlist)
        names(xlist) <- x
    }
    
    ## If there are xcovs
    if(is.null(xcov)==FALSE) {
        xlist[[1]] <- xlevels
        names(xlist) <- x
    }
    
    
    # Fill bylist (if necessary)
    ## Confirm there are byvars
    if(is.null(byvars)[1]==FALSE) {
        ## Fill bylist by bylevels specification
        for(i in 1:length(byvars)) {
            ### match byvars index to varlist
            tempi <- which(byvars[i]==varlist)
            if(length(tempi)==0) {stop("Error: check variable specification")}
            ### identity
            if(bylevels[i][1]=="identity") {
                bylist[[i]] <- levels(as.factor(modeldf[[tempi]]))
                bylist.g[[i]] <- levels(as.factor(modeldf[[tempi]]))
                #### back to numeric if necessary
                if(is.numeric(modeldf[[tempi]]==TRUE)) {
                    bylist[[i]] <- as.numeric(bylist[[i]])
                }
            }
            ### sd
            else if(bylevels[i][1]=="sd") {
                bylist[[i]] <- c(mean(modeldf[,tempi],na.rm=TRUE) -
                                     sd(modeldf[,tempi],na.rm=TRUE),
                                 mean(modeldf[,tempi],na.rm=TRUE) +
                                     sd(modeldf[,tempi],na.rm=TRUE))
                bylist.g[[i]] <- c("-1 SD", "+1 SD")
            }
            ### otherwise use specified
            else {
                bylist[[i]] <- bylevels[[i]]
                bylist.g[[i]] <- levels(as.factor(bylevels[[i]]))
            }
        }
    }
    
    # Fill covars.list
    ## Confirm covars exist
    if (covars.ind>0) {
        # if any covars were specified
        if(is.null(covars)==FALSE) {
            # match and fill specified covars
            for (i in 1:length(covars)) {
                tempi <- which(covars[i], varlist)
                covars.list[length(covars.list)+1] <- covars.lv[i]
                names(covars.list)[length(covars.list)] <- varlist[tempi]
            }
        }
        # for those that were not specified
        else {
            for(i in 1:length(varlist)) {
                # confirm index not already specified elsewhere
                if(varlist[i] %in% byvars==FALSE &&
                   varlist[i] %in% x==FALSE &&
                   varlist[i] %in% covars==FALSE &&
                   varlist[i] %in% xcov==FALSE) {
                    covars.list[length(covars.list)+1] <- mean(modeldf[,i])
                    names(covars.list)[length(covars.list)] <- varlist[i]
                }
            }
        }
    }
    
    # Make predictdf and predictdf.g
    ## If byvars present, make the DF
    ### Get the dfbylist with appropriate repetitions
    if(is.null(byvars)[1]==FALSE) {
        dfbylist <- bylist
        dfbylist.g <- vector("list", length(bylist.g))
        dfbylist.g <- bylist.g
        if(length(bylist)>1) {
            for(i in 2:length(bylist)) {
                dfbylist[[i]] <- combfunk(dfbylist[[i-1]], dfbylist[[i]])
                dfbylist.g[[i]] <- combfunk(dfbylist.g[[i-1]], dfbylist.g[[i]])
            }
        }
        names(dfbylist) <- byvars
        names(dfbylist.g) <- byvars
    }
    
    ### Make predictdf if byvars present
    if(is.null(byvars)[1]==FALSE) {
        newxlist <- c()
        for (i in 1:length(xlist[[1]])) {
            newxlist <- append(newxlist, 
                               rep(xlist[[1]][i], 
                                   length(dfbylist[[length(dfbylist)]])))
        }
        if(covars.ind==0) {
            predictdf <- data.frame(newxlist, dfbylist)
            colnames(predictdf)[1] <- x
            predictdf.g <- data.frame(newxlist, dfbylist.g)
            colnames(predictdf.g)[1] <- x
        }
        if(covars.ind>0) {
            predictdf <- data.frame(newxlist, dfbylist, covars.list)
            colnames(predictdf)[1] <- x
            predictdf.g <- data.frame(newxlist, dfbylist.g)
            colnames(predictdf.g)[1] <- x
        }
    }
    
    ### Make predictdf if byvars not present
    if(is.null(byvars)[1]==TRUE) {
        if(covars.ind==0) {
            predictdf <- data.frame(xlist[[1]])
            colnames(predictdf)[1] <- x
            predictdf.g <- data.frame(xlist[[1]])
            colnames(predictdf.g)[1] <- x
        }
        if(covars.ind>0) {
            predictdf <- data.frame(xlist[[1]], covars.list)
            colnames(predictdf)[1] <- x
            predictdf.g <- data.frame(xlist[[1]])
            colnames(predictdf.g)[1] <- x
        }
    }
    
    # Add xcovs if present
    if(is.null(xcov)==FALSE) {
        if(length(xcov.lv[[1]])!=length(xlevels)) {
            stop("xlevels must exist and length(xcov.lv) must equal xlevels")
        }
        mergedf <- data.frame(c(1:nrow(predictdf)))
        mergedf <- cbind(mergedf, xcov.lv, temp=c(1:length(xcov.lv[[1]])))
        mergedf <- mergedf[order(mergedf$temp),]
        mergedf <- mergedf[c(-1,-length(mergedf))]
        names(mergedf) <- xcov
        predictdf <- cbind(predictdf, mergedf)
    }
    
    # Save predictdf without y
    newdata <- predictdf
    
    # Get yhat values
    y1 <- predict(model, newdata=predictdf, re.form=NA)
    predictdf <- cbind(predictdf, y1)
    predictdf.g <- cbind(predictdf.g, y1)
    colnames(predictdf)[ncol(predictdf)] <- yname
    colnames(predictdf.g)[ncol(predictdf.g)] <- yname
    
    # Get CIs if necessary
    if(cint==TRUE) {
        bootvals <- bootMer(model,
                            FUN=function(x) predict(x, newdata=newdata,
                                                    re.form=NA),
                            nsim=nsim,
                            use.u=FALSE, type="parametric", seed=seed)
        lower <- apply(bootvals$t, 2, quantile, 0.025)
        upper <- apply(bootvals$t, 2, quantile, 0.975)
        predictdf <- cbind(predictdf, lower, upper)
        predictdf.g <- cbind(predictdf.g, lower, upper)
    }
    
    ## for no byvars base plot
    if (is.null(byvars)[1]==TRUE) {
        outplot <- ggplot(predictdf.g, aes_string(x=names(predictdf.g)[1],
                                                  y=yname)) +
            geom_line()
    }
    
    ### for 1 byvar
    if (is.null(byvars)[1]==FALSE && length(byvars)==1) {
        outplot <- ggplot(predictdf.g,
                          aes_string(x=names(predictdf.g)[1],
                                     y=yname,
                                     color=names(predictdf.g)[2])) +
            geom_line()
    }
    
    ## for 2 byvars
    if (is.null(byvars)[1]==FALSE && length(byvars)==2) {
        outplot <- ggplot(predictdf.g,
                          aes_string(x=names(predictdf.g)[1],
                                     y=yname,
                                     color=names(predictdf.g)[2])) +
            geom_line() +
            facet_grid(as.formula(paste("~", names(predictdf.g)[3]))) +
            theme(aspect.ratio=1)
    }
    
    ## for 3 byvars
    if (is.null(byvars)[1]==FALSE && length(byvars)==3) {
        outplot <- ggplot(predictdf.g,
                          aes_string(x=names(predictdf.g)[1],
                                     y=yname,
                                     color=names(predictdf.g)[2])) +
            geom_line() +
            facet_grid(as.formula(paste(names(predictdf.g)[3],
                                        "~", names(predictdf.g)[4]))) +
            theme(aspect.ratio=1)
    }
    
    #### for confidence intervals
    if(cint==TRUE) {
        outplot <- outplot +
            geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2)
    }
    
    # Output options
    if(outtype=="plot") {
        output <- outplot
    }
    if(outtype=="plotdf") {
        output <- predictdf.g
    }
    if(outtype=="both") {
        output <- list(plotdf=predictdf.g, plot=outplot)
    }
    if(outtype=="predict") {
        output <- predictdf
    }
    output
}

