#' tabstat
#'
#' This function produces descriptive statistics with a flexibility similar to that of the Stata tabstat function. It can produce statstics of any number of groups and subgroups. It can return any percentile value when using "ptileXX.XX" (quotes are required).
#' @param mydata A dataframe.
#' @param myvars A character vector of variables to be summarized.
#' @param mystats A list of statistics to be included in the summary. NOTE: This must be included in the command as list() and cannot be a pre-existing object.
#' @param byvars A character vector of by variables (i.e. variables representing groups or subgroups to be included in the summary).
#' @param roundto digits to round to, default does not specify
#' @keywords summary statistics, tabstat
#' @export
#' @examples
#' tabstat

tabstat <- function(mydata, myvars, mystats, byvars, roundto=NULL) {
    # Create core structures
    mystats1 <- as.character(lapply(substitute(mystats),toString))[-1]
    df.myvars <- mydata[myvars]
    t.matrix <- matrix(nrow=length(myvars), ncol=length(mystats))
    out.list <- list()

    # Replace ptiles with functions
    for (i in 1:length(mystats1)) {
        if (ifelse(grepl("p", mystats1[i]),"yes","no")=="yes") {
            probs <- as.numeric(strsplit(mystats1[i], "ptile")[[1]][2])/100
            mystats[[i]] <- function(x, na.rm=TRUE) {
                quantile(x, probs, type=2, na.rm=na.rm)
            }
            mystats1[i] <- paste0("p", probs*100)
        }
        else {mystats[[i]] <- mystats[[i]]}
    }

    # Make total output
    for (i in 1:length(mystats)) {
        t.matrix[,i] <- apply(df.myvars, 2, mystats[[i]], na.rm=TRUE)
    }
    rownames(t.matrix) <- myvars
    colnames(t.matrix) <- mystats1
    out.list[[length(out.list)+1]] <- t.matrix

    # By-Structures
    if(missing(byvars)==FALSE) {
        byvars.list <- list() # byvar list of lists
        v.comb <- vector("list", length(byvars)) # variable combinations
        for (i in 1:length(byvars)) {
            byvars.list[[length(byvars.list)+1]] <- c(as.matrix(mydata
                                                                [byvars[i]]))
            v.comb[[i]] <- c(0:1)
        }
        ## Make combination matrix
        c.matrix <- expand.grid(v.comb)
        ## Order the c.matrix by sum
        c.matrix <- cbind(c.matrix, rowSums(c.matrix))
        c.matrix <- c.matrix[order(c.matrix[length(c.matrix)]),]
        c.matrix <- c.matrix[-1,] # remove unnecessary first row
        ## rename colnames to numbers
        colnames(c.matrix) <- c(1:length(c.matrix))
        c.matrix <- c.matrix[-ncol(c.matrix)] # drop last column for next step

        ## rename elements to reflect colnames
        for(i in 1:ncol(c.matrix)) {
            c.matrix[,i][c.matrix[,i]==1] <- i
        }
        f.byvars.list <- list()
        for(i in 1:nrow(c.matrix)) {
            nlist <- as.list(c.matrix[i,])
            nlist[nlist==0] <- NA
            for(j in 1:length(byvars.list)){
                if(is.na(nlist[[j]])==FALSE) {nlist[[j]] <- byvars.list[[j]]}
            }
            names(nlist) <- byvars
            nlist <- nlist[!is.na(nlist)]
            f.byvars.list[[length(f.byvars.list)+1]] <- nlist # Specify
        }
        for(i in 1:length(f.byvars.list)) {
            for (j in 1:length(mystats)) {
                agt <- aggregate(df.myvars, f.byvars.list[[i]], mystats[[j]],
                                 na.rm=TRUE)
                ag1 <- as.matrix(agt[-1:-length(f.byvars.list[[i]])])
                ag2 <- as.matrix(agt[1:length(f.byvars.list[[i]])])

                if(j==1) {
                    y <- ncol(ag1)
                    output <- matrix(nrow=nrow(ag2)*y, ncol=ncol(ag2))
                    for(k in 1:ncol(ag2)) {
                        output[,k] <- c(rep(ag2[,k], y))
                    }
                    output <- data.frame(output)
                    colnames(output) <- colnames(ag2)

                    names <- colnames(ag1)
                    Variable <- c()
                    for (h in 1:length(names)) {
                        Variable <- append(Variable, rep(names[h], nrow(ag1)))
                    }
                    output <- cbind(output, Variable)
                }
                output <- cbind(output, ag1[1:length(ag1)])
            }
            colnames(output)[(length(output)-length(mystats)+1):
                                 length(output)] <- mystats1
            out.list[[length(out.list)+1]] <- output
        }
    }

    # Labels for out.list
    if(missing(byvars)==FALSE) {
        c.matrix.l <- c()
        for (i in 1:nrow(c.matrix)) {
            c.matrix.l[length(c.matrix.l)+1] <- bylabel.f(c.matrix[i,], byvars)
        }
        c.matrix.l <- append("Total", c.matrix.l)
        names(out.list) <- c.matrix.l
    }
    if(missing(byvars)==TRUE) {
        names(out.list) <- "Total"
    }
    if(is.null(roundto)==FALSE) {
      out.list <- roundan(out.list, roundto)
    }
    out.list
}
