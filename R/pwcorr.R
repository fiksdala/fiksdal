#' pwcorr
#'
#' This function reorganizes the rcorr function in the Hmisc package in a way that more closely resembles the pwcorr Stata function. It produces a single table with r, n, and p values, omitting the upper triangle.
#' @param data The dataset.
#' @param x A character vector of variables to be included in the correlation matrix.
#' @keywords correlation
#' @export
#' @examples
#' pwcorr()

pwcorr <- function(data, x) {
    suppressMessages(require(Hmisc))
    a <- data[x]
    b <- rcorr(as.matrix(a))
    output.n <- matrix(ncol=length(x))
    output.c <- matrix(ncol=length(x))
    d <- c()
    
    b1 <- round(matrix(as.numeric(unlist(b[1])), 
                       ncol=length(x), 
                       nrow=length(x)), digits=4)
    b2 <- round(matrix(as.numeric(unlist(b[2])), 
                       ncol=length(x), 
                       nrow=length(x)), digits=4)
    b3 <- round(matrix(as.numeric(unlist(b[3])), 
                       ncol=length(x), 
                       nrow=length(x)), digits=4)
    
    b1.1 <- b1
    b2.1 <- b2
    b3.1 <- b3
    
    b1.1[upper.tri(b1.1, diag=FALSE)] <- NA
    b2.1[upper.tri(b2.1, diag=TRUE)] <- NA
    b3.1[upper.tri(b3.1, diag=TRUE)] <- NA
    
    for(i in 1:length(x)) {
        output.n1 <- rbind(b1[i,], b3[i,], b2[i,])
        output.n <- rbind(output.n, output.n1)
        output.c1 <- rbind(b1.1[i,], b3.1[i,], b2.1[i,])
        output.c <- rbind(output.c, output.c1)
        d1 <- c(x[i], "", "")
        d <- append(d, d1)
    }
    output.n <- output.n[-1,]  
    output.c <- output.c[-1,]  
    colnames(output.n) <- x
    rownames(output.n) <- d
    colnames(output.c) <- x
    rownames(output.c) <- d
    output.c <- formatC(output.c, format="f", digits=4)
    output.c[output.c=="   NA"] <- ""
    output <- list(output.n=output.n, output.c=output.c)
    new("pwcorr_class", output)
}
