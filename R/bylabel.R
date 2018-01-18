#' A Cat Function
#' bylabel.f
#' This function produces a list of names that corresponds to the combination of by variables in the tabstat function. It is not generally useful outside of that function.
#' @param comb A row of a matrix that represents by variables.
#' @param varlist A character vector of the names of by variables.
#' @keywords bylabel
#' @export
#' @examples
#' bylabel.f()

bylabel.f <- function(comb, varlist) {
    newlabel <- c("By ")
    comb <- as.character(comb)
    for(i in 1:length(comb)) {
        if(comb[i]!="0") {comb[i] <- varlist[i]}
    }
    comb <- comb[comb!=0]
    
    if(length(comb)==1) {newlabel <- paste0(newlabel, comb)}
    if(length(comb)==2) {newlabel <- paste0(newlabel, comb[1], 
                                            " & ", comb[2])}
    if(length(comb)>2) {
        for(j in 1:(length(comb)-1)) {
            newlabel <- paste0(newlabel, comb[j], ", ")
        }
        newlabel <- paste0(newlabel, "& ", comb[length(comb)])
    }
    newlabel
}