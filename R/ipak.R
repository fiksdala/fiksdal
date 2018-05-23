#' ipak
#'
#' This is Steven Worthington's excellent ipak function. I've included it
#' here just for my personal use and convenience and to avoid loading more
#' packages than I need.
#' Original code and Steven's package can be found:
#' https://gist.github.com/stevenworthington/3178163
#' "ipak function: install and load multiple R packages.
#' check to see if packages are installed. Install them if they are not, then
#' load them into the R session."
#' @param pkg Character vector of packages.
#' @keywords package
#' @export
#' @examples
#' ipak()

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
