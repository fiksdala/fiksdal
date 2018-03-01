# Update fiksdal package
library("devtools")
library(roxygen2)
setwd("/Users/alex/Documents/R/fiksdal")
document()
install_github('fiksdala/fiksdal')
library(fiksdal)
