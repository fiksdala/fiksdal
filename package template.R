setwd("/Users/alex/Documents/R")
library("devtools")
library(roxygen2)

create("fiksdal")
setwd("./fiksdal")
document()
setwd("..")
install("fiksdal")
library(fiksdal)


# Update fiksdal package
library("devtools")
library(roxygen2)
setwd("/Users/alex/Documents/R/fiksdal")
document()
setwd("..")
install("fiksdal")
library(fiksdal)
