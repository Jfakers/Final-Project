# Running the intercoder Reliability Analysis for JA & CT Dec 14th 2017 
library(irr)  # running the intercoder reliability file
list.files()
d <- read.csv("IntercoderReliability.csv", stringsAsFactors = F)
# there should only be 16 rows
d<-d[d[c(1:16)], ]  # my indexing isn't working..
as.character(d) # hopefully this will let the test run
# lets try a Kappa Test
# For intrinsic motivation

kappa2(d[,c(1:2)], "unweighted")
