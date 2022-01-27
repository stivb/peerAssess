source("rfuncs.r")
library(stringr)

args = commandArgs(trailingOnly=TRUE)
infile<-args[[1]]
ofile<-str_replace(args[[1]],"csv","png")
groupSize<-as.numeric(args[[2]])

data <- read_csv(infile, col_names = FALSE)

graphVoting(data,groupSize,ofile)

