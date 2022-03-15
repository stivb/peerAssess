library(readr)
df4 <- read_csv("~/research/results/df4.csv")
dfz<-melt(setDT(df4), id = 1L, 
      measure = list(c(4,7,10), c(5,8,11), c(6,9,12)), 
      value.name = c("Site", "Scores", "Feedback"))
write.csv(dfz,file="~/research/results/reorg.csv")