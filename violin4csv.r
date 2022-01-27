library(ggplot2)
library(reshape2)
library(readr)
fileChosen<-file.choose(new = FALSE)
df<- read_csv(fileChosen, col_names = FALSE, col_types = cols())
df.m<-reshape2::melt(df,id.vars=NULL)
ggplot(df.m,aes(x=variable,y=value)) + geom_boxplot(outlier.colour="yellow", outlier.shape=16,
                                                    outlier.size=2) + stat_summary(fun.y=mean, geom="point", size=2, color="red") +theme_dark()

