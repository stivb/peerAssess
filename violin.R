library(tidyverse)
library(reshape2)
#mcars<-mtcars[,5:6]
#x<-melt(mcars)
#plt<-ggplot(data=x,aes(x=variable,y=value))
#plt+geom_violin()+ theme_minimal() + labs(x="Title",y="x")

x<-scan.str("90.50 87.68 87.65 86.70 78.43 77.78 77.28 76.10 74.35 73.80 71.59 71.50 70.55 68.40 68.05 67.78 64.74 62.95 61.95 60.10 59.15 58.70 57.78 56.00 52.25 51.85 51.59 48.55 47.15 45.85 45.78 28.75 26.60 25.97 21.66 19.00 9.50" )
df <- data.frame(matrix(ncol = 2, nrow = length(x)))
colnames(df)<-c("variable","value")
df$variable<-"mark"
df$value<-x
plt<-ggplot(data=df,aes(x=variable,y=value))
plt+geom_violin()+ 
  geom_dotplot(binaxis = "y",stackdir = "center",dotsize = 0.5) + 
  theme_minimal() + labs(x="Title",y="x") + 
  scale_y_continuous(breaks = seq(0, 100, by=10), limits=c(0,100))
