library(tidyverse)
library(reshape2)
mcars<-mtcars[,5:6]
x<-melt(mcars)
plt<-ggplot(data=x,aes(x=variable,y=value))
plt+geom_violin()+ theme_minimal() + labs(x="Title",y="x")