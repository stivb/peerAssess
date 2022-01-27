library(readr)
introsurvey <- read_csv("F:/2020-2021/7COM1079/surveys/introsurvey.csv")
df = subset(introsurvey, select = -c(Email,Name) )
for ( col in 1:ncol(df)){
  colnames(df)[col] <-  tail(strsplit(colnames(df)[col],split=" ")[[1]],1)
}
View(df)
print ("Mean of git level")
print (mean(as.numeric(unlist(df[,4]))))
print ("Table of git level")
print (table(as.numeric(unlist(df[,4]))))

print ("Mean of spreadsheets")
print (mean(as.numeric(unlist(df[,5]))))
print ("Table of spreadsheets")
print (table(as.numeric(unlist(df[,5]))))

print ("Mean of statistics")
print (mean(as.numeric(unlist(df[,6]))))
print ("Table of statistics")
print (table(as.numeric(unlist(df[,6]))))

print ("Mean of R")
print (mean(as.numeric(unlist(df[,7]))))
print ("Table of R")
print (table(as.numeric(unlist(df[,7]))))

print ("Mean of Kanban")
print (mean(as.numeric(unlist(df[,8]))))
print ("Table of Kanban")
print (table(as.numeric(unlist(df[,8]))))

print ("Table of voters")
print (unlist(df[,9]))

print ("Table of rainfall")
print (unlist(df[,10]))

print ("Table of basketball")
print (unlist(df[,11]))

print ("Table of pValue")
print (unlist(df[,13]))

print("Correlation betweeen knowledge of spreadsheets and knowledge of R")
cor(as.numeric(unlist(df[,5])),as.numeric(unlist(df[,7])))
plot(as.numeric(unlist(df[,5])),as.numeric(unlist(df[,7])))


