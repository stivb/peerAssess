library(svDialogs)
userinput <- dlgInput("Either enter a single number for the number of criteria \n Or a series of numbers separated by comma indicating proportions per criterion", Sys.info()["user"])$res
valz <- strsplit(userinput, split = ",")
if (length(valz[[1]])==1)
{
  nCriteria<-as.numeric(valz[[1]][1])
  criteriaProportions<-rep(1, nCriteria)
} else
{
  nCriteria<-length(valz[[1]])
  criteriaProportions<-as.numeric(valz[[1]])
}
print(nCriteria)
print(criteriaProportions)
