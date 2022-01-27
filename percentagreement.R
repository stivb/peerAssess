percentages<-c()
totalJudgements=0
totalDisagreements=0
for (i in names(X17_18IMa4)) 
{
  cc<-X17_18IMa4[i]
  qq<-na.omit(cc[[1]])
  zz<-combn(qq,2,simplify=FALSE)
  totalJudgements=length(zz) + totalJudgements
  tbl
  pc<-prop.table(table(unlist(lapply(zz,function(x) abs(x[1]-x[2])>1))))["FALSE"]
  percentages <-append(percentages,pc)
  totalDisagreements = as.vector(pc)[1] + totalDisagreements
}
print(totalJudgements)
print(totalDisagreements)