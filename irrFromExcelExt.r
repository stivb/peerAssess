source("rfuncs.r")
library(readxl)
library(readr)
library(irrCAC)
library(data.table)
library(dplyr)
library(tools)



read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_names = FALSE,na=c("","*","-")))
  if(!tibble) x <- lapply(x, as.data.frame)
}

fileChosen<-file.choose(new = FALSE)
mysheets <- read_excel_allsheets(fileChosen)
tab_names <- excel_sheets(path = fileChosen)

numChosen<-as.numeric(readline(prompt="Choose weighting: (1) quadratic (2) ordinal (3) linear (4) radical (5) ratio (6) circular (7) bipolar (8) unweighted: "))

chosenWeighting<-switch(numChosen, 
          "quadratic",
          "ordinal",
          "linear",
          "radical",
          "ratio",
          "circular",
          "bipolar",
          "unweighted"
)
if (is.null(chosenWeighting)) chosenWeighting<-"unweighted"

print(chosenWeighting)

maxsheets = length(mysheets)

masterlist <- list() #create an empty list
looplist <- list()

lapply(mysheets, function(df) {
  looplist <- list()
  print(sprintf("Reading file %s",tab_names[[length(masterlist)+1]]))
  looplist[["fn"]]<-tab_names[[length(masterlist)+1]]
  
  
  for (ab in 1:4) 
    {
    nm<-paste0("pcAggreement",ab)
    print(nm)
    looplist[[nm]] <-doCalc(df,ab)
  }
  
  looplist[["pcAgg2"]]<-doCalc2(df,1)
  
  looplist[["pcBigGaps"]]<-countPercentileGaps(df)
  
  tmpDf <- cbind(df)
  #a line (switch on or off) to remove column names if necessary
  #colnames(tmpDf) <- NULL
  
  
  
  irrDf <- t(tmpDf)
  
  looplist[["gwet"]] <-doGwet(irrDf,chosenWeighting)
  
  looplist[["pa"]] <-doPA(irrDf,chosenWeighting)
  looplist[["krippAR"]] <-doKrippenAlphaRaw(irrDf,chosenWeighting)
  looplist[["fleissKappa"]] <-doFleissKappa(irrDf,chosenWeighting)
  looplist[["bp"]] <-doBPRaw(irrDf,chosenWeighting)
  #looplist[["conger"]]<-doCongerKappa(irrDf,chosenWeighting)
  
  allEntries<-unlist(df)
  allNumericEntries<-as.numeric(na.omit(allEntries))
  
  looplist[["To"]] = max(allNumericEntries)
  looplist[["From"]] = min(allNumericEntries)
  looplist[["nStudents"]] = nrow(df)
  #print(looplist)
  masterlist[[length(masterlist)+1]] <<- looplist
  
})

print(paste(masterlist[[1]]," "))

#do.call("rbind", masterlist)
qun<-data.table::rbindlist(masterlist)
qun<-qun %>% mutate_if(is.numeric, round, digits=3)
qun<-qun[!(qun$To<5),]
View(qun)

write.csv(qun,paste(file_path_sans_ext(basename(fileChosen)),chosenWeighting,".csv",sep=""))


qq<-cbind(qun)

qq$fn<-NULL
res <- cor(qq)
res<- round(res, 2)
print(res)


