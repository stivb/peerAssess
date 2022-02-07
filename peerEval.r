source("peerFuncs.r")
library(readxl)
library(readr)
library(xtable)


doTables<-function(tableList,dStem)
{
  for (name in names(tableList)) {
    csvPath<-paste(dStem,"/",name,".csv",sep="")
    htmlPath<-paste(dStem,"/",name,".htm",sep="")
    tbl<-tableList[[name]]
    tblDf<-apply(tbl,2,as.character)
    write.csv(tblDf,csvPath)
    #print.xtable(tblDf, type="html", file=htmlPath)
  }
  
}

#############BODY###########################


fileChosen<-file.choose(new = FALSE)
excelPath <- dirname(fileChosen)


mysheets <- read_excel_allsheets(fileChosen)
tab_names <- excel_sheets(fileChosen)
maxsheets = length(mysheets)

##defaults###
weightsToUse<-"ratio"
uniformMarkRange=TRUE
minScore=1
maxScore=5
nCriteria=2
criteriaProprotions=c(1,1)
tutorStudentProportions<-c(80,20)
criteriaShortNames<-sprintf("c%d", 1:nCriteria) 
criteriaLongNames<-sprintf("Criteria%d", 1:nCriteria) 

graphsToPlot<-c(
   "doSummativeGraph",
   "doMarkerStacks",
   "doHeatMaps",
   "doStacks",
   "doHists"
  )


#############various lists used to compile output sheets##################
workSheetList <- list() #create an empty list
tmpWorkSheetList <- list()
markerAuditList<-list()
tmpMarkerAuditList<-list()
gradeSheetList<-list()


##################output sheets###########################################
#concept: 
# a "report" is for a single event, 
# "analytics" is over multiple events

tblMarksReport<-data.frame()  #single Event (marks given to groups)
tblMarkingAuditReport<-data.frame()  #single Event (checks marker probity/competence)
tblTutorStudentCorrReport<-data.frame() #single Event


tblIRRAnalytics<-data.frame()  #multiple events (irr for multiple marking events)
tblIRRCorrAnalytics<-data.frame() #multiple events (the above with correlations between all the measures)
tblTutorStudentCorrAnalytics<-data.frame() 

destStem<-""

#############################################################


lapply(mysheets, function(df) {
  
  topLeftStr<-paste(as.character(df[1,1]))
  numcols<-ncol(df)
  
  tabName<-tab_names[[length(workSheetList)+1]]
  putativePath = paste(excelPath,tabName,sep="/")
  ifelse(!dir.exists(putativePath), dir.create(putativePath), "Folder exists already")
  
  destStem<<-paste(putativePath, "/", tabName,sep="")
  
  if (str_detect(topLeftStr,regex("[^\\W][0-9]")))
  {

    df_s<-tutorStudentSplitSheet(df)
    
    #now removing the markerGroupId column from the newly created dataframes
    dfStudents<-df_s$`0`[, -ncol(df_s$`0`)]
    dfTutors<-df_s$`1`[-1, -ncol(df_s$`1`)]

    studentAveScores<-colMeans(as.data.frame(sapply(dfStudents,as.numeric)),na.rm=TRUE)
    if (nrow(dfTutors)==1) tutorAveScores<-as.numeric(dfTutors)
    else tutorAveScores<-colMeans(as.data.frame(sapply(dfTutors,as.numeric)),na.rm=TRUE)
    
    
    #am thinking the tscomparison analytics is canonical rather than event
    tblTutorStudentCorrReport<-doTSCorrReport(studentAveScores,tutorAveScores,nCriteria)
    
    tblMarksReport<<-makeGradeSheet(studentAveScores,tutorAveScores,2,criteriaProprotions,tutorStudentProportions)
    
    
    
    doPlot(as.numeric(tutorAveScores), studentAveScores,destStem)
    tMean<-mean(tutorAveScores, na.rm=TRUE)
    tSd<-sd(tutorAveScores, na.rm=TRUE)
    
    
    
    for (ct in 1:nrow(dfStudents)) 
      {
      
      indStudMarking<-as.numeric(unlist(dfStudents[ct,],use.names=FALSE))
      
      markerAuditList[[length(markerAuditList)+1]] <<- 
        auditIndStudentMarking(rownames(dfStudents)[ct], indStudMarking, 
                               tutorAveScores, minScore,maxScore)
    }
    
    tblMarkingAuditReport<<-as.data.frame(do.call(rbind, markerAuditList))

    df<-dfStudents
    }
    
  
  
  tmpWorkSheetList <- list()
  tmpWorkSheetList[["fn"]]<-tabName
  
  tmpDf <- cbind(df)
  colnames(tmpDf) <- NULL
  irrDf <- t(tmpDf)
  nullVotesList<-propUniforms(df)
  nullVotesPercent<-length(nullVotesList[nullVotesList>0.05])/length(nullVotesList)

  tmpWorkSheetList<-getAgreements(as.data.frame(irrDf),weightsToUse,tmpWorkSheetList)
  tmpWorkSheetList<-getCustomMeasures(df,maxScore,minScore,tmpWorkSheetList)
  tmpWorkSheetList<-addMarkCounts(unlist(df), tmpWorkSheetList, 1, 5)
  
  
  doHist(df, graphsToPlot,destStem)
  doMarkerStack(df, graphsToPlot,destStem)
  doHeatMap(df,graphsToPlot,destStem)
  doCriterionViolins(df,graphsToPlot,destStem)
  
  
  #doCGram(df, destStem)
  
  #tmpWorkSheetList[["iccNA"]]<-doICCNA(irrDf)
  #print("iccNa")
  #print(tmpWorkSheetList[["iccNA"]])
  
  doEvtIrrCsv(tmpWorkSheetList,destStem)
  workSheetList[[length(workSheetList)+1]] <<- tmpWorkSheetList
  
})


do.call("rbind", workSheetList)
tblIRRAnalytics<-data.table::rbindlist(workSheetList)

print("Completed")




#View(tblMarksReport)
##View(tblMarkingAuditReport)
#View(tblIRRAnalytics)
#View(tblIRRCorrAnalytics)

#outputSheets<-list(tblMarksReport,tblMarkingAuditReport,tblIRRAnalytics)

outputSheets<-list()
outputSheets[[ "tblMarksReport" ]] <- tblMarksReport
outputSheets[[ "tblMarkingAuditReport" ]] <- tblMarkingAuditReport
outputSheets[[ "tblIRRAnalytics" ]] <- tblIRRAnalytics


doTables(outputSheets,excelPath)

mks <- xtable(tblMarksReport)
print(mks,type="html")

# 
# View(q)
# 
# qq<-cbind(q)
# 
# qq$fn<-NULL
# qq<-qq[,2:(length(qq)-11)]
# res <- cor(qq,use="pairwise.complete.obs")
# res<- round(res, 2)
# print(res)

##just getting the numeric only columns from the data table

#sheetCounts<- q %>% dplyr::select(matches("^[0-9]*$"))
#doSummativeGraph(sheetCounts, graphsToPlot)

# do.call("rbind", markerAuditList)
# mAL<-data.table::rbindlist(markerAuditList)
# View(mAL)

