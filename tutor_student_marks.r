library(readxl)
library(ggpubr)

studentFile<-file.choose(new = FALSE)
tutorFile<-file.choose(new = FALSE)

studentsheets <- read_excel_allsheets(studentFile)
tutorsheets <- read_excel_allsheets(tutorFile)

sheetNames<-excel_sheets(path=studentFile) 

print(length(studentsheets))
print(length(tutorsheets))

studentJgmtMeansBySheet <- c()
tutorJgmtMeansBySheet <- c()

lapply(studentsheets, function(df) {
  studentJgmtMeansBySheet[[length(studentJgmtMeansBySheet)+1]] <<- colMeans(df,na.rm=TRUE)
})


lapply(tutorsheets, function(df) {
  tutorJgmtMeansBySheet[[length(tutorJgmtMeansBySheet)+1]] <<- colMeans(df,na.rm=TRUE)
})


for (i in 1:length(studentJgmtMeansBySheet)){
  corr.result<-cor.test(studentJgmtMeansBySheet[[i]], tutorJgmtMeansBySheet[[i]], method="pearson")
  print(round(corr.result$estimate,2))
  print(round(corr.result$p.value,2))
  print("--------------------------------------------------------------------------------")
  theTable<-do.call(rbind, Map(data.frame, s=studentJgmtMeansBySheet[[i]], t=tutorJgmtMeansBySheet[[i]]))
  ggscatter(theTable, x = "s", y = "t", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Students", ylab = "Tutors") + ggtitle(paste("Sheet",i,sheetNames[i],sep=":"))
  ggsave(paste(sheetNames[i],"scatter.png",sep=""))
  
  ggplot(theTable, aes(x = as.numeric(gsub("...","",row.names(theTable))))) +
         geom_line(aes(y = s), color = "steelblue", linetype="twodash") + 
           geom_line(aes(y = t), color="darkred") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + 
    ggtitle(paste("Sheet",i,sheetNames[i],sep=":"))
  ggsave(paste(sheetNames[i],"_lineplots.png",sep=""))
}






