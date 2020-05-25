#####################################################
## University Ranking Data Import - Jay Au [URN:6361715]
#####################################################

#please reset RStudio by using Ctrl+Shift+F10 & The following command to clean the environment:
rm(list = ls())

#note: 2017 raw data is of a different format, current lazy fix to create seperate 17 functions

#####################################################
## Initializing Libraries and Folder Paths
#####################################################

library(readxl)
library(Bolstad2)
library(ggplot2)
library(janitor)
library(stringr)
library(DT)

setwd("D:\\Libraries\\Documents\\University Work\\Dissertation v2")

#####################################################
## Importing Data
#####################################################

#FUNCTIONS#
{
  #selecting the satisfaction data based on Q27 and selecting first degree graduates
  selectData <- function(rawData, qNumber, level = "First degree"){
    selectData <- rawData[with(rawData, `Question Number` == qNumber & `Level` == level),]
    return(selectData)
  }
  
  #selects all data on all departments of unique universities basis
  selectDataSubject <- function(data, subjectName){
    selectDataSubject <- data[which(grepl(subjectName, data$Subject, fixed=TRUE)),]
    return(selectDataSubject)
  }
  
  #cleaning universities data set
  cleanDataSubject <- function(data){
    cleanDataSubject <- data[,c(2,7:11,16)]
    
    for (i in 1:5){
      cleanDataSubject[,paste("Sample",i)] <- round(cleanDataSubject[paste("Answered",i)]*cleanDataSubject$Response)
    }
    
    cleanDataSubject$calculatedSample <- rowSums(cleanDataSubject[,8:12])
    cleanDataSubject$positiveSample <- rowSums(cleanDataSubject[,11:12])
    cleanDataSubject$errorTotalSample <- cleanDataSubject$Response - cleanDataSubject$calculatedSample
    cleanDataSubject <- cleanDataSubject[,-c(2:7)]
    row.names(cleanDataSubject) <- NULL
    return(cleanDataSubject)
  }
  
  cleanDataSubject17 <- function(data){
    cleanDataSubject <- data[,c(2,6:10,15)]
    
    for (i in 1:5){
      cleanDataSubject[,paste("Sample",i)] <- round(cleanDataSubject[paste("Answered",i)]*cleanDataSubject$Response)
    }
    
    cleanDataSubject$calculatedSample <- rowSums(cleanDataSubject[,8:12])
    cleanDataSubject$positiveSample <- rowSums(cleanDataSubject[,11:12])
    cleanDataSubject$errorTotalSample <- cleanDataSubject$Response - cleanDataSubject$calculatedSample
    cleanDataSubject <- cleanDataSubject[,-c(2:7)]
    row.names(cleanDataSubject) <- NULL
    colnames(cleanDataSubject)[1] <- "Provider"
    return(cleanDataSubject)
  }
  
  #combining the three subject functions together neatly
  summaryDataSubjectFunction <- function(rawData, qNum, subjectID){
    finalData <- selectData(rawData, qNum)
    finalData <- selectDataSubject(finalData, subjectID)
    finalData <- cleanDataSubject(finalData)
    #finalData <- summationFunction(finalData)
    finalData <- finalData %>% adorn_totals("row")
    return(finalData)
  }
  
  summaryDataSubjectFunction17 <- function(rawData, qNum, subjectID){
    finalData <- selectData(rawData, qNum)
    finalData <- selectDataSubject(finalData, subjectID)
    finalData <- cleanDataSubject17(finalData)
    #finalData <- summationFunction(finalData)
    finalData <- finalData %>% adorn_totals("row")
    return(finalData)
  }
  
  #selects all data on all universities of unique departments basis
  selectDataUniversity <- function(data, uniName){
    selectDataUniversity <- data[which(grepl(uniName, data$Provider, fixed=TRUE)),]
    return(selectDataUniversity)
  }
  
  selectDataUniversity17 <- function(data, uniName){
    selectDataUniversity <- data[which(grepl(uniName, data$Institution, fixed=TRUE)),]
    return(selectDataUniversity)
  }
  
  #cleaning departmental data set
  cleanDataUniversity <- function(data){
    cleanDataUniversity <- data[,c(4,7:11,16)]
    
    for (i in 1:5){
      cleanDataUniversity[,paste("Sample",i)] <- round(cleanDataUniversity[paste("Answered",i)]*cleanDataUniversity$Response)
    }
    
    cleanDataUniversity$calculatedSample <- rowSums(cleanDataUniversity[,8:12])
    cleanDataUniversity$positiveSample <- rowSums(cleanDataUniversity[,11:12])
    cleanDataUniversity$errorTotalSample <- cleanDataUniversity$Response - cleanDataUniversity$calculatedSample
    cleanDataUniversity <- cleanDataUniversity[,-c(2:7)]
    return(cleanDataUniversity)
  }
  
  cleanDataUniversity17 <- function(data){
    cleanDataUniversity <- data[,c(3,6:10,15)]
    
    for (i in 1:5){
      cleanDataUniversity[,paste("Sample",i)] <- round(cleanDataUniversity[paste("Answered",i)]*cleanDataUniversity$Response)
    }
    
    cleanDataUniversity$calculatedSample <- rowSums(cleanDataUniversity[,8:12])
    cleanDataUniversity$positiveSample <- rowSums(cleanDataUniversity[,11:12])
    cleanDataUniversity$errorTotalSample <- cleanDataUniversity$Response - cleanDataUniversity$calculatedSample
    cleanDataUniversity <- cleanDataUniversity[,-c(2:7)]
    return(cleanDataUniversity)
  }
  
  #combining the three university functions together neatly
  summaryDataUniversityFunction <- function(rawData, qNum, uniName){
    finalData <- selectData(rawData, qNum)
    finalData <- selectDataUniversity(finalData, uniName)
    finalData <- cleanDataUniversity(finalData)
    finalData <- finalData %>% adorn_totals("row")
    return(finalData)
  }
  
  summaryDataUniversityFunction17 <- function(rawData, qNum, uniName){
    finalData <- selectData(rawData, qNum)
    finalData <- selectDataUniversity17(finalData, uniName)
    finalData <- cleanDataUniversity17(finalData)
    finalData <- finalData %>% adorn_totals("row")
    return(finalData)
  }  
  
  #generating summary tables and folders
  summaryTableSubjectGeneration <- function(){
    summaryTable <- data.frame(University = as.character(),
                               PositiveSample = numeric(),
                               SampleSize = numeric(),
                               LowerConfidenceInterval = numeric(),
                               Value = numeric(),
                               UpperConfidenceInterval = numeric(),
                               stringsAsFactors = FALSE)
    return(summaryTable)
  }
  summaryTableUniGeneration <- function(){
    summaryTable <- data.frame(Subject = as.character(),
                               PositiveSample = numeric(),
                               SampleSize = numeric(),
                               LowerConfidenceInterval = numeric(),
                               Value = numeric(),
                               UpperConfidenceInterval = numeric(),
                               stringsAsFactors = FALSE)
    return(summaryTable)
  }
  folderPathGeneration <- function(name){
    folderPath = file.path(getwd(), paste(name))
    dir.create(folderPath)
    return(folderPath)
  }
}

rawData19 <- as.data.frame(read_excel("NSSRawData2019.xlsx", sheet = "NSS3", skip = 3))
rawData18 <- as.data.frame(read_excel("NSSRawData2018.xlsx", sheet = "NSS3", skip = 3))
rawData17 <- as.data.frame(read_excel("NSSRawData2017.xlsx", sheet = "NSS3", skip = 3))

questionNumber <- "Q27" #overarching satisfaction question

#data structure definition
{
  universityIDSurrey <- "Surrey"
  universityIDRH <- "Royal Holloway"
  universityIDSouthampton <- "Southampton"
  universityIDOpen <- "Open"
  universityIDBirm <- "Birmingham"
  
  subjectIDPhysics <- "Physics"
  subjectIDMech <- "Mechanical"
  subjectIDMaths <- "Math"

  physicsDataSatisf19 <- summaryDataSubjectFunction(rawData19, questionNumber, subjectIDPhysics)
  physicsDataSatisf18 <- summaryDataSubjectFunction(rawData18, questionNumber, subjectIDPhysics)
  physicsDataSatisf17 <- summaryDataSubjectFunction17(rawData17, questionNumber, subjectIDPhysics)
  
  mathsDataSatisf19 <- summaryDataSubjectFunction(rawData19, questionNumber, subjectIDMaths)
  mathsDataSatisf18 <- summaryDataSubjectFunction(rawData18, questionNumber, subjectIDMaths)
  mathsDataSatisf17 <- summaryDataSubjectFunction17(rawData17, questionNumber, subjectIDMaths)
  
  mechDataSatisf19 <- summaryDataSubjectFunction(rawData19, questionNumber, subjectIDMech)
  mechDataSatisf18 <- summaryDataSubjectFunction(rawData18, questionNumber, subjectIDMech)
  mechDataSatisf17 <- summaryDataSubjectFunction17(rawData17, questionNumber, subjectIDMech)
  
  surreyDataSatisf19 <- summaryDataUniversityFunction(rawData19, questionNumber, universityIDSurrey)
  surreyDataSatisf18 <- summaryDataUniversityFunction(rawData18, questionNumber, universityIDSurrey)
  surreyDataSatisf17 <- summaryDataUniversityFunction17(rawData17, questionNumber, universityIDSurrey)
  
  openDataSatisf19 <- summaryDataUniversityFunction(rawData19, questionNumber, universityIDOpen)
  openDataSatisf18 <- summaryDataUniversityFunction(rawData18, questionNumber, universityIDOpen)
  openDataSatisf17 <- summaryDataUniversityFunction17(rawData17, questionNumber, universityIDOpen)
  
  rhDataSatisf19 <- summaryDataUniversityFunction(rawData19, questionNumber, universityIDRH)
  rhDataSatisf18 <- summaryDataUniversityFunction(rawData18, questionNumber, universityIDRH)
  rhDataSatisf17 <- summaryDataUniversityFunction17(rawData17, questionNumber, universityIDRH)
  
  southamptonDataSatisf19 <- summaryDataUniversityFunction(rawData19, questionNumber, universityIDSouthampton)
  southamptonDataSatisf18 <- summaryDataUniversityFunction(rawData18, questionNumber, universityIDSouthampton)
  southamptonDataSatisf17 <- summaryDataUniversityFunction17(rawData17, questionNumber, universityIDSouthampton)
  
  uniSatisfList2019 <- list("Surrey Summary Table" = surreyDataSatisf19, "Royal Holloway Summary Table" = rhDataSatisf19, "Southampton Summary Table" = southamptonDataSatisf19, "Open Summary Table" = openDataSatisf19)
  uniSatisfList2018 <- list("Surrey" = surreyDataSatisf18, "Royal Holloway" = rhDataSatisf18, "Southampton" = southamptonDataSatisf18, "Open" = openDataSatisf18)
  uniSatisfList2017 <- list("Surrey" = surreyDataSatisf17, "Royal Holloway" = rhDataSatisf17, "Southampton" = southamptonDataSatisf17, "Open" = openDataSatisf17)
  
  subjectSatisfList2019 <- list("Physics Summary Table" = physicsDataSatisf19, "Maths Summary Table" = mathsDataSatisf19, "Mech Eng Summary Table" = mechDataSatisf19)
  subjectSatisfList2018 <- list("Physics Summary Table" = physicsDataSatisf18, "Maths Summary Table" = mathsDataSatisf18, "Mech Eng Summary Table" = mechDataSatisf18)
  subjectSatisfList2017 <- list("Physics Summary Table" = physicsDataSatisf17, "Maths Summary Table" = mathsDataSatisf17, "Mech Eng Summary Table" = mechDataSatisf17)
}

#####################################################
## Bayesian Statistics Section - Comparison amongst UK and including total summary tables
#####################################################

#FUNCTIONS#
{
  #setting up binomial distributions
  distributionBinomialPerXCompare <- function(data, counter){
    stat <- dbinom(data$positiveSample[counter],
                   size = data$calculatedSample[counter], prob = x)
    return(stat)
  }
  distributionBinomialNormaliseCompare <- function(data){
    data[,2] <- data[,2] / sintegral(data[,1], data[,2])$int
  }

  #plotting functions
  plotDataPDFCompare <- function(data){
    plot <- ggplot(data, aes(x=x, y=y)) +
      geom_line(size=1.1) +
      xlab("P(Satisified)") +
      ylab("Density") +
      scale_x_continuous(limits = c(0,1), expand = c(0,0.01)) +
      theme(axis.title = element_text(size = 24), axis.text = element_text(size = 20))
    return(plot)
  }
  plotDataCDFCompare <- function(data){
    plot <- ggplot(data, aes(x = x, y = data[,3])) +
      geom_line(size=1.1) +
      xlab("P(Satisfied)") +
      ylab("Cumulative Probability") +
      scale_x_continuous(limits = c(0,1), expand = c(0,0.01)) +
      theme(axis.title = element_text(size = 24), axis.text = element_text(size = 20))
    return(plot)
  }
  plotDataStatCompare <- function(plotData2){
    plotDataStatCompare <- layer_data(plotData2, 1)
    CILow <- plotDataStatCompare[which.min(abs(0.025-plotDataStatCompare$y)),1] #trying to find closest value to lower interval
    Med <- plotDataStatCompare[which.min(abs(0.5-plotDataStatCompare$y)),1] #trying to find closest value to median through CDF
    CIUpp <- plotDataStatCompare[which.min(abs(0.975-plotDataStatCompare$y)),1] #trying to find closest value to upper interval
    statList <- list("Lower CI" = CILow, "Median" = Med, "Upper CI" = CIUpp)
    return (statList)
  }
  plotDataGenerationCompare <- function(data, counter){
    graphData <- data.frame(x = x, y = distributionBinomialPerXCompare(data, counter))
    graphData[,2] <- distributionBinomialNormaliseCompare(graphData)
    graphData$yInt <- sintegral(graphData[,1], graphData[,2], n.pts = 200)$y
    return(graphData)
  }
  plotGraphCompare <- function(plot1, plot2, stat, data, folderName, counter){
    plot2 <- plot2 + geom_segment(aes(x=0, y=0.975, xend=stat$`Upper CI`, yend=0.975), linetype="dashed", color="blue", size=1.2) +
      geom_segment(aes(x=0, y=0.025, xend=stat$`Lower CI`, yend=0.025), linetype="dashed", color="blue", size=1.2) +
      geom_segment(aes(x=0, y=0.5, xend=stat$Median, yend=0.5), linetype="dashed", color="red", size=1.2)
    ggsave(file.path(folderName,
                     paste0(gsub(" ","", data$Provider[counter], fixed=TRUE),"PDF.png")), plot1)
    ggsave(file.path(folderName,
                     paste0(gsub(" ","", data$Provider[counter], fixed=TRUE),"CDF.png")), plot2)
  }
  summaryTableCompareDataFillSubject <- function(data, summary){
    sampleData <- data.frame()
    for (i in 1:nrow(data)){
      counter <- i
      graphData <- plotDataGenerationCompare(data, counter)
      sampleData[1,i] <- data$Provider[i]
      for (j in 1:1000){
        sampleData[j+1,i] <- sample(graphData[,1], 1, replace = TRUE, prob = graphData[,2])
      }
      plot1a <- plotDataPDFCompare(graphData)
      plot1b <- plotDataCDFCompare(graphData)
      stat <- plotDataStatCompare(plot1b)
      summary[i,] <- list(data$Provider[i], data$positiveSample[i],
                          data$calculatedSample[i], stat$`Lower CI`, stat$Median, stat$`Upper CI`)
    }
    sampleData <- data.frame(t(sampleData))
    sampleData <- sampleData[-nrow(sampleData),]
    listReturn <- list("sampleTable" = sampleData, "summaryTable" = summary)
    return(listReturn)
  }
  summaryTableCompareDataFillSubjectwPlot <- function(data, summary, folderName){
    sampleData <- data.frame()
    for (i in 1:nrow(data)){
      counter <- i
      graphData <- plotDataGenerationCompare(data, counter)
      sampleData[1,i] <- data$Provider[i]
      for (j in 1:1000){
        sampleData[j+1,i] <- sample(graphData[,1], 1, replace = TRUE, prob = graphData[,2])
      }
      plot1a <- plotDataPDFCompare(graphData)
      plot1b <- plotDataCDFCompare(graphData)
      stat <- plotDataStatCompare(plot1b)
      plotGraphCompare(plot1a, plot1b, stat, data, folderName, counter)
      summary[i,] <- list(data$Provider[i], data$positiveSample[i],
                                          data$calculatedSample[i], stat$`Lower CI`, stat$Median, stat$`Upper CI`)
    }
    sampleData <- data.frame(t(sampleData))
    sampleData <- sampleData[-nrow(sampleData),]
    listReturn <- list("sampleTable" = sampleData, "summaryTable" = summary)
    return(listReturn)
  }
  summaryTableCompareDataFillUni <- function(data, summary){
    for (i in 1:nrow(data)){
      counter <- i
      graphData <- plotDataGenerationCompare(data, counter)
      plot1a <- plotDataPDFCompare(graphData)
      plot1b <- plotDataCDFCompare(graphData)
      stat <- plotDataStatCompare(plot1b)
      summary[i,] <- list(data$Subject[i], data$positiveSample[i],
                          data$calculatedSample[i], stat$`Lower CI`, stat$Median, stat$`Upper CI`)
    }
    return(summary)
  }
}

folderGraphCompare <- folderPathGeneration("Comparison amongst UK")
folderPhysics2019 <- folderPathGeneration("Physics 2019 PDFs and CDFs")

#defining summary table structures
{
physicsSummaryTable2019 <- summaryTableSubjectGeneration()
physicsSummaryTable2018 <- summaryTableSubjectGeneration()
physicsSummaryTable2017 <- summaryTableSubjectGeneration()

mathsSummaryTable2019 <- summaryTableSubjectGeneration()
mathsSummaryTable2018 <- summaryTableSubjectGeneration()
mathsSummaryTable2017 <- summaryTableSubjectGeneration()

mechSummaryTable2019 <- summaryTableSubjectGeneration()
mechSummaryTable2018 <- summaryTableSubjectGeneration()
mechSummaryTable2017 <- summaryTableSubjectGeneration()

surreySummaryTable2019 <- summaryTableUniGeneration()
surreySummaryTable2018 <- summaryTableUniGeneration()
surreySummaryTable2017 <- summaryTableUniGeneration()

rhSummaryTable2019 <- summaryTableUniGeneration()
rhSummaryTable2018 <- summaryTableUniGeneration()
rhSummaryTable2017 <- summaryTableUniGeneration()

southamptonSummaryTable2019 <- summaryTableUniGeneration()
southamptonSummaryTable2018 <- summaryTableUniGeneration()
southamptonSummaryTable2017 <- summaryTableUniGeneration()

openSummaryTable2019 <- summaryTableUniGeneration()
openSummaryTable2018 <- summaryTableUniGeneration()
openSummaryTable2017 <- summaryTableUniGeneration()
}

x <- seq(0, 1, length.out = 200) #dividing 200 steps between 0 and 1

#generating summary tables as individual comparisons
{
physicsSummaryTable2019 <- summaryTableCompareDataFillSubjectwPlot(physicsDataSatisf19, physicsSummaryTable2019, folderPhysics2019)$summaryTable
physicsSampleTable2019 <- summaryTableCompareDataFillSubject(physicsDataSatisf19, physicsSummaryTable2019)$sampleTable
physicsSummaryTable2018 <- summaryTableCompareDataFillSubject(physicsDataSatisf18, physicsSummaryTable2018)$summaryTable
physicsSampleTable2018 <- summaryTableCompareDataFillSubject(physicsDataSatisf18, physicsSummaryTable2018)$sampleTable
physicsSummaryTable2017 <- summaryTableCompareDataFillSubject(physicsDataSatisf17, physicsSummaryTable2017)$summaryTable
physicsSampleTable2017 <- summaryTableCompareDataFillSubject(physicsDataSatisf17, physicsSummaryTable2017)$sampleTable

mathsSummaryTable2019 <- summaryTableCompareDataFillSubject(mathsDataSatisf19, mathsSummaryTable2019)$summaryTable
mathsSampleTable2019 <- summaryTableCompareDataFillSubject(mathsDataSatisf19, mathsSummaryTable2019)$sampleTable
mathsSummaryTable2018 <- summaryTableCompareDataFillSubject(mathsDataSatisf18, mathsSummaryTable2018)$summaryTable
mathsSampleTable2018 <- summaryTableCompareDataFillSubject(mathsDataSatisf18, mathsSummaryTable2018)$sampleTable
mathsSummaryTable2017 <- summaryTableCompareDataFillSubject(mathsDataSatisf17, mathsSummaryTable2017)$summaryTable
mathsSampleTable2017 <- summaryTableCompareDataFillSubject(mathsDataSatisf17, mathsSummaryTable2017)$sampleTable

mechSummaryTable2019 <- summaryTableCompareDataFillSubject(mechDataSatisf19, mechSummaryTable2019)$summaryTable
mechSampleTable2019 <- summaryTableCompareDataFillSubject(mechDataSatisf19, mechSummaryTable2019)$sampleTable
mechSummaryTable2018 <- summaryTableCompareDataFillSubject(mechDataSatisf18, mechSummaryTable2018)$summaryTable
mechSampleTable2018 <- summaryTableCompareDataFillSubject(mechDataSatisf18, mechSummaryTable2018)$sampleTable
mechSummaryTable2017 <- summaryTableCompareDataFillSubject(mechDataSatisf17, mechSummaryTable2017)$summaryTable
mechSampleTable2017 <- summaryTableCompareDataFillSubject(mechDataSatisf17, mechSummaryTable2017)$sampleTable

surreySummaryTable2019 <- summaryTableCompareDataFillUni(surreyDataSatisf19, surreySummaryTable2019)
surreySummaryTable2018 <- summaryTableCompareDataFillUni(surreyDataSatisf18, surreySummaryTable2018)
surreySummaryTable2017 <- summaryTableCompareDataFillUni(surreyDataSatisf17, surreySummaryTable2017)

rhSummaryTable2019 <- summaryTableCompareDataFillUni(rhDataSatisf19, rhSummaryTable2019)
rhSummaryTable2018 <- summaryTableCompareDataFillUni(rhDataSatisf18, rhSummaryTable2018)
rhSummaryTable2017 <- summaryTableCompareDataFillUni(rhDataSatisf17, rhSummaryTable2017)

southamptonSummaryTable2019 <- summaryTableCompareDataFillUni(southamptonDataSatisf19, southamptonSummaryTable2019)
southamptonSummaryTable2018 <- summaryTableCompareDataFillUni(southamptonDataSatisf18, southamptonSummaryTable2018)
southamptonSummaryTable2017 <- summaryTableCompareDataFillUni(southamptonDataSatisf17, southamptonSummaryTable2017)

openSummaryTable2019 <- summaryTableCompareDataFillUni(openDataSatisf19, openSummaryTable2019)
openSummaryTable2018 <- summaryTableCompareDataFillUni(openDataSatisf18, openSummaryTable2018)
openSummaryTable2017 <- summaryTableCompareDataFillUni(openDataSatisf17, openSummaryTable2017)
}

#generating aggregate summary tables
{
uniAggregateSummaryTable2019 <- summaryTableSubjectGeneration()
uniAggregateSummaryTable2018 <- summaryTableSubjectGeneration()
uniAggregateSummaryTable2017 <- summaryTableSubjectGeneration()
  
departmentAggregateSummaryTable2019 <- summaryTableUniGeneration()
departmentAggregateSummaryTable2018 <- summaryTableUniGeneration()
departmentAggregateSummaryTable2017 <- summaryTableUniGeneration()
  
uniSummaryList2019 <- list("Surrey Summary Table" = surreySummaryTable2019, "RoyalHolloway Summary Table" = rhSummaryTable2019, "Southampton Summary Table" = southamptonSummaryTable2019, "Open Summary Table" = openSummaryTable2019)
uniSummaryList2018 <- list("Surrey Summary Table" = surreySummaryTable2018, "RoyalHolloway Summary Table" = rhSummaryTable2018, "Southampton Summary Table" = southamptonSummaryTable2018, "Open Summary Table" = openSummaryTable2018)
uniSummaryList2017 <- list("Surrey Summary Table" = surreySummaryTable2017, "RoyalHolloway Summary Table" = rhSummaryTable2017, "Southampton Summary Table" = southamptonSummaryTable2017, "Open Summary Table" = openSummaryTable2017)
  
subjectSummaryList2019 <- list("Physics Summary Table" = physicsSummaryTable2019, "Maths Summary Table" = mathsSummaryTable2019, "MechanicalEngineering Summary Table" = mechSummaryTable2019)
subjectSummaryList2018 <- list("Physics Summary Table" = physicsSummaryTable2018, "Maths Summary Table" = mathsSummaryTable2018, "MechanicalEngineering Summary Table" = mechSummaryTable2018)
subjectSummaryList2017 <- list("Physics Summary Table" = physicsSummaryTable2017, "Maths Summary Table" = mathsSummaryTable2017, "MechanicalEngineering Summary Table" = mechSummaryTable2017)
}

#outputting files to folder
{
for (i in 1:length(uniSummaryList2019)){
  write.csv(uniSummaryList2019[[i]], paste0(folderGraphCompare, 
                                         "/", names(uniSummaryList2019[i]),
                                         " 2019", ".csv"), row.names = FALSE)
  }
for (i in 1:length(uniSummaryList2018)){
  write.csv(uniSummaryList2018[[i]], paste0(folderGraphCompare, 
                                            "/", names(uniSummaryList2018[i]),
                                            " 2018", ".csv"), row.names = FALSE)
}
for (i in 1:length(uniSummaryList2017)){
  write.csv(uniSummaryList2017[[i]], paste0(folderGraphCompare, 
                                            "/", names(uniSummaryList2017[i]),
                                            " 2017", ".csv"), row.names = FALSE)
}
for (i in 1:length(subjectSummaryList2019)){
  write.csv(subjectSummaryList2019[[i]], paste0(folderGraphCompare, 
                                                "/", names(subjectSummaryList2019[i]),
                                                " 2019", ".csv"), row.names = FALSE)
}
for (i in 1:length(subjectSummaryList2018)){
    write.csv(subjectSummaryList2018[[i]], paste0(folderGraphCompare, 
                                                  "/", names(subjectSummaryList2018[i]),
                                                  " 2018", ".csv"), row.names = FALSE)
  }
for (i in 1:length(subjectSummaryList2017)){
    write.csv(subjectSummaryList2017[[i]], paste0(folderGraphCompare, 
                                                  "/", names(subjectSummaryList2017[i]),
                                                  " 2017", ".csv"), row.names = FALSE)
  }
}
  
aggregateScore <- function(list, summary){
  for (i in 1:length(list)){
    table <- list[[i]]
    summary <- rbind(summary, table[nrow(table),])
    summary[i,1] <- names(list)[i]
    summary[i,1] <- word(summary[i,1], 1)
  }
  return(summary)
}

#creating aggregate summary tables
{
uniAggregateSummaryTable2019 <- aggregateScore(uniSummaryList2019, uniAggregateSummaryTable2019)
uniAggregateSummaryTable2018 <- aggregateScore(uniSummaryList2018, uniAggregateSummaryTable2018)
uniAggregateSummaryTable2017 <- aggregateScore(uniSummaryList2017, uniAggregateSummaryTable2017)

departmentAggregateSummaryTable2019 <- aggregateScore(subjectSummaryList2019, departmentAggregateSummaryTable2019)
departmentAggregateSummaryTable2018 <- aggregateScore(subjectSummaryList2018, departmentAggregateSummaryTable2018)
departmentAggregateSummaryTable2017 <- aggregateScore(subjectSummaryList2017, departmentAggregateSummaryTable2017)
}

#outputting files to folder
{
write.csv(uniAggregateSummaryTable2019, "uniAggregateSummaryTable2019.csv", row.names = FALSE)
write.csv(uniAggregateSummaryTable2018, "uniAggregateSummaryTable2018.csv", row.names = FALSE)
write.csv(uniAggregateSummaryTable2017, "uniAggregateSummaryTable2017.csv", row.names = FALSE)

write.csv(departmentAggregateSummaryTable2019, "departmentAggregateSummaryTable2019.csv", row.names = FALSE)
write.csv(departmentAggregateSummaryTable2018, "departmentAggregateSummaryTable2018.csv", row.names = FALSE)
write.csv(departmentAggregateSummaryTable2017, "departmentAggregateSummaryTable2017.csv", row.names = FALSE)
}
  
#####################################################
## Sampling and Generating University League Tables
#####################################################

samplingStat <- function(sampleTable, uniName){
  testList <- NULL
  medianValueList <- NULL
  confidenceIntervalLowerList <- NULL
  confidenceIntervalUpperList <- NULL
  
  for (i in 1:nrow(sampleTable)-1){
    testLeague <- data.frame("Universities" = sampleTable[,1], "Score" = sampleTable[,i+1])
    testLeague <- testLeague[order(-rank(testLeague$Score)),]
    row.names(testLeague) <- NULL
    testList <- c(testList, which(grepl(uniName, testLeague$Universities, fixed=TRUE)))
  }
  testList <- sort(testList)
  medianValueList <- median(testList)
  #97.5% confidence interval
  confidenceIntervalLowerList <- testList[round(length(testList)*0.025)]
  confidenceIntervalUpperList <- testList[round(length(testList)*0.975)]
  png(paste0("Histogram", uniName, ".png"))
  hist(testList, xlab = "Table Positions",
       col = "powderblue", main=NULL)
  dev.off()
  statList <- list("Lower CI" = confidenceIntervalLowerList, "Median" = medianValueList, "Upper CI" = confidenceIntervalUpperList)
}

surreyPhysicsSample2019 <- samplingStat(physicsSampleTable2019, universityIDSurrey)
birmPhysicsSample2019 <- samplingStat(physicsSampleTable2019, universityIDBirm)

#####################################################
{
testTable <- physicsSummaryTable2019[1:nrow(physicsSummaryTable2019)-1,c(1,4:6)]
testTable[,2:4] <- round(testTable[,2:4], digits=2)
testTable <- testTable[order(-rank(testTable$Value)),]
row.names(testTable) <- NULL
testTable$Error <- NA
for(i in 1:nrow(testTable)){
  testTable$Error[i] <- paste0("[", sprintf("%.2f",testTable[i,2]), ",", sprintf("%.2f",testTable[i,4]), "]")
}
testTable <- testTable[,c(1,3,5)]
breaks <- quantile(testTable[,2], probs = seq(.05, .95, .05), na.rm = TRUE)
colors <- round(seq(255, 40, length.out = length(breaks) + 1), 0) %>%
{paste0("rgb(", ., ",240,", ., ")")}
datatable(testTable) %>%
  formatStyle(names(testTable), backgroundColor = styleInterval(breaks,colors))
}
