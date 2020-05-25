#####################################################
## University Ranking Data Analysis - Jay Au [URN:6361715]
#####################################################

#please reset RStudio by using Ctrl+Shift+F10 & The following command to clean the environment:
rm(list = ls())

#####################################################
## Initializing Libraries and Folder Paths
#####################################################

setwd("D:\\Libraries\\Documents\\University Work\\Dissertation v2")

#####################################################
## Overlap Statistic
#####################################################

overlapCount <- function(summaryTable){
  overlaps <- 0
  for (i in 1:nrow(summaryTable)){
    for (j in 1:nrow(summaryTable)){
      if (i != j &
          summaryTable$UpperConfidenceInterval[i] >= summaryTable$LowerConfidenceInterval[j] & 
          summaryTable$LowerConfidenceInterval[i] <= summaryTable$UpperConfidenceInterval[j]){
        overlaps = overlaps + 1
      }
    }
  }
  overlaps <- overlaps/((nrow(summaryTable)**2)-nrow(summaryTable))
  return(overlaps)
}

folderName <- "Comparison amongst UK"
fileNames <- list.files(folderName)
names <- substr(fileNames, 1, nchar(fileNames)-4)

for (i in 1:length(names)){
  tempName <- gsub(' ', '',paste(names[i]))
  filePath <- paste(folderName, fileNames[i], sep="/")
  assign(tempName, read.csv(filePath))
}

physicsOverlap2019 <- overlapCount(PhysicsSummaryTable2019)
physicsOverlap2018 <- overlapCount(PhysicsSummaryTable2018)
physicsOverlap2017 <- overlapCount(PhysicsSummaryTable2017)
physicsOverlapList <- c("Physics", physicsOverlap2019, physicsOverlap2018, physicsOverlap2017)

mathsOverlap2019 <- overlapCount(MathsSummaryTable2019)
mathsOverlap2018 <- overlapCount(MathsSummaryTable2018)
mathsOverlap2017 <- overlapCount(MathsSummaryTable2017)
mathsOverlapList <- c("Maths", mathsOverlap2019, mathsOverlap2018, mathsOverlap2017)

mechOverlap2019 <- overlapCount(MechEngSummaryTable2019)
mechOverlap2018 <- overlapCount(MechEngSummaryTable2018)
mechOverlap2017 <- overlapCount(MechEngSummaryTable2017)
mechOverlapList <- c("Mechanical Engineering", mechOverlap2019, mechOverlap2018, mechOverlap2017)

surreyOverlap2019 <- overlapCount(SurreySummaryTable2019)
surreyOverlap2018 <- overlapCount(SurreySummaryTable2018)
surreyOverlap2017 <- overlapCount(SurreySummaryTable2017)
surreyOverlapList <- c("Surrey", surreyOverlap2019, surreyOverlap2018, surreyOverlap2017)

rhOverlap2019 <- overlapCount(RoyalHollowaySummaryTable2019)
rhOverlap2018 <- overlapCount(RoyalHollowaySummaryTable2018)
rhOverlap2017 <- overlapCount(RoyalHollowaySummaryTable2017)
rhOverlapList <- c("Royal Holloway", rhOverlap2019, rhOverlap2018, rhOverlap2017)

southamptonOverlap2019 <- overlapCount(SouthamptonSummaryTable2019)
southamptonOverlap2018 <- overlapCount(SouthamptonSummaryTable2018)
southamptonOverlap2017 <- overlapCount(SouthamptonSummaryTable2017)
southamptonOverlapList <- c("Southampton", southamptonOverlap2019, southamptonOverlap2018, southamptonOverlap2017)

openOverlap2019 <- overlapCount(OpenSummaryTable2019)
openOverlap2018 <- overlapCount(OpenSummaryTable2018)
openOverlap2017 <- overlapCount(OpenSummaryTable2017)
openOverlapList <- c("Open University", openOverlap2019, openOverlap2018, openOverlap2017)

overlapSubjectSummary <- data.frame(physicsOverlapList, mathsOverlapList, mechOverlapList)
overlapSubjectSummary <- data.frame(t(overlapSubjectSummary))
colnames(overlapSubjectSummary) <- c("Department", "2019", "2018", "2017")
row.names(overlapSubjectSummary) <- NULL
write.csv(overlapSubjectSummary, "Subject Overlap Table.csv", row.names = FALSE)

overlapUniSummary <- data.frame(surreyOverlapList, rhOverlapList, southamptonOverlapList, openOverlapList)
overlapUniSummary <- data.frame(t(overlapUniSummary))
colnames(overlapUniSummary) <- c("University", "2019", "2018", "2017")
row.names(overlapUniSummary) <- NULL
write.csv(overlapUniSummary, "University Overlap Table.csv", row.names = FALSE)
