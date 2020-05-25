#####################################################
## University Ranking Comparing Years - Jay Au [URN:6361715]
#####################################################

#please reset RStudio by using Ctrl+Shift+F10 & the following command to clean the environment:
rm(list = ls())

#####################################################
## Initializing Libraries and Folder Paths
#####################################################

library(ggplot2)

setwd("D:\\Libraries\\Documents\\University Work\\Dissertation v2\\Comparison amongst UK")

#####################################################
## Importing Data
#####################################################

physicsSummaryTable2019 <- read.csv(text=paste0(head(readLines("Physics Summary Table 2019.csv"),-1)), colClasses = c("University" = "character"))
physicsSummaryTable2018 <- read.csv(text=paste0(head(readLines("Physics Summary Table 2018.csv"),-1)), colClasses = c("University" = "character"))
physicsSummaryTable2017 <- read.csv(text=paste0(head(readLines("Physics Summary Table 2017.csv"),-1)), colClasses = c("University" = "character"))

mathsSummaryTable2019 <- read.csv(text=paste0(head(readLines("Maths Summary Table 2019.csv"),-1)), colClasses = c("University" = "character"))
mathsSummaryTable2018 <- read.csv(text=paste0(head(readLines("Maths Summary Table 2018.csv"),-1)), colClasses = c("University" = "character"))
mathsSummaryTable2017 <- read.csv(text=paste0(head(readLines("Maths Summary Table 2017.csv"),-1)), colClasses = c("University" = "character"))

mechengSummaryTable2019 <- read.csv(text=paste0(head(readLines("MechanicalEngineering Summary Table 2019.csv"),-1)), colClasses = c("University" = "character"))
mechengSummaryTable2018 <- read.csv(text=paste0(head(readLines("MechanicalEngineering Summary Table 2018.csv"),-1)), colClasses = c("University" = "character"))
mechengSummaryTable2017 <- read.csv(text=paste0(head(readLines("MechanicalEngineering Summary Table 2017.csv"),-1)), colClasses = c("University" = "character"))


tableSubjectYearDifference <- function(tableNew, tableOld){
  comparisonTable <- data.frame(University = as.character(),
                                Difference = numeric(),
                                SampleSize = numeric(),
                                stringsAsFactors = FALSE)
  
  for (i in (1:nrow(tableNew))){
    #seeing in university score existed in prior year
    if (tableNew$University[i] %in% tableOld$University){
      j <- which(tableNew$University[i] == tableOld$University)
      #seeing if university score is within confidence levels (i.e. deviation due to statistical noise only)
      if (tableNew$Value[i] >= tableOld$LowerConfidenceInterval[j] &
          (tableNew$Value[i] <= tableOld$UpperConfidenceInterval[j])){
        comparisonTable[i,] <- list(tableNew$University[i] , tableNew$Value[i] - tableOld$Value[j], tableNew$SampleSize[i])
      }
    }
  }
  comparisonTable <- comparisonTable[complete.cases(comparisonTable),]
  row.names(comparisonTable) <- NULL
  #fitting abs of difference to y = A * x**B
  comparisonTable[,2] <- abs(comparisonTable[,2])
  return(comparisonTable)
}

physics19v18 <- tableSubjectYearDifference(physicsSummaryTable2019, physicsSummaryTable2018)
physics18v17 <- tableSubjectYearDifference(physicsSummaryTable2018, physicsSummaryTable2017)

maths19v18 <- tableSubjectYearDifference(mathsSummaryTable2019, mathsSummaryTable2018)
maths18v17 <- tableSubjectYearDifference(mathsSummaryTable2018, mathsSummaryTable2017)

mech19v18 <- tableSubjectYearDifference(mechengSummaryTable2019, mechengSummaryTable2018)
mech18v17 <- tableSubjectYearDifference(mechengSummaryTable2018, mechengSummaryTable2017)

########################## OLD MOSAIC SECTION - IGNORE
#fit <- fitModel(Difference~A*SampleSize**B, data=test)
#fit2 <- fitModel(Difference~A*SampleSize**B, data=test2)
#summary(fit)
#summary(fit2)
#plotPoints(Difference~SampleSize, data = test)
#plotPoints(Difference~SampleSize, data = test2, add = TRUE)
#plotFun(fit(SampleSize)~SampleSize, SampleSize.lim=range(0,150), add = TRUE)
#plotFun(fit2(SampleSize)~SampleSize, SampleSize.lim=range(0,150), add = TRUE)
###########################

physicsPlotName <- "PhysicsComparisonYears.png"
physicsGraphName <- "Physics"
mathsPlotName <- "MathsComparisonYears.png"
mathsGraphName <- "Maths"
mechengPlotName <- "MechEngComparisonYears.png"
mechengGraphName <- "Mechanical Engineering"

fitFunction <- function(x, a, b){a*(x^b)}

plottingFunction <- function(data19v18, data18v17, plotName, graphSubject){
  graphData19v18 <- data.frame('x' = data19v18[,3], 'y' = data19v18[,2])
  graphData18v17 <- data.frame('x' = data18v17[,3], 'y' = data18v17[,2])
  colors <- c("2019 vs 2018" = "blue", "2018 vs 2017" = "red")
  
  plot <- ggplot(NULL, aes(x, y)) +
    geom_point(data = graphData19v18, aes(color = "2019 vs 2018")) +
    geom_point(data = graphData18v17, aes(color = "2018 vs 2017")) +
    geom_smooth(method = "nls",
                formula = y ~ a * x ^ b,
                method.args = list(start=c(a = 0.5, b = -0.5)),
                data = graphData19v18,
                col = "blue",
                se = FALSE) +
    geom_smooth(method = "nls",
                formula = y ~ a * x ^ b,
                method.args = list(start=c(a = 0.5, b = -0.5)),
                data = graphData18v17,
                col = "red",
                se = FALSE) +
    labs(#title = "Observing variation in satisfaction with respect to sample size",
         #subtitle = paste0(graphSubject, " students satisfaction data over 3 years"),
         color = "Legend") +
    xlab("Sample Size") +
    ylab("Difference in Satisfaction") +
    theme_bw() +
    theme(legend.position = c(0.95, 0.95), legend.justification = c("right", "top"),
          axis.title = element_text(size = 24), axis.text = element_text(size = 20),
          legend.title = element_text(size = 18), legend.text = element_text(size = 14),
          plot.title = element_text(size = 20)) +

    scale_color_manual(values = colors)
  
  ggsave(plotName, plot, dpi = 600, width = 9, height = 9, units = "in")
  parameter1918 <- nls(y ~ a * x ^ b, data = graphData19v18, start = c(a = 0.5, b = -0.5))
  parameter1817 <- nls(y ~ a * x ^ b, data = graphData18v17, start = c(a = 0.5, b = -0.5))
  
  statList <- list("parameter1918" = summary(parameter1918), "parameter1817" = summary(parameter1817))
  return(statList)
}

physicsParameterStats <- plottingFunction(physics19v18, physics18v17, physicsPlotName, physicsGraphName)
mathsParameterStats <- plottingFunction(maths19v18, maths18v17, mathsPlotName, mathsGraphName)
mechengParameterStats <- plottingFunction(mech19v18, mech18v17, mechengPlotName, mechengGraphName)
