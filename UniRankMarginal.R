#####################################################
## University Ranking Marginal - Jay Au [URN:6361715]
#####################################################

#please reset RStudio by using Ctrl+Shift+F10 & The following command to clean the environment:
rm(list = ls())

#####################################################
## Initializing Libraries and Folder Paths
#####################################################

library(Bolstad2)
library(ggplot2)
library(latex2exp)

setwd("D:\\Libraries\\Documents\\University Work\\Dissertation v2")

#####################################################
## Marginal Probabilities
#####################################################

#define function of combined probabilities (i.e. A x B)
#define changing variable function: P_a(F_a, Delta_F) = P_a(F_a) * P_b(F_a + Delta_F)
#integrate wrt F_a (from 0 to 1), careful of bounds: { F_a < 0, F_a > 1 } = 0

#setting jet color palette (often found in MatPlotLib & MatLab)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
#FUNCTIONS#
{
  folderPathGeneration <- function(name){
    folderPath = file.path(getwd(), paste(name))
    dir.create(folderPath)
    return(folderPath)
  }
  
  probFunction <- function(dataA, dataB, fractionA, fractionB){
    probA <- dbinom(dataA$PositiveSample, 
                    size = dataA$SampleSize, prob = fractionA)
    probB <- dbinom(dataB$PositiveSample, 
                    size = dataB$SampleSize, prob = fractionB)
    probFunction <- probA * probB
    return(probFunction)
  }
  
  probDelta <- function(dataA, dataB, fractionA, delta){
    probDelta <- vector(mode = "list", length = length(fractionA))
    for (i in 1:length(fractionA)){
      probA <- dbinom(dataA$PositiveSample, 
                      size = dataA$SampleSize, prob = fractionA[i])
      fractionB <- fractionA[i] + delta
      if (fractionB <= 1 && fractionB >= 0){
        probB <- dbinom(dataB$PositiveSample, 
                        size = dataB$SampleSize, prob = fractionB)
      } else {
        probB <- 0
      }
      probDelta[i] <- probA * probB
    }
    return(probDelta)
  }
  
  contourDataFunction <- function(dataA, dataB, fractionA, fractionB, delta){
    prob2Ddata <- matrix(NA, nrow = 200, ncol = 200)
    for (i in 1:200){
      for (j in 1:200){
        if (fractionB[i]+delta[j] <= 1 && fractionB[i]+delta[j] >= 0){
          prob2Ddata[i,j] <- probFunction(dataA, dataB, fractionA[i], fractionB[i]+delta[j])
        } else {
          prob2Ddata[i,j] <- 0
        }
      }
    }
    png(paste0(dataA[1,1],"vs",dataB[1,1], "contour.png"))
    par(mar=c(5,6,4,4)+.1)
    filled.contour(x = fractionA, y = delta, z = prob2Ddata, 
                   xlab = TeX('$F_{A}$'), ylab = TeX('$\\Delta F$'), color = jet.colors, cex.lab=1.7,
                   xlim = c(0.8,0.9), ylim = c(-0.1,0.1))
    dev.off()
    return(prob2Ddata)
  }
  
  generatePDFData <- function(dataA, dataB, fractionA, fractionB, delta, folderName){
    probMargin <- data.frame(Delta = as.numeric(), probMargin = as.numeric())
    
    for (i in 1:length(delta)){
      intProbMargin <- sintegral(fractionA, probDelta(dataA, dataB, fractionA, delta[i]))
      probMargin[i,] <- list(delta[i], intProbMargin$int)
    }
    
    #normalise data
    probMargin[,2] <- probMargin[,2] / sintegral(probMargin[,1], probMargin[,2])$int
    probMargin$yInt <- sintegral(probMargin[,1], probMargin[,2], n.pts = 200)$y
    plot <- ggplot(probMargin, aes(x=Delta, y=probMargin)) +
      geom_line(size=1.0) + xlab(TeX('$\\Delta F')) + ylab(TeX('$P_{AB}(\\Delta F)')) +
      xlim(-0.1, 0.1) +
      #labs(title = paste0("Observing difference in satisfaction from the perspective of ", dataA[1,1] ," student")) +
           #subtitle = paste0("Target subject = ", dataB[1,1])) +
      theme_bw() +
      theme(axis.title = element_text(size = 24), axis.text = element_text(size = 22),
            plot.title = element_text(size = 17)) 
    ggsave(file.path(folderName, 
                     paste0(dataA[1,1], "vs", dataB[1,1],".png")), plot, 
           dpi = 600, width = 9, height = 9, units = "in")
    return(probMargin)
  }
  
  #find most probable value and confidence intervals
  CDFDataAnalysis <- function(probData){
    cdfPlot <- ggplot(probData, aes(x = probData[,1], y = probData[,3])) +
      geom_line(size=1.1) +
      xlab("P(Satisfied)") +
      ylab("Cumulative Probability") +
      scale_x_continuous(limits = c(-1,1), expand = c(0,0.01))
    plotDataStatCompare <- layer_data(cdfPlot, 1)
    CILow <- plotDataStatCompare[which.min(abs(0.025-plotDataStatCompare$y)),1] #trying to find closest value to lower interval
    Med <- plotDataStatCompare[which.min(abs(0.5-plotDataStatCompare$y)),1] #trying to find closest value to median through CDF
    CIUpp <- plotDataStatCompare[which.min(abs(0.975-plotDataStatCompare$y)),1] #trying to find closest value to upper interval
    statList <- list("Lower CI" = CILow, "Median" = Med, "Upper CI" = CIUpp)
    return (statList)
  }

  statFunctionSummary <- function(dataA, dataB, fractionA, fractionB, delta, folderName){
    contourDataFunction(dataA, dataB, fractionA, fractionB, delta)
    pdfData <- generatePDFData(dataA, dataB, fractionA, fractionB, delta, folderName)
    statList <- CDFDataAnalysis(pdfData)
    return(statList)
  }
  
}

deltaValues <- seq(-1, 1, length.out = 200)
fractionA <- seq(0, 1, length.out = 200)
fractionB <- seq(0, 1, length.out = 200)

folderName <- folderPathGeneration("Marginal Probability Graphs")
departmentData19 <- read.csv("departmentAggregateSummaryTable2019.csv")
uniData19 <- read.csv("uniAggregateSummaryTable2019.csv")

comparisonA <- departmentData19[1,]
comparisonB <- departmentData19[2,]
comparisonC <- departmentData19[3,]

comparisonAUni <- uniData19[1,]
comparisonBUni <- uniData19[2,]
comparisonCUni <- uniData19[3,]
comparisonDUni <- uniData19[4,]

physicsVsMathsStats19 <- statFunctionSummary(comparisonA, comparisonB, fractionA, fractionB, deltaValues, folderName)
physicsVsMechEngStats19 <- statFunctionSummary(comparisonA, comparisonC, fractionA, fractionB, deltaValues, folderName)
mathsVsMechEngStats19 <- statFunctionSummary(comparisonB, comparisonC, fractionA, fractionB, deltaValues, folderName)

surreyVsRHStats19 <- statFunctionSummary(comparisonAUni, comparisonBUni, fractionA, fractionB, deltaValues, folderName)
surreyVsSouthamptonStats19 <- statFunctionSummary(comparisonAUni, comparisonCUni, fractionA, fractionB, deltaValues, folderName)
surreyVsOpenStats19 <- statFunctionSummary(comparisonAUni, comparisonDUni, fractionA, fractionB, deltaValues, folderName)