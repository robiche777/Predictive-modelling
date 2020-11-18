# This is a code which include data preparation for my PGA predictive model ####

# Installation of packages ####

# Data manipulations
install.packages("dplyr")
install.packages("data.table")
install.packages("Readr")
install.packages("tidyr")
install.packages("stringr")
install.packages("lubricate")
install.packages("rjson")
install.packages("jsonlite")

#data Visualization
install.packages("ggplot2")
install.packages("ggvis")
install.packages("rgl")
install.packages("htmlwidgets")
install.packages("lattice")

# Machine learning
install.packages("mlr")
install.packages("randomForest")
install.packages("caret")
install.packages("e1071")
install.packages("vcd")
install.packages("glmnet")

# Loading of the library ####
# Data manipulations
library("dplyr")
library("data.table")
library("Readr")
library("tidyr")
library("stringr")
library("lubricate")
library("rjson")
library("jsonlite")

#data Visualization
library("ggplot2")
library("ggvis")
library("rgl")
library("htmlwidgets")
library("lattice")

# Machine learning
library("mlr")
library("randomForest")
library("caret")
library("e1071")
library("vcd")
library("glmnet")


# Data importation ####
Master2020.json <- fromJSON(file = "C:/Users/alexr/Documents/R/Projects/Predictive-modelling/PGA tour data/scorecard.json") #load JSON format to R

# Data manipulation ####

ConvertJsonToDF <- function(x) {
  
  #This function allows to reorganize the list format created from the JSON file in a Data frame format
  
  # correction needed, find a way to identify which hole the player started to make sure the scores are assigned to the right columns
  # conditioning could be done on Master2020$scorecards[[i]]$startHole
  
  library(plyr)
  holeId <- ldply(Master2020$scorecards[[x]]$scores, data.frame)[,1]
  
  cum.rows <- data.frame()
  #columnHeader <-c("TournementName", "playerID", "roundID", paste("ScoreHole",holeId, sep = ""), paste("ParHole",holeId, sep = ""), paste("YardageHole",holeId, sep = ""))
  #colnames(cum.rows) <- scoreHeader
  
  for (i in 1:x) { 
    TournementName <- Master2020$scorecards[[i]]$eventName
    playerID <- Master2020$scorecards[[i]]$golferId
    roundID <- Master2020$scorecards[[i]]$round
    new.row <- c(TournementName, playerID, roundID, as.numeric(ldply(Master2020$scorecards[[i]]$scores, data.frame)[,4]),
                                               as.numeric(ldply(Master2020$scorecards[[i]]$scores, data.frame)[,3]), 
                                               as.numeric(ldply(Master2020$scorecards[[i]]$scores, data.frame)[,5]))
    cum.rows <- rbind(cum.rows, new.row)
  }
  
  columnHeader <-c("TournementName", "playerID", "roundID", paste("ScoreHole",holeId, sep = ""),
                   paste("ParHole",holeId, sep = ""),
                   paste("YardageHole",holeId, sep = ""))
  colnames(cum.rows) <- columnHeader
  
  return(cum.rows)
}


number.scorecard <- length(Master2020$scorecards)
tournement.1 <- ConvertJsonToDF(number.scorecard)
head(tournement.1)
tail(tournement.1)

  


