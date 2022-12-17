library(haven)
library(tidyverse)

MOVregn <- read.csv('C:/Users/drewm/Desktop/liveBetting/nba/MOVreg.csv')
OUregn <- read.csv('C:/Users/drewm/Desktop/liveBetting/nba/OUreg.csv')

intercept <- MOVregn[1,2]
currentMOVn <- MOVregn[2,2]
spread <- MOVregn[3,2]
interceptOn <- OUregn[1,2]
currenttotaln <- OUregn[2,2]
totalMinleft <- OUregn[3,2]

vegasNBA <- data.frame(a = 100:114, b= 100:114, c = c(rep(NA,15)), d = c(rep(NA,15)))
colnames(vegasNBA) <- c("Spread", "OU", "AwayTeam", "HomeTeam")

vn <- function(nGames)
{
  
  sure <- as.numeric(readline("Are you sure? "))
  if(sure) {
    vegasNBA[!is.na(vegasNBA)] <<- NA
    
    for (x in 1:nGames) {
      while(is.na(vegasNBA[x,1])) {
        vegasNBA[x,1] <<- as.numeric(readline("Line: "))
      }
      while(is.na(vegasNBA[x,2])) {
        vegasNBA[x,2] <<- as.numeric(readline("O/U: "))
      }
      while(is.na(vegasNBA[x,3])) {
        vegasNBA[x,3] <<- readline("Away Team: ")
      }
      while(is.na(vegasNBA[x,4])) {
        vegasNBA[x,4] <<- readline("Home Team: ")
      }
    }
    vegasNBA$Spread <<- as.numeric(vegasNBA$Spread)
    vegasNBA$OU <<- as.numeric(vegasNBA$OU)
  }
}


gn <- function()
{
  inputHomeTeam <- readline("Home Team: ")
  inputGame <- which(vegasNBA$HomeTeam == inputHomeTeam)
  
  if (is.na(inputGame)) {
    inputSpreadn <<- as.numeric(readline("Vegas Line: "))
    inputOU <<- as.numeric(readline("Vegas O/U: "))
    inputAwayTeam <<- readline("Away Team: ")
  } else {
    inputSpreadn <<- vegasNBA[inputGame, 1]
    inputOU <<- vegasNBA[inputGame, 2]
    inputAwayTeam <<- vegasNBA[inputGame, 3]
    cat("Spread: ", inputSpreadn, "Over/Under: ", inputOU)
  }
  inputAway <- as.numeric(readline("Current Away: "))
  inputHome <- as.numeric(readline("Current Home: "))
  inputQuarter <- as.numeric(readline("Current Quarter: "))
  inputTime <- as.numeric(readline("Current Minutes: "))
  MOV <- inputAway - inputHome
  total <- inputAway + inputHome
  time <- ((5 - inputQuarter) * 12) - (12 - (inputTime))
  sf <- (time / 48) * inputSpreadn
  projMOV <- intercept + (currentMOVn * MOV) + (spread * sf)
  projOU <- ((interceptOn + 
               (currenttotaln * total) + 
               (totalMinleft * time)) * ((48 - time) / 48)) + 
            (inputOU * (time / 48))
  cat("Projected MOV:", projMOV, '\n', "Projected Total:", projOU)
}


