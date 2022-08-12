library(haven)
library(tidyverse)

formatBBall <- function(df) {
  pbp <- df[,c('URL','WinningTeam','Quarter','SecLeft','AwayTeam','AwayScore',
               'HomeTeam','HomeScore')]
  pbp$TotalSecLeft <- ((5 - pbp$Quarter) * 720) - (720 - pbp$SecLeft)
  pbp$MOV <- pbp$AwayScore - pbp$HomeScore 
  pbp$TotalScore <- pbp$AwayScore + pbp$HomeScore
  
  finalscores <- pbp %>% filter(pbp$TotalSecLeft==0)
  finalscores <- finalscores %>% distinct(URL,.keep_all = TRUE)
  finalscores <- finalscores[,c('URL','AwayScore','HomeScore','MOV','TotalScore')]
  
  pbp <- pbp[,c('URL','AwayScore','HomeScore','TotalSecLeft','MOV','TotalScore')]
  pbp <- right_join(pbp,finalscores,by='URL')
  colnames(pbp) <- c('URL','AwayScore','HomeScore','TotalSecLeft','CurrentMOV','CurrentTotal',
                     'FinalAway', 'FinalHome','FinalMOV','FinalTotal')
  pbp
}

raw19 <- read_csv('C:/Users/drewm/Desktop/nbascrape/NBA_PBP_2019-20.csv')
raw20 <- read_csv('C:/Users/drewm/Desktop/nbascrape/NBA_PBP_2020-21.csv')

pbpB19 <- formatBBall(raw19)
pbpB20 <- formatBBall(raw20)

masterBBall <- rbind(pbpB19, pbpB20)

MOVfit <- lm(FinalMOV ~ CurrentMOV + TotalSecLeft, data=masterBBall)
summary(MOVfit)
intercept <- summary(MOVfit)$coefficients[1,1]
currentMOV <- summary(MOVfit)$coefficients[2,1]
totalsecleft <- summary(MOVfit)$coefficients[3,1]

{
  inputAway <- as.numeric(readline("Current Away: "))
  inputHome <- as.numeric(readline("Current Home: "))
  inputQuarter <- as.numeric(readline("Current Quarter: "))
  inputTime <- as.numeric(readline("Current Minutes: "))
}

MOV <- inputAway - inputHome
Time <- ((5 - inputQuarter) * 720) - (720 - (inputTime)*60)

projMOV <- intercept + (currentMOV * MOV) + (totalsecleft * Time)
projMOV
