library(haven)
library(tidyverse)

library(nflreadr)
library(nflfastR)

formatPBP <- function(year) {
  raw <- load_pbp(year)
  pbp <- raw %>% filter(!is.na(raw$posteam_type))
  pbp <- pbp[, c('game_id','home_team','away_team','posteam_type','yardline_100',
                 'total_home_score', 'total_away_score','game_seconds_remaining',
                 'drive','down','ydstogo','home_score', 'away_score','ep','touchdown',
                 'series_result','drive_ended_with_score','spread_line','total_line',
                 'fixed_drive_result')]
  
  pbp$ep[is.na(pbp$ep)] <- 0
  
  pbp$spreadfactor <- pbp$spread_line * (pbp$game_seconds_remaining / 3600)
  pbp$OUfactor <- pbp$total_line * (pbp$game_seconds_remaining / 3600)
  
  pbp$es <- if_else(pbp$posteam_type=='away',pbp$total_away_score+pbp$ep,
                    pbp$total_home_score+pbp$ep)
  
  pbp$os <- if_else(pbp$posteam_type=='away',pbp$total_home_score,
                    pbp$total_away_score)
  
  pbp$eMOV <- if_else(pbp$posteam_type=='away',pbp$es - pbp$os, 
                      pbp$os - pbp$es) + pbp$spreadfactor
  
  pbp$currentOU <- pbp$es + pbp$os + pbp$OUfactor
  
  pbp$finalMOV <- pbp$away_score - pbp$home_score
  pbp$finalOU <- pbp$away_score + pbp$home_score
  
  pbp
}

pbp21 <- formatPBP(2021)
pbp20 <- formatPBP(2020)
pbp19 <- formatPBP(2019)

masterfile <- rbind(pbp21, pbp20, pbp19)
masterfile$time_intervals <- ceiling(masterfile$game_seconds_remaining / 60)
masterfile$yard_intervals <- ceiling(masterfile$yardline_100 / 10) * 10

raw <- load_pbp(2021)

intervals <- masterfile %>% group_by(time_intervals, yard_intervals) %>%
  summarise(n=n())

MOVfit <- lm(finalMOV ~ eMOV + game_seconds_remaining, data=masterfile)
interceptM <- summary(MOVfit)$coefficients[1,1]
currentMOV <- summary(MOVfit)$coefficients[2,1]
totalsecleft <- summary(MOVfit)$coefficients[3,1]
write.csv(summary(MOVfit)$coefficients[,1],'C:/Users/drewm/Desktop/nflLive/MOVreg.csv')

OUfit <- lm(finalOU ~ currentOU + game_seconds_remaining + total_line, data=masterfile)
interceptO <- summary(OUfit)$coefficients[1,1]
currenttotal <- summary(OUfit)$coefficients[2,1]
totalSecleft <- summary(OUfit)$coefficients[3,1]
OU <- summary(OUfit)$coefficients[4,1]
write.csv(summary(OUfit)$coefficients[,1],'C:/Users/drewm/Desktop/nflLive/OUreg.csv')

