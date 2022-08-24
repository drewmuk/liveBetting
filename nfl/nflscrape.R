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

MOVfit <- lm(finalMOV ~ eMOV + game_seconds_remaining, data=masterfile)
OUfit <- lm(finalOU ~ currentOU + game_seconds_remaining + total_line, data=masterfile)

write.csv(summary(MOVfit)$coefficients[,1],'C:/Users/drewm/liveBetting/nfl/MOVreg.csv')
write.csv(summary(OUfit)$coefficients[,1],'C:/Users/drewm/liveBetting/nfl/OUreg.csv')


# roof types (type of roof for each home team's stadium)

roof_df <- data.frame(a = c(rep(NA,32)), b=c(rep("outdoors",32)))
roof_df$a <- c("ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN","DET",
               "GB","HOU","IND","JAX","KC","LAC","LAR","LV","MIA","MIN","NE","NO",
               "NYG","NYJ","PHI","PIT","SEA","SF","TB","TEN","WAS")
roof_df[which(roof_df$a=="LV" | roof_df$a=="NO" |
                roof_df$a=="DET" | roof_df$a=="LAR" |
                roof_df$a=="LAC" | roof_df$a=="MIN"),2] <- "dome"

write.csv(roof_df,'C:/Users/drewm/Desktop/liveBetting/nfl/roof_df.csv',row.names = FALSE)

# slate info (number of games per slate)

slate_df <- data.frame(a = c(rep(c(1:18),5), c(4,5,8,10)),
                       b = c(rep("T",18), rep("S",54), rep("M",18), rep("S", 4)), 
                       c = c(rep("N",18), rep("M",18), rep("A",18),rep("N",36), rep("E",4)), 
                       d = c(rep(1,18), rep(9,18), rep(4,18), rep(1,36), rep(1,4)))
colnames(slate_df) <- c("Week","Day","Time","Games")

slate_df[which((slate_df$Week==6 | slate_df$Week==8 |
                  slate_df$Week==9 | slate_df$Week==11 |
                  slate_df$Week==13) & slate_df$Time=="M"),4] <- 8

slate_df[which((slate_df$Week==2 | slate_df$Week==7 |
                  slate_df$Week==10 | slate_df$Week==12 |
                  slate_df$Week==14) & slate_df$Time=="M"),4] <- 7

slate_df[which((slate_df$Week==4 | slate_df$Week==5 |
                 slate_df$Week==6 | slate_df$Week==10 |
                  slate_df$Week==11 | slate_df$Week==14 |
                  slate_df$Week==15) & slate_df$Time=="A"),4] <- 3

slate_df[which(slate_df$Week==2 & slate_df$Time=="A"),4] <- 5
slate_df[which(slate_df$Week==2 & slate_df$Day=="M"),4] <- 2
slate_df[which(slate_df$Week==9 & slate_df$Time=="A"),4] <- 2
slate_df[which(slate_df$Week==12 & slate_df$Day=="T"),4] <- 3
slate_df[which(slate_df$Week==15 & slate_df$Time=="M"),4] <- 5

write.csv(slate_df,'C:/Users/drewm/Desktop/liveBetting/nfl/slate_df.csv',row.names = FALSE)
