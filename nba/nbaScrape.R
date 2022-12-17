library(haven)
library(tidyverse)
library(hoopR)

nba_pbp23 <- progressr::with_progress({nba_pbp <- hoopR::load_nba_pbp(seasons=2023)})
nba_pbp22 <- progressr::with_progress({nba_pbp <- hoopR::load_nba_pbp(seasons=2022)})
nba_pbp21 <- progressr::with_progress({nba_pbp <- hoopR::load_nba_pbp(seasons=2021)})

pbp <- rbind(nba_pbp23, nba_pbp22, nba_pbp21)
pbp <- pbp[,c('game_id','home_score','away_score','away_team_abbrev',
              'home_team_abbrev','game_spread','qtr','clock_minutes', 'season')]

# one row per game, with final score
allgames <- pbp[nrow(pbp):1,]
allgames <- allgames[!duplicated(allgames$game_id)]
allgames <- allgames[nrow(allgames):1,]

pbp$min_left <- ((5 - pbp$qtr) * 12) - (12 - pbp$clock_minutes)

# adjusting the spread by the amount of time left in the game
pbp$spreadfactor <- pbp$game_spread * (pbp$min_left / 48)

# adding the final scores
pbp <- left_join(pbp, allgames[,1:3], by="game_id")
pbp <- pbp %>% rename('final_home_score' = 'home_score.y',
                      'final_away_score' = 'away_score.y',
                      'home_score' = 'home_score.x',
                      'away_score' = 'away_score.x')

pbp$finalMOV <- pbp$final_away_score - pbp$final_home_score
pbp$currentMOV <- pbp$away_score - pbp$home_score
pbp$finaltotal <- pbp$final_away_score + pbp$final_home_score
pbp$currenttotal <- pbp$away_score + pbp$home_score

MOVfit <- lm(finalMOV ~ currentMOV + spreadfactor, data=pbp)
OUfit <- lm(finaltotal ~ currenttotal + min_left, data=pbp)

write.csv(summary(MOVfit)$coefficients[,1],'C:/Users/drewm/Desktop/liveBetting/nba/MOVreg.csv')
write.csv(summary(OUfit)$coefficients[,1],'C:/Users/drewm/Desktop/liveBetting/nba/OUreg.csv')
