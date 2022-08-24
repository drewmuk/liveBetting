library(tidyverse)

MOVreg <- read.csv('C:/Users/drewm/Desktop/liveBetting/nfl/MOVreg.csv')
OUreg <- read.csv('C:/Users/drewm/Desktop/liveBetting/nfl/OUreg.csv')
roofs <- read.csv('C:/Users/drewm/Desktop/liveBetting/nfl/roof_df.csv')
slate <- read.csv('C:/Users/drewm/Desktop/liveBetting/nfl/slate_df.csv')

interceptM <- MOVreg[1,2]
interceptO <- OUreg[1,2]
currentMOV <- MOVreg[2,2]
currenttotal <- OUreg[2,2]
totalsecleft <- MOVreg[3,2]
totalSecleft <- OUreg[3,2]
OU <- OUreg[4,2]

calc_things <- function(vegasLine, vegasOU, aTeam, hTeam, aScore, hScore, posteam, quarter, 
                        minutes, yardLine, fieldSide, down, ydsToGo, pTO, dTO) 
{
  secLeft <- ((5 - quarter) * 900) - (900 - (minutes)*60)
  if(quarter > 2) {
    halfsecLeft <- secLeft
  } else {
    halfsecLeft <- ((3 - quarter) * 900) - (900 - (minutes)*60)
  }
  
  spreadfactor1 <- vegasLine * (secLeft / 3600)
  OUfactor1 <- vegasOU * (secLeft / 3600)
  if(posteam==1) {
    xteam <- hTeam
  } else {
    xteam <- aTeam
  }
  roof_type <- roofs[which(roofs$a==hTeam),2]
  
  data <- tibble::tibble(
    "season" = 2021,
    "home_team" = hTeam,
    "posteam" = xteam,
    "roof" = roof_type,
    "half_seconds_remaining" = halfsecLeft,
    "yardline_100" = ifelse(fieldSide==0,100-yardLine,yardLine),
    "down" = down,
    "ydstogo" = yardLine,
    "posteam_timeouts_remaining" = pTO,
    "defteam_timeouts_remaining" = dTO
  )
  
  # use the fastR calculator for expected points
  
  projEP <<- (nflfastR::calculate_expected_points(data) %>% select(ep))[1,1]
  eMOV <- ifelse(posteam==0,aScore-hScore+projEP,aScore-hScore-projEP) + spreadfactor1
  total <- aScore + hScore + projEP + OUfactor1
  
  if(!is.na(inputSpread)) {
    projMOV <- interceptM + (currentMOV * eMOV) + (totalsecleft * secLeft)
    cat("Projected Margin of Victory: ", round(projMOV,2),'\n')
  }
  
  if(!is.na(inputOU)) {
    projOU <- interceptO + (currenttotal * total) + (totalSecleft * secLeft) + (OU * vegasOU)
    cat("Projected Total Points: ", round(projOU,2))
  }
}

vegas <- data.frame(a = 100:115,b = 100:115, c = c(rep(NA,16)), d = c(rep(NA,16)))
colnames(vegas) <- c("Spread", "OU", "AwayTeam", "HomeTeam")


v <- function(nGames,week=NA,day=NA,time=NA)
{
  vegas[!is.na(vegas)] <<- NA
  
  if (!is.na(week)) {
    nGames <- slate[which(slate$Week==week & slate$Day==day & slate$Time==time),4]
  }
  
  for (x in 1:nGames) {
    while(is.na(vegas[x,1])) {
      vegas[x,1] <<- as.numeric(readline("Line: "))
    }
    while(is.na(vegas[x,2])) {
      vegas[x,2] <<- as.numeric(readline("O/U: "))
    }
    while(is.na(vegas[x,3])) {
      vegas[x,3] <<- readline("Away Team: ")
    }
    while(is.na(vegas[x,4])) {
      vegas[x,4] <<- readline("Home Team: ")
    }
  }
  vegas$Spread <<- as.numeric(vegas$Spread)
  vegas$OU <<- as.numeric(vegas$OU)
}

g <- function()
{
  inputHomeTeam <- readline("Home Team: ")
  inputGame <- which(vegas$HomeTeam == inputHomeTeam)
  
  
  if (is.na(inputGame)) {
    inputSpread <<- as.numeric(readline("Vegas Line: "))
    inputOU <<- as.numeric(readline("Vegas O/U: "))
    inputAwayTeam <<- readline("Away Team: ")
  } else {
    inputSpread <<- vegas[inputGame, 1]
    inputOU <<- vegas[inputGame, 2]
    inputAwayTeam <<- vegas[inputGame, 3]
    cat("Spread: ", inputSpread, " O/U: ", inputOU)
  }
  
  inputAwaySc <- as.numeric(readline("Away Score: "))
  inputHomeSc <- as.numeric(readline("Home Score: "))
  
  inputPos <- as.numeric(readline("Team With Ball (0=Away) : "))
  while(inputPos>1){
    inputPos <- as.numeric(readline("Try again, has to be 0 or 1: "))
  }
  
  inputQuarter <- as.numeric(readline("Current Quarter: "))
  while(inputQuarter==5){
    inputPos <- as.numeric(readline("OT Coming Soon"))
  }
  while((inputQuarter>5) | (inputQuarter < 1)) {
    inputQuarter <- as.numeric(readline("Try again, has to be 1-4: "))
  }
  
  inputTime <- as.numeric(readline("Current Minutes: "))
  while((inputTime>15) | (inputPos < 0)) {
    inputTime <- as.numeric(readline("Try again, has to be 0-15: "))
  }
  
  inputSide <- as.numeric(readline("Current Side (0=own): "))
  while(inputSide>1){
    inputSide <- as.numeric(readline("Try again, has to be 0 or 1: "))
  }
  
  inputLine <- as.numeric(readline("Current Yard Line: "))
  while((inputLine>99) | (inputLine < 1)) {
    inputLine <- as.numeric(readline("Try again, has to be 1-99: "))
  }
  
  inputDown <- as.numeric(readline("Current Down: "))
  while((inputDown>4) | (inputDown < 1)) {
    inputDown <- as.numeric(readline("Try again, has to be 1-4: "))
  }
  
  inputYdsToGo <- as.numeric(readline("Current Yards to Go: "))
  while(inputYdsToGo<1){
    inputYdsToGo <- as.numeric(readline("Try again, has to be at least 1: "))
  }
  
  inputpTO <- as.numeric(readline("Pos. Team Timeouts: "))
  while((inputpTO>3) | (inputpTO < 0)) {
    inputpTO <- as.numeric(readline("Try again, has to be 0-3: "))
  }
  
  inputdTO <- as.numeric(readline("Def. Team Timeouts: "))
  while((inputdTO>3) | (inputdTO < 0)) {
    inputdTO <- as.numeric(readline("Try again, has to be 0-3: "))
  }
  
  calc_things(inputSpread, inputOU, inputAwayTeam, inputHomeTeam,inputAwaySc, 
              inputHomeSc, inputPos, inputQuarter, inputTime, inputLine, inputSide, 
              inputDown, inputYdsToGo, inputpTO, inputdTO)
}
