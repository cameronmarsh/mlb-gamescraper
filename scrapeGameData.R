require(rvest)
require(jsonlite)

#red sox venue id = 3

#
#return the json data saved at the baseballsavant.com Gamefeed URL passed in
#
getDataFromUrl <- function(url){
  ret <- tryCatch(
    {
      html <- read_html(url)
      jsonList <- html_text(html_node(html, "main script")) %>% strsplit(";") %>% unlist()
      jsonStr <- jsonList[1]
      jsonStr <- sub(x = jsonStr, substr(jsonStr, 1, 13), replacement = "")
      return(fromJSON(jsonStr))
    }, error = function(e){
      return(NULL)
    })
}

#
#return true if the game is over, false otherwise
#
isGameOver <- function(gameStatus){
  #game status is "F" if finished and "I" if in progress
  if(gameStatus == "F") return(TRUE)
  return(FALSE)
}


#
# see if there is any new data from the data pull
# if there is, return it in a data frame
#
getNewPitchData <- function(pitcherData, gameTotalPitches){
  pitches <- data.frame()

  for(pitcher in names(pitcherData)){
    res <- pitcherData[[pitcher]]
    newPitchInd <- which(res$game_total_pitches > gameTotalPitches)
    pitches <- rbind(res[newPitchInd, c("pitcher_name", "inning", "player_total_pitches", "game_total_pitches", "result", "outs", "batter", "start_speed")], pitches)
  }
  
  return(pitches[order(pitches$game_total_pitches),])
}


#
# get the post work ratio for a given inning
#
getPostWorkToRestRatio <- function(inning, inningTimes){
  if(inning == 1) return(NA)
  
  return(inningTimes[inning-1, "Inning Length In Seconds"] / inningTimes[inning, "Time Between Innings In Seconds"])
}


#
# get the pre work to rest ratio for a given inning
#
getPreWorkToRestRatio <- function(inning, inningTimes){
  return(inningTimes[inning, "Inning Length In Seconds"]/inningTimes[inning, "Time Between Innings In Seconds"])
}

#
# get NOP Stress calculation
# this doesn't make sense
#
getNOPStress <- function(numPitches){
  if(is.na(numPitches)) return(NA)
  
  if(numPitches > 26.7) return(2)
  if(numPitches > 26.1) return(1.9)
  if(numPitches > 25.5) return(1.8)
  if(numPitches > 24.9) return(1.7)
  if(numPitches > 24.4) return(1.6)
  if(numPitches > 23.8) return(1.5)
  if(numPitches > 23.2) return(1.4)
  if(numPitches > 22.6) return(1.3)
  if(numPitches > 22) return(1.2)
  if(numPitches > 21.4) return(1.1)
  if(numPitches > 20.8) return(1)
  if(numPitches > 20.3) return(.9)
  if(numPitches > 19.7) return(.8)
  if(numPitches > 19.1) return(.7)
  if(numPitches > 18.5) return(.6)
  if(numPitches > 17.9) return(.5)
  if(numPitches > 17.3) return(.4)
  if(numPitches > 16.8) return(.3)
  if(numPitches > 16.2) return(.2)
  if(numPitches > 15.6) return(.1)
  if(numPitches < 3.3) return(-2)
  if(numPitches < 3.9) return(-1.9)
  if(numPitches < 4.5) return(-1.8)
  if(numPitches < 5.1) return(-1.7)
  if(numPitches < 5.6) return(-1.6)
  if(numPitches < 6.2) return(-1.5)
  if(numPitches < 6.8) return(-1.4)
  if(numPitches < 7.4) return(-1.3)
  if(numPitches < 8) return(-1.2)
  if(numPitches < 8.6) return(-1.1)
  if(numPitches < 9.2) return(-1)
  if(numPitches < 9.7) return(-.9)
  if(numPitches < 10.3) return(-.8)
  if(numPitches < 10.9) return(-.7)
  if(numPitches < 11.5) return(-.6)
  if(numPitches < 12.1) return(-.5)
  if(numPitches < 12.7) return(-.4)
  if(numPitches < 13.2) return(-.3)
  if(numPitches < 13.8) return(-.2)
  if(numPitches < 14.4) return(-.1)
  if(numPitches < 15.6) return(0)
  
  return(0)

}


#
# get PPB Stress
# could simplify this, and some values not accounted for
#
getPPBStress <- function(pitchesPerBatter){
    if(is.na(pitchesPerBatter)) return(NA)
  
    if(pitchesPerBatter > 5.99) return(2)
    if(pitchesPerBatter > 5.89) return(1.9)
    if(pitchesPerBatter > 5.79) return(1.8)
    if(pitchesPerBatter > 5.69) return(1.7)
    if(pitchesPerBatter > 5.59) return(1.6)
    if(pitchesPerBatter > 5.49) return(1.5)
    if(pitchesPerBatter > 5.39) return(1.4)
    if(pitchesPerBatter > 5.29) return(1.3)
    if(pitchesPerBatter > 5.19) return(1.2)
    if(pitchesPerBatter > 5.09) return(1.1)
    if(pitchesPerBatter > 4.99) return(1)
    if(pitchesPerBatter > 4.89) return(.9)
    if(pitchesPerBatter > 4.79) return(.8)
    if(pitchesPerBatter > 4.69) return(.7)
    if(pitchesPerBatter > 4.59) return(.6)
    if(pitchesPerBatter > 4.49) return(.5)
    if(pitchesPerBatter > 4.39) return(.4)
    if(pitchesPerBatter > 4.29) return(.3)
    if(pitchesPerBatter > 4.19) return(.2)
    if(pitchesPerBatter > 4.09) return(.1)
    #could split here and just do some arithmetic
    if(pitchesPerBatter < 2) return(-2)
    if(pitchesPerBatter < 2.1) return(-1.9)
    if(pitchesPerBatter < 2.2) return(-1.8)
    if(pitchesPerBatter < 2.3) return(-1.7)
    if(pitchesPerBatter < 2.4) return(-1.6)
    if(pitchesPerBatter < 2.5) return(-1.5)
    if(pitchesPerBatter < 2.6) return(-1.4)
    if(pitchesPerBatter < 2.7) return(-1.3)
    if(pitchesPerBatter < 2.8) return(-1.2)
    if(pitchesPerBatter < 2.9) return(-1.1)
    if(pitchesPerBatter < 3) return(-1)
    if(pitchesPerBatter < 3.1) return(-.9)
    if(pitchesPerBatter < 3.2) return(-.8)
    if(pitchesPerBatter < 3.3) return(-.7)
    if(pitchesPerBatter < 3.4) return(-.6)
    if(pitchesPerBatter < 3.5) return(-.5)
    if(pitchesPerBatter < 3.6) return(-.4)
    if(pitchesPerBatter < 3.7) return(-.3)
    if(pitchesPerBatter < 3.8) return(-.2)
    if(pitchesPerBatter < 3.9) return(-.1)
    if(pitchesPerBatter < 4) return(0)
  
    return(0)
}


#
# ROB Stress
#
getROBStress <- function(numRunners){
  if(is.na(numRunners)) return(NA)
  
  if(numRunners>4.9) return(3.7)
  if(numRunners>3.9) return(2.8)
  if(numRunners>2.9) return(1.9)
  if(numRunners>1.9) return(0.9)
  if(numRunners>1) return(0)
  
  return(0)
}

#
# LOI Stress
#
getLOIStress <- function(inningLength){
  if(is.na(inningLength)) return(NA)
  
  if(inningLength > 14) return(2)
  if(inningLength < 1) return(0)
  if(inningLength < 2) return(-1)
  
  else return(0)
}

#
# TBI Stress
#
getTBIStress <- function(timeBetweenInnings){
  if(is.na(timeBetweenInnings)) return(NA)
  
  if(timeBetweenInnings > 21) return(2)
  if(timeBetweenInnings < 1) return(0)
  if(timeBetweenInnings < 5) return(2)
  
  else return(0)
}

#
# Stressful Inning Total
#
getStressfulInningScore <- function(inning, pitcherInningData, inningTimes){
  prework <- getPreWorkToRestRatio(inning, inningTimes)
  postwork <- getPostWorkToRestRatio(inning, inningTimes)
  if(is.na(prework)) prework <- 0
  if(is.na(postwork)) postwork <- 0
  
  return(sum(getPostWorkToRestRatio(inning, inningTimes), #Post Work to Rest ratio
             getPreWorkToRestRatio(inning, inningTimes), #add Pre Work to Rest ratio
             getNOPStress(nrow(pitcherInningData)), # NOP Stress
             getPPBStress(round(nrow(pitcherInningData)/length(unique(pitcherInningData)), 2)), # PPB Stress
             getROBStress(length(unique(pitcherInningData$batter)) - length(unique(pitcherInningData$outs))), # ROB Sress
             getLOIStress(inningTimes[inning, "Inning Length In Seconds"]/60), # LOI Stress
             getTBIStress(inningTimes[inning, "Time Between Innings In Seconds"]/60)))
}


#
# Get the raw data for each pitcher
#
getPitcherRawData <- function(gamePitchData, homeOrAway, opponent, date, inningTimes){
  rawData <- data.frame()
  
  #get the unique pitchers to pitch in each inning
  for(inning in as.numeric(unique(gamePitchData$inning))){
    inningPitchData <- gamePitchData[which(gamePitchData$inning == inning),]
    for(pitcher in unique(inningPitchData$pitcher_name)){
      pitcherInningData <- inningPitchData[which(inningPitchData$pitcher_name == pitcher), ]
      pitcherEntry <- data.frame(pitcher, 
                                 strftime(date, format = "%Y"), 
                                 homeOrAway, 
                                 opponent, 
                                 date,
                                 inning, 
                                 round(nrow(pitcherInningData)/nrow(inningPitchData), 2), #inning type
                                 nrow(pitcherInningData), #total pitches
                                 length(unique(pitcherInningData$batter)), #batters
                                 round(nrow(pitcherInningData)/length(unique(pitcherInningData)), 2), #pitches per batter
                                 length(unique(pitcherInningData$batter)) - length(unique(pitcherInningData$outs)), #runners
                                 inningTimes[inning, "Inning Length"], #length of inning
                                 round(mean(as.double(inningPitchData$start_speed)), 2), #avg pitch vel
                                 getPostWorkToRestRatio(inning, inningTimes), #Post Work to Rest ratio
                                 getPreWorkToRestRatio(inning, inningTimes), #add Pre Work to Rest ratio
                                 getNOPStress(nrow(pitcherInningData)), # NOP Stress
                                 getPPBStress(round(nrow(pitcherInningData)/length(unique(pitcherInningData)), 2)), # PPB Stress
                                 getROBStress(length(unique(pitcherInningData$batter)) - length(unique(pitcherInningData$outs))), # ROB Sress
                                 getLOIStress(inningTimes[inning, "Inning Length In Seconds"]/60), # LOI Stress
                                 getTBIStress(inningTimes[inning, "Time Between Innings In Seconds"]/60), # TBI Stress
                                 getStressfulInningScore(inning, pitcherInningData, inningTimes), #stress score
                                 stringsAsFactors = FALSE  
                                )
      colnames(pitcherEntry) <- c("Pitcher", "Season", "Venue", "Opponent", "Date", "Inning", "Inning Type", 
                                  "Total Pitches", "Number of Batters", "Pitches Per Batter", "Number Runners on Base",
                                  "Inning Length", "Avg Pitch Velocity", "Post Work to Rest Ratio", 
                                  "Pre Work to Rest Ratio", "NOP Stress", "PPB Stress", "ROB Stress", "LOI Stress", "TBI Stress", "Stressful Inning Total")
      rawData <- rbind(pitcherEntry, rawData)
    }
  }
  
  return(rawData)
}

#
# create the table for inning stress scores for each pitcher to write to the google sheet
#
getInningStressScores <- function(pitcherRawData){
  stressScores <- data.frame()
  for(i in 1:nrow(pitcherRawData)){
    pitcherInning <- pitcherRawData[i, c("Pitcher", "Inning", "Stressful Inning Total")]
    stressScores <- rbind(pitcherInning, stressScores)
  }
  
  return(stressScores)
}




  

