library(rvest)
library(jsonlite)

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
# Get the raw data for each pitcher
#
getPitcherRawData <- function(gamePitchData, homeOrAway, opponent, date){
  rawData <- data.frame()
  
  #get the unique pitchers to pitch in each inning
  for(inning in unique(gamePitchData$inning)){
    inningPitchData <- gamePitchData[which(gamePitchData$inning == inning),]
    for(pitcher in unique(inningPitchData$pitcher_name)){
      pitcherInningData <- inningPitchData[which(inningPitchData$pitcher_name == pitcher), ]
      pitcherEntry <- data.frame(pitcher, 
                                 strftime(date, format = "%Y"), 
                                 homeOrAway, 
                                 opponent, 
                                 date,
                                 inning, 
                                 round(nrow(pitcherInningData)/nrow(inningPitchData), 2),
                                 nrow(pitcherInningData),
                                 length(unique(pitcherInningData$batter)),
                                 round(nrow(pitcherInningData)/length(unique(pitcherInningData)), 2),
                                 length(unique(pitcherInningData$batter)) - length(unique(pitcherInningData$outs)),
                                 NA, ###TODO: enter the inning length here 
                                 round(mean(as.double(inningPitchData$start_speed)), 2), 
                                 NA, ###TODO: add Post Work to Rest ratio
                                 NA, ###TODO: add Pre Work to Rest ratio,
                                 NA  #####TODO: calculate stress inning score and put it here
                                )
      colnames(pitcherEntry) <- c("Pitcher", "Season", "Venue", "Opponent", "Date", "Inning", "Inning Type", 
                                  "Total Pitches", "Number of Batters", "Pitches Per Batter", "Num Runners on Base",
                                  "Inning Length", "Avg Pitch Velocity", "Post Work to Rest Ratio", 
                                  "Pre Work to Rest Ratio", "Stressful Inning Total")
      rawData <- rbind(pitcherEntry, rawData)
    }
  }
  
  return(rawData)
}

#
# create the table for inning stress scores for each pitcher to write to the google sheet
#
getInningStressScores <- function(pitcherData){
  #get all pitch data since the game began
  stressScores <- data.frame()
  
  #
  
  return(stressScores)
}




  

