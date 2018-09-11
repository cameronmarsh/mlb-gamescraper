source("scrapeGameData.R")
source("googlesheetsController.R")

# check about try/catch with google access errors
# check when game metadata is available
# delete unnecessary code and objects
# record inning scores
# calculate stress scores and put them in the raw data
# put inning time recordings into their own functions
# set length of inning in pitcher raw data
# track end of pitcher inning


gameData <- NULL
homeOrAway <- NULL

#get the game data from baseballsavant.com
while(is.null(gameData)){
  url <- readline("Gamefeed URL: ")
  gameData <- getDataFromUrl(url)
}

#determine if the red sox are home or away
if(gameData$venue_id == "3"){
  homeOrAway = "home"
} else {
  homeOrAway = "away"
}

###TODO: check if this will compute before the game starts
homeTeam <- gameData$team_home[1, "team_fielding"]
awayTeam <- gameData$team_home[1, "team_batting"]
opponent <- NULL
if(homeOrAway == 'home'){
  opponent <- awayTeam
} else {
  opponent <- homeTeam
}

#have the user authenticate their google account in a browser
gs_auth()
#create new google sheet once user has been authenticated
##############TODO: MAKE SURE YOU DONT NEED TO AUTHENTICATE WITH EACH CALL
sheetTitle <- paste(awayTeam, "@", homeTeam, gameData$game_date)
createGoogleSheet(sheetTitle)
sheet <- gs_title(sheetTitle)
addWorksheetHeaders(sheet)




############################################################
#### Collect data throughout game and update the spreadsheet
############################################################

#keep track of pitchers seen so far
pitcherData <- gameData[[paste0(homeOrAway, "_pitchers")]]
gameTotalPitches <- 0
inning <- 1
inningTimes <- data.frame(rep(NA, 9), rep(NA, 9), rep(NA, 9), rep(NA, 9), rep(NA, 9), rep(NA, 9))
colnames(inningTimes) <- c("Start Time", "End Time", "Inning Length", "Inning Length In Seconds", "Time Between Innings", "Time Between Innings In Seconds")
pitcherRawData <- data.frame()


#while the game is not over, look for data updates and write new data to google sheet
while(TRUE){
  #grab data we haven't seen yet
  newPitches <- getNewPitchData(pitcherData, gameTotalPitches)
  
  #if there is new data, 
  if(nrow(newPitches) != 0){
    writeToGamefeed(sheet, getNewPitchData(pitcherData, 0))
    
    #create raw data for each pitcher by inning
    pitcherRawData <- getPitcherRawData(getNewPitchData(pitcherData, 0), homeOrAway, opponent, gameData$game_date, inningTimes)
    writePitcherRawData(sheet, pitcherRawData)
      
    # pitcherStress <- getInningStressScores(pitcherData)
    # writeStressScores(sheet, pitcherStress)
    
    #record time inning starts/ends
    if("3" %in% newPitches$outs){
      inningTimes[inning, "End Time"] = strftime(Sys.time(), "%T")
      
      #set inning length, if possible
      if(!is.na(inningTimes[inning, "Start Time"])){
        startTime <- as.numeric(as.POSIXct(paste(gameData$game_date, inningTimes[inning, "Start Time"])))
        endTime <- as.numeric(as.POSIXct(paste0(gameData$game_date, inningTimes[inning, "End Time"])))
        inningLength <- endTime - startTime
        hours <- round(inningLength / 3600, 0)
        minutes <- round(inningLength / 60, 0)
        seconds <- inningLength %% 60
        inningTimes[inning, "Inning Length In Seconds"] = inningLength
        inningTimes[inning, "Inning Length"] = paste0(hours, "h ", minutes, "m ", seconds, "s")
        
      }
    }
    
    #NOTE: if started after the inning starts, it will not be accurate
    if(max(as.numeric(pitcherRawData$Inning) > inning)){
      inning <- max(as.numeric(pitcherRawData$Inning))
      inningTimes[inning, "Start Time"] = strftime(Sys.time(), "%T")
      
      #set interval between innings, if possible
      if(!is.na(inningTimes[inning-1, "End Time"])){
        lastInningEnd <- as.numeric(as.POSIXct(paste(gameData$game_date, inningTimes[inning-1, "End Time"])))
        thisInningStart <- as.numeric(as.POSIXct(paste0(gameData$game_date, inningTimes[inning, "Start Time"])))
        preInningTime <- thisInningStart - lastInningEnd
        hours <- round(preInningTime / 3600, 0)
        minutes <- round(preInningTime / 60, 0)
        seconds <- preInningTime %% 60
        inningTimes[inning, "Time Between Innings In Seconds"] = preInningTime
        inningTimes[inning, "Time Between Innings"] = paste0(hours, "h ", minutes, "m ", seconds, "s")
      }
    }
    
    
    gameTotalPitches <- max(newPitches$game_total_pitches)
  } else {
    print("No new data, checking for updates...")
  }
  
  ###TODO: check if we need to save this data -> could waste a lot of space
  #look for updated data from baseballsavant gamefeed
  #if the game is over, exit
  if(isGameOver(gameData$game_status)){
    break
  }
  gameData <- getDataFromUrl(url)
  
  #update data for next iteration of the loop
  pitcherData <- gameData[[paste0(homeOrAway, "_pitchers")]]
  
  Sys.sleep(10)
}

