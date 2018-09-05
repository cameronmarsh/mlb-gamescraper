source("scrapeGameData.R")
source("googlesheetsController.R")

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
innings <- 0
pitcherRawData <- data.frame()


#while the game is not over, look for data updates and write new data to google sheet
while(TRUE){
  #grab data we haven't seen yet and write to gamefeed worksheet
  newPitches <- getNewPitchData(pitcherData, gameTotalPitches)
  writeToGamefeed(sheet, newPitches, anchor = paste0("A", gameTotalPitches+1))
  
  #create raw data for each pitcher by inning
  pitcherRawData <- rbind(pitcherRawData, getPitcherRawData(newPitches, homeOrAway, opponent, gameData$game_date))
  writePitcherRawData(sheet, pitcherRawData)
    
  pitcherStress <- getInningStressScores(pitcherData)
  writeStressScores(sheet, pitcherStress)
  
  gameTotalPitches <- max(newPitches$game_total_pitches)
  
  #look for updated data from baseballsavant gamefeed
  gameData <- getDataFromUrl(url)
  #if the game is over, exit
  if(isGameOver(gameData$game_status)){
    break
  }
  
  #update data for next iteration of the loop
  pitcherData <- gameData[[paste0(homeOrAway, "_pitchers")]]
  
}

