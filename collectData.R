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

homeTeam <- gameData$team_home[1, "team_fielding"]
awayTeam <- gameData$team_home[1, "team_batting"]

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

#add the current data to the spreadsheets, if there is any


#look for data update 
while(!isGameOver(gameData$game_status)){
  newPitches <- getNewPitchData(pitcherData, gameTotalPitches)
  writeToGamefeed(sheet = sheet, newPitches = newPitches, anchor = paste0("A", gameTotalPitches+1))
  gameTotalPitches <- max(newPitches$game_total_pitches)
  
  gameData <- getDataFromUrl(url)
  pitcherData <- gameData[[paste0(homeOrAway, "_pitchers")]]
  
}
