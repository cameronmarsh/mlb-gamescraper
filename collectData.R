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
createGoogleSheet(paste(awayTeam, "@", homeTeam, gameData$game_date))


#look for data update 
pitcherData <- gameData[[paste0(homeOrAway, "_pitchers")]]
  
