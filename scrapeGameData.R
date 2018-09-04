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
  # colnames(pitches) <- c("pitcher_name", "inning", "player_total_pitches", "game_total_pitches", "result", "outs")
  
  for(pitcher in names(pitcherData)){
    res <- pitcherData[[pitcher]]
    newPitchInd <- which(res$game_total_pitches > gameTotalPitches)
    pitches <- rbind(res[newPitchInd, c("pitcher_name", "inning", "player_total_pitches", "game_total_pitches", "result", "outs")], pitches)
  }
  
  return(pitches[order(pitches$game_total_pitches),])
}






  

