library(rvest)
library(jsonlite)

#red sox venue id = 3

url <- "https://baseballsavant.mlb.com/gamefeed?game_pk=531449&type=pitch_velocity&chart_view=pitch&chart_type=sbp&inning=&count=&batter_hand=&pitcher_hand=&filter="


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


isGameOver <- function(gameStatus){
  #game status is "F" if finished and "I" if in progress
  if(gameStatus == "F") return(TRUE)
  return(FALSE)
}






  

