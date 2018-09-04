require(googlesheets)

createGoogleSheet <- function(title){
  sheet <- gs_new(title = title, ws_title = "Stressful Inning Scores")
  gs_ws_new(ss = sheet, ws_title = "Pitcher Raw Data")
  gs_ws_new(ss = sheet, ws_title = "Inning Times")
}