require(googlesheets)

#create new sheet and worksheets
createGoogleSheet <- function(title){
  sheet <- gs_new(title = title, ws_title = "Stressful Inning Scores")
  gs_ws_new(ss = sheet, ws_title = "Pitcher Raw Data")
  gs_ws_new(ss = sheet, ws_title = "Inning Times")
  gs_ws_new(ss = sheet, ws_title = "Gamefeed")
}

#update the inning times worksheet
writeToInningTimes <- function(sheet, inningTimes){
  gs_edit_cells(sheet, ws = "Inning Times", anchor = "A1", 
                input = inningTimes[, c("Inning", "Start Time", "End Time", "Inning Length", "Time Between Innings")], 
                col_names = TRUE)
}

#update the Gamefeed page
writeToGamefeed <- function(sheet, newPitches){
 gs_edit_cells(ss = sheet, ws = "Gamefeed", anchor = "A1", input = newPitches, col_names = TRUE)
}

#write pitcher data by inning
writePitcherRawData <- function(sheet, pitcherRawData){
  gs_edit_cells(ss = sheet, ws = "Pitcher Raw Data", input = pitcherRawData, anchor = "A1", col_names = TRUE)
}

#update stressful inning scores for pitchers
writeStressScores <- function(sheet, stressScores){
  gs_edit_cells(ss = sheet, ws = "Stressful Inning Scores", input = stressScores, anchor = "A1", col_names = TRUE)
}

