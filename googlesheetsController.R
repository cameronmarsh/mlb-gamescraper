require(googlesheets)

#create new sheet and worksheets
createGoogleSheet <- function(title){
  sheet <- gs_new(title = title, ws_title = "Stressful Inning Scores")
  gs_ws_new(ss = sheet, ws_title = "Pitcher Raw Data")
  gs_ws_new(ss = sheet, ws_title = "Inning Times")
  gs_ws_new(ss = sheet, ws_title = "Gamefeed")
}

#populate the worksheets with column headers
addWorksheetHeaders <- function(sheet){
  gs_edit_cells(ss = sheet, ws = "Stressful Inning Scores", input = c("Pitcher", "Inning", "Stressful Inning Total"), byrow = TRUE)
  gs_edit_cells(ss = sheet, ws = "Pitcher Raw Data", input = c("Pitcher", "Season", "Venue", "Opponent", "Date", "Inning", "Inning Type", "Total Pitches",
                                                               "Number of Batters", "Pitches Per Batter", "Number of Runners on Base", "Length of Inning", 
                                                               "Average Pitch Velocity", "Post Work to Rest Ratio", "Pre Work to Rest Ratio", 
                                                               "Stressful Inning Total"), byrow = TRUE)
  gs_edit_cells(ss = sheet, ws = "Inning Times", c("Start", "End", "Inning Length", "Time Between Innings"), byrow = TRUE)
  gs_edit_cells(ss = sheet, ws = "Gamefeed", c("pitcher_name", "inning", "player_total_pitches", "game_total_pitches", "result", "outs", "batter", "start_speed"), byrow = TRUE)
}, 


#update the Gamefeed page
writeToGamefeed <- function(sheet, newPitches){
 gs_edit_cells(ss = sheet, ws = "Gamefeed", anchor = "A2", input = newPitches, col_names = FALSE)
}

#write pitcher data by inning
writePitcherRawData <- function(sheet, pitcherRawData){
  gs_edit_cells(ss = sheet, ws = "Pitcher Raw Data", input = pitcherRawData, anchor = "A2", col_names = FALSE)
}

#update stressful inning scores for pitchers
writeStressScores <- function(sheet, stressScores){
  gs_edit_cells(ss = sheet, ws = "Stressful Inning Scores", input = stressScores, anchor = "A2")
}

