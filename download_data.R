library(baseballr)
library(tidyverse)
library(DT)
library(magrittr)
library(future)
library(data.table)

#Download Data From Chadwick's Directory
chadwick_table_csv <- function() {
  temp <- chadwick_player_lu()
  fwrite(temp, "chadwick_table.csv")
}


inningsfilter <- function(chadwick_data, innings = 12, pitcherlimit = 100){
  
  #Filters all pitchers with at least 12 innings pitched from MLB.com
  pitching_filter <- mlb_stats_leaders(leader_categories = "inningsPitched", limit = pitcherlimit) %>%
    filter("inningsPitched" > innings) %>%
    pull("person_id")
  
  players_IDs <- filter(chadwick_data, key_mlbam %in% pitching_filter) %>%
    select("key_fangraphs", "key_mlbam")

#returns a data frame of MLB.com and FanGraphs ids for players that fit the innings pitched filter
  return(players_IDs)
}

mlb_substitution_data <- function(){
  mlb
}


goose_game_logs_csv <- function(mlb_ids, fangraphs_ids){

###BASEBALL SAVANT PARSING###
#Initialize Empty Data Frame for BaseballSavant Game Logs
  savant_pbp_game_logs = NULL

  #GIDP and Pick-offs from Statcast (Maybe Handing 0-0 or lead to Pen)
  #for loop to get all pitches thrown by pitchers
  for (i in 1:length(mlb_ids)){
    savant_pbp_game_logs <- rbind(savant_pbp_game_logs, statcast_search_pitchers(start_date = "2022-04-01", end_date = "2022-11-01", pitcherid = mlb_ids[i]) %>%
                                    filter(events == "grounded_into_double_play") %>%
                                    group_by(game_date, player_name) %>%
                                    count(game_date)
    )
  }
  
  #turn n column name "n" into "GIDP"
  #player_name, and game_date are also changed to match the fgs_game_logs dataframe
  #mutate Player name to same convention has fgs_game_logs$PlayerName
  savant_pbp_game_logs %<>% 
    rename("GIDP" = "n", "PlayerName" = "player_name", "Date" = "game_date") %>%
    mutate(PlayerName = sub('(\\w+), (\\w+)', "\\2 \\1", PlayerName)) %>%
    as.data.frame() 
  
  #And change $Date form date type to string type
  savant_pbp_game_logs$Date %<>%
    format()
  


  ###FANGRAPHS PARSING###
  #Initialize Empty Data Frame for FanGraphs Game Logs
  fgs_game_logs = NULL
  
  #For loop to go FanGraphs game_logs for each "person_id" to compiled_game_logs
  for (i in 1:length(fangraphs_ids)){
    fgs_game_logs <- rbind(fgs_game_logs, fg_pitcher_game_logs(fangraphs_ids[i], year = 2022), fill = TRUE)
  }
  
  #getting only needed columns from FanGraphs
  
  fgs_game_logs %<>% 
    select("PlayerName", "Date", "playerid", "Opp", "Team", "HomeAway", "IP", "H", "R", "ER", "HR", "BB", "IBB", "WP", "BK", "SO")
  
  #combine fgs_game_logs and savant_pbp_game_logs by PlayerName and Date
  game_logs <- right_join(savant_pbp_game_logs, fgs_game_logs, by = c("PlayerName", "Date"))

#changing game_logs to a tibble
  game_logs %<>% as_tibble() %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

  fwrite(game_logs, "game_logs.csv")
}



