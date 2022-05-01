library(shiny)
library(baseballr)
library(tidyverse)
library(DT)
library(magrittr)
library(future)
library(data.table)
source("download_data.R")

#Finds and selects to players_fgs_ids FanGraphs ID and other IDs for different functions from MLB.com ID using Chadwicks Reference Table
#Only runs between 01:00  and 01:30

t1 <- as.POSIXct("01:00:00", format = "%H:%M:%S")
t2 <- as.POSIXct("21:55:00", format = "%H:%M:%S")

current_time <- Sys.time()

#update current_time only if its 1:00 - 1:30
if(t1 < current_time & t2 > current_time){
  #Read Daily version of Chadwick's Table
  chadwick_ids <- fread("chadwick_table.csv")

  players_IDs_by_ips <- inningsfilter(chadwick_ids, 12, 50)

  #pulls FanGraphs to fgs_IDs Vector
  fgs_IDs <- pull(players_IDs_by_ips, "key_fangraphs")

  #pulls MLB.com IDs to mlbam_IDs Vector
  mlbam_IDs <- pull(players_IDs_by_ips, "key_mlbam")

  goose_game_logs_csv(mlbam_IDs, fgs_IDs)
}

### J-Score Calculations ###
game_logs <- fread("game_logs.csv")

game_logs$IP %<>% round(3) %>%
  format()
  
game_logs %<>% separate(IP, into = c("IP", "OAS")) %>%
  mutate("J-Score" = 50 
        + (as.numeric(IP)*3) 
        + as.numeric(OAS) 
        + GIDP 
        - (H*2) 
        - (2*(BB)) 
        - (ER*4) 
        - (R*2)
        + if_else(as.numeric(IP) > 4 & as.numeric(IP) <  8, 
                  + (as.numeric(IP) - 5) * 2, 
                  0)
        + if_else(as.numeric(IP) > 7 & as.numeric(IP) < 10, 
                  + (as.numeric(IP) - 7) * 4, 
                  0)
  )

mlb_games <- mlb_game_pks("2022-04-23")

fwrite(mlb_games, "game_packs.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("J-Score"),
    
    #DT::dataTableOutput("ip_leaders")
    DT::dataTableOutput("game_logs"),
)

server <- function(input, output) {
  #Output for R Shiny
  output$game_logs <- renderDataTable(
   game_logs
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
