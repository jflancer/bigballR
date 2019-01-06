convert_ids <- function(column, player_df){
  #via Matt Barlowe
  column <- player_df[match(column, player_df$id, nomatch = column),
                      c('Player')]
}
convert_numeric <- function(column){
  as.numeric(as.character(column))
}
event_angle <- function(x, y) {
  #via Emmanuel Perry
  return(abs(atan(y/(89 - abs(x)))*(180/pi)))
}
distance_from_net <- function(x, y) {
  # via Emmanuel Perry
  return(sqrt((89 - abs(x))^2 + y^2))
}
bindingFunction <- dplyr::bind_rows
get_roster_info <- function(game_id = NA, roster_json = NA){
  if(!is.na(game_id)) {
    roster_json <- rjson::fromJSON(file =  paste("https://www.nwhl.zone/game/get_play_by_plays?id=", game_id, sep = ''))$roster_player
  }

  roster_data <- lapply(roster_json, unlist)
  roster_data <- lapply(roster_data, FUN = function(x){ data.frame(t(x), stringsAsFactors = F) })
  roster_df <- do.call("bindingFunction", roster_data)
  return(roster_df)
}
get_team_info <- function(game_id = NA, team_json = NA){
  if(!is.na(game_id)) {
    team_json <- rjson::fromJSON(file = paste("https://www.nwhl.zone/game/get_play_by_plays?id=", game_id, sep = ''))$team_instance
  }

  team_data <- lapply(team_json, unlist)
  columns <- names(team_data[[1]])
  roster_df <- data.frame(matrix(unlist(team_data),
                                 byrow = T,
                                 nrow = length(team_data)),
                          stringsAsFactors = F)
  colnames(roster_df) <- columns
  return(roster_df)
}
get_game_info <- function(game_id = NA, game_json = NA){
  if(!is.na(game_id)) {
    game_json <- rjson::fromJSON(file = paste("https://www.nwhl.zone/game/get_play_by_plays?id=", game_id, sep = ''))$game
  }
  game_vect <- unlist(game_json)
  game_df <- data.frame(t(game_vect), stringsAsFactors = F)
  return(game_df)
}
complete_game_scrape <- function(game_id){
  game_id <- as.character(game_id) #handle if user enters it as numeric
  print(game_id)
  # Gets json file
  game_url <- paste("https://www.nwhl.zone/game/get_play_by_plays?id=", game_id, sep = '')
  pbp_json <- rjson::fromJSON(file = game_url)

  #Gets extraneous data from other functions
  game_data <- get_game_info(game_json = pbp_json$game)
  team_data <- get_team_info(team_json = pbp_json$team_instance)
  roster_data <- get_roster_info(roster_json = pbp_json$roster_player)

  #Formats other data
  team_ids <- dplyr::select(team_data, id, abbrev)

  players <- roster_data %>%
    dplyr::select(id, first_name, last_name) %>%
    dplyr::mutate(Player = paste(first_name,last_name)) %>%
    dplyr::select(-first_name, -last_name)

  #extracts play by play
  plays <- pbp_json$plays

  #return na if play by play not found
  if(length(plays) == 0){
    print(paste("NO PLAY BY PLAY DATA AVAILABLE FOR GAMEID",game_id))
    return(NA)
    }

  #This essentially converts fromJSON list to a dataframe
  plays_reduced <- lapply(plays, unlist)
  play_data <- lapply(plays_reduced, FUN = function(x){ data.frame(t(x), stringsAsFactors = F) })
  play_uncleaned <- do.call("bindingFunction", play_data)

  if(!"play_summary.x_coord" %in% colnames(play_uncleaned)){
    play_uncleaned$play_summary.x_coord <- NA
    play_uncleaned$play_summary.y_coord <- NA
    play_uncleaned$play_summary.loser_id <- NA
  }

  #This prepares everything
  play_prep <- play_uncleaned %>%
    #Selects relevant columns
    dplyr::select(game_id, play_index, clock_time, min, sec, time_interval,  play_actions.subseason_id, created_at,
           home_team_score, away_team_score, team_id,
           play_type,play_summary.x_coord, play_summary.y_coord,
           primary_player_id,
           play_summary.loser_id, play_summary.goalie_id,
           play_actions.away_team_goalie, play_actions.home_team_goalie) %>%
    #note: ifelse statements are to create NA column if the column wasn't found
    # common reasons:
        #shutout (no goals scored so no assists or plus/minus)
        #no penalties so no penalty data
        #no empty net goals, etc.
    dplyr::mutate(play_summary.assist_1_id = ifelse(rep("play_summary.assist_1_id" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                             play_uncleaned$play_summary.assist_1_id, NA),
           play_summary.assist_2_id = ifelse(rep("play_summary.assist_2_id" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                             play_uncleaned$play_summary.assist_2_id, NA),
           play_summary.served_by_id = ifelse(rep("play_summary.served_by_id" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                              play_uncleaned$play_summary.served_by_id, NA),
           play_summary.penalty_type = ifelse(rep("play_summary.penalty_type" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                              play_uncleaned$play_summary.penalty_type, NA),
           play_summary.infraction_type = ifelse(rep("play_summary.infraction_type" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                                 play_uncleaned$play_summary.infraction_type, NA),
           play_summary.penalty_minutes = dplyr::case_when(play_summary.penalty_type == "Minor" ~ 2,
                                                    play_summary.penalty_type == "Major" ~ 4,
                                                    play_summary.penalty_type == "1" ~ 2,
                                                    TRUE ~ 0),
           #PLAYER 1 is faceoff win, goal scorer/shot taker, shot blocker, penalty taker, turnover doer
           event_player_1 = primary_player_id,
           #PLAYER 2 is faceoff loss, assist1, penalty server, goalie on shots on goal
           event_player_2 = dplyr::case_when(play_type == "Faceoff" ~ ifelse(!is.na(play_summary.loser_id),play_summary.loser_id, NA_character_),
                                      play_type == "Shot" ~ ifelse(!is.na(play_summary.goalie_id),play_summary.goalie_id, NA_character_),
                                      play_type == "Penalty" ~ ifelse(!is.na(play_summary.served_by_id),play_summary.served_by_id,NA_character_),
                                      play_type == "Goal" ~ ifelse(!is.na(play_summary.assist_1_id),play_summary.assist_1_id,NA_character_),
                                      TRUE ~ NA_character_
                                      ),
           #PLAYER 3 is secondary assist
           event_player_3 = play_summary.assist_2_id,
           #As of now the only detail is penalty info
           event_detail = ifelse(play_type == "Penalty",
                                 paste( play_summary.penalty_minutes, play_summary.infraction_type, play_summary.penalty_type),
                                 NA),
           #create background columns
           home_team = game_data$home_team, #these are currently ids
           away_team = game_data$away_team,
           game_date = created_at,
           game_seconds = 1200*(convert_numeric(time_interval)-1) + (20-convert_numeric(min)) *60 -convert_numeric(sec),
           #same as before, this deals with columns not found
           minus_player_1 = ifelse(rep("play_actions.minus_player_1" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                   play_uncleaned$play_actions.minus_player_1, NA),
           minus_player_2 = ifelse(rep("play_actions.minus_player_2" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                   play_uncleaned$play_actions.minus_player_2, NA),
           minus_player_3 = ifelse(rep("play_actions.minus_player_3" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                   play_uncleaned$play_actions.minus_player_3, NA),
           minus_player_4 = ifelse(rep("play_actions.minus_player_4" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                   play_uncleaned$play_actions.minus_player_4, NA),
           minus_player_5 = ifelse(rep("play_actions.minus_player_5" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                   play_uncleaned$play_actions.minus_player_5, NA),
           minus_player_6 = ifelse(rep("play_actions.minus_player_6" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                   play_uncleaned$play_actions.minus_player_6, NA),
           plus_player_1 = ifelse(rep("play_actions.plus_player_1" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                  play_uncleaned$play_actions.plus_player_1, NA),
           plus_player_2 = ifelse(rep("play_actions.plus_player_2" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                  play_uncleaned$play_actions.plus_player_2, NA),
           plus_player_3 = ifelse(rep("play_actions.plus_player_3" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                  play_uncleaned$play_actions.plus_player_3, NA),
           plus_player_4 = ifelse(rep("play_actions.plus_player_4" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                  play_uncleaned$play_actions.plus_player_4, NA),
           plus_player_5 = ifelse(rep("play_actions.plus_player_5" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                  play_uncleaned$play_actions.plus_player_5, NA),
           plus_player_6 = ifelse(rep("play_actions.plus_player_6" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                  play_uncleaned$play_actions.plus_player_6, NA)
           )  %>%
    #Adds interval for events, ensures shots before faceoffs
    dplyr::arrange(game_seconds, dplyr::desc((play_type %in% c("Goal","Shot"))*1)) %>%
    dplyr::mutate(event_interval = ifelse(!is.na(dplyr::lag(game_seconds)), game_seconds - dplyr::lag(game_seconds), 0)) %>%
    #Remove columns not needed any more
    dplyr::select(-play_summary.loser_id, -play_summary.assist_1_id, -play_summary.goalie_id, -play_summary.served_by_id,
           -play_summary.assist_2_id, - play_summary.penalty_type, -play_summary.infraction_type) %>%
    #converts team and season ids to values
    dplyr::left_join(team_ids, by = c("home_team"="id")) %>%
    dplyr::left_join(team_ids, by = c("away_team"="id")) %>%
    dplyr::left_join(team_ids, by = c("team_id"="id")) %>%
    dplyr::left_join(seasons, by = c("play_actions.subseason_id" = "id")) %>%
    dplyr::select(-home_team, -away_team, -team_id, -play_actions.subseason_id, -created_at, -clock_time) %>%
    #cleaning column names
    dplyr::rename(home_team = abbrev.x,
           away_team = abbrev.y,
           event_team = abbrev,
           period = time_interval,
           event_type = play_type,
           x_coord = play_summary.x_coord,
           y_coord = play_summary.y_coord,
           away_goalie = play_actions.away_team_goalie,
           home_goalie = play_actions.home_team_goalie,
           penalty_length = play_summary.penalty_minutes
           ) %>%
    #Added in coordinate cleaning
    dplyr::mutate(
      #Converts to (-100,100), (-42.5,42.5) coordinate system
      x_coord = convert_numeric(x_coord)*1.98 - 99,
      y_coord = convert_numeric(y_coord)*0.85 - 42.5,
      #Converts shots to proper side of ice (note blocked shots are oriented to which team took it)
      x_coord_2 = ifelse(((event_team == home_team & x_coord > 0) | (event_team == away_team & x_coord < 0)) & event_type %in% c("Shot","Goal"), -x_coord,x_coord),
      y_coord_2 = ifelse(((event_team == home_team & x_coord > 0) | (event_team == away_team & x_coord < 0)) & event_type %in% c("Shot","Goal"), -y_coord,y_coord),
      x_coord_2 = ifelse(((event_team == home_team & x_coord < 0) | (event_team == away_team & x_coord > 0)) & event_type == "BlockedShot", -x_coord,x_coord),
      y_coord_2 = ifelse(((event_team == home_team & x_coord < 0) | (event_team == away_team & x_coord > 0)) & event_type == "BlockedShot", -y_coord,y_coord),
      #Pushes all events to one side of the ice
      x_coord_1 = ifelse(x_coord < 0, - x_coord, x_coord),
      y_coord_1 = ifelse(x_coord < 0, -y_coord, y_coord),
      event_angle = event_angle(x_coord, y_coord),
      event_distance = distance_from_net(x_coord, y_coord)
    ) %>%
    #putting everything in a logical order
    dplyr::select(Season, game_id, game_date, home_team, away_team,
           play_index, period, min, sec, game_seconds, event_interval,
           event_type, event_detail, x_coord, y_coord, event_team,
           event_player_1, event_player_2, event_player_3,
           event_angle,event_distance,x_coord_2:y_coord_1,
           home_goalie, away_goalie, plus_player_1:plus_player_5,plus_player_6,
           minus_player_1:minus_player_5,minus_player_6,
           penalty_length
           ) %>%
    dplyr::mutate(Season = substr(Season,1,4))

  #Added 9/19/18 by Carleen Markey
  #Converts source date in GMT into home team time zone then updates game_date colum with converted time
  #Set tz = to neame for the home team time zone in the local OS, ie. tz = "America/Indianapolis" each system is different
  if(any(play_prep$home_team == "MIN")) {
    play_prep$game_date <- as.POSIXct(as.integer(play_uncleaned$created_at_integer), origin = "1970-01-01", tz= "America/Chicago")
    play_prep$game_date <- as.character(play_prep$game_date)
    play_prep$game_date <- gsub(" .*","", play_prep$game_date)
  } else {
    play_prep$game_date <- as.POSIXct(as.integer(play_uncleaned$created_at_integer), origin = "1970-01-01", tz= "America/Indianapolis")
    play_prep$game_date <- as.character(play_prep$game_date)
    play_prep$game_date <- gsub(" .*","", play_prep$game_date)
  }


  #Create Score Columns
  play_prep$home_score <- 0
  play_prep$away_score <- 0

  #gets running count for scores
  play_prep$home_score <- 1*(play_prep$home_team == play_prep$event_team)*(play_prep$event_type == "Goal")
  play_prep$home_score[is.na(play_prep$home_score)] <- 0
  play_prep$home_score <- cumsum(play_prep$home_score)
  play_prep$away_score <- 1*(play_prep$away_team == play_prep$event_team)*(play_prep$event_type == "Goal")
  play_prep$away_score[is.na(play_prep$away_score)] <- 0
  play_prep$away_score <- cumsum(play_prep$away_score)

  #Converts to numeric columns
  play_prep$penalty_length <- convert_numeric(play_prep$penalty_length)
  play_prep$period <- convert_numeric(play_prep$period)

  #Gets home skater strength----

  #This takes events that could impact strength
  home_state_changes <- play_prep %>%
    dplyr::filter((event_type == "Goal" & event_team == away_team) |
             (event_type == "Penalty" & event_team == home_team)) %>%
    dplyr::select(event_type,game_seconds,penalty_length) %>%
    dplyr::mutate(event_type = ifelse(event_type == "Penalty",1,2),
           prev.event = dplyr::lag(event_type),
           prev.time = dplyr::lag(game_seconds),
           prev.length = dplyr::lag(penalty_length))

  #Ok this determines if a player should be added or subtracted from the home team count
  home_pen_mat <- apply(home_state_changes,
                        1,
                         FUN = function(x) {
      #Creates a -1 for duration of penalty and 0s surrounding it
      if(x[1] == 1 & x[2]+x[3]*60 < (max(play_prep$period)*1200-1)){
          c( rep( 0, length( 0:x[2] )),
          rep( -1, x[3]*60),
          rep(0, length((x[2]+x[3]*60 + 1):(max(play_prep$period)*1200-1)))
          )
        #Creates a -1 for duration of penalty and 0s before (for end of game penalties)
        } else if(x[1] == 1 & x[2]+x[3]*60 >= (max(play_prep$period)*1200-1)) {
          c( rep( 0, length( 0:x[2] )),
             rep(-1, max(play_prep$period)*1200-1-x[2] )
          )
        #Creates a +1 from time power play goal is scored to end of penalty to handle skater coming back on
        } else if( x[1] == 2 & (x[2] %in% ifelse(!is.na(x[5]) & !is.na(x[6]) & x[2] != x[5], x[5]:(x[5]+x[6]*60),-1 )) ) {
          c( rep( 0, length( 0:(x[2]) )),
            rep( 1, length( (x[2]+1):(x[6]*60-(x[2]-x[5])))),
            rep(0, length((x[6]*60-(x[2]-x[5])):(max(play_prep$period)*1200-1)))
          )
        # Creates all zeros if event doesnt effect strength
        } else {
          rep(0, length(0:(max(play_prep$period)*1200-1)))
        }
      })

  #creates vector for skaters
  home_skaters <- 5 + apply(home_pen_mat, 1, sum)

  #Gets away skater strength----

  #**See above comments for explanation

  away_state_changes <- play_prep %>%
    dplyr::filter((event_type == "Goal" & event_team == home_team) |
             (event_type == "Penalty" & event_team == away_team)) %>%
    dplyr::select(event_type,game_seconds,penalty_length) %>%
    dplyr::mutate(event_type = ifelse(event_type == "Penalty",1,2),
           prev.event = dplyr::lag(event_type),
           prev.time = dplyr::lag(game_seconds),
           prev.length = dplyr::lag(penalty_length))

  away_pen_mat <- apply(away_state_changes,
                        1,
                        FUN = function(x) {
                          if(x[1] == 1 & x[2]+x[3]*60 < (max(play_prep$period)*1200-1)){
                            c( rep( 0, length( 0:x[2] )),
                               rep( -1, x[3]*60),
                               rep(0, length((x[2]+x[3]*60 + 1):(max(play_prep$period)*1200-1)))
                            )
                          } else if(x[1] == 1 & x[2]+x[3]*60 >= (max(play_prep$period)*1200-1)) {
                            c( rep( 0, length( 0:x[2] )),
                               rep(-1, max(play_prep$period)*1200-1-x[2] )
                            )
                          } else if( x[1] == 2 & x[2] %in% ifelse(!is.na(x[5]) & !is.na(x[6]) & x[2] != x[5], x[5]:(x[5]+x[6]*60),-1 ) ) {
                            c( rep( 0, length( 0:(x[2]) )),
                               rep( 1, length( (x[2]+1):(x[6]*60-(x[2]-x[5])))),
                               rep(0, length((x[6]*60-(x[2]-x[5])+1):(max(play_prep$period)*1200-1)))
                            )
                          } else {
                            rep(0, length(0:(max(play_prep$period)*1200-1)))
                          }
                        })

  away_skaters <- 5 + apply(away_pen_mat, 1, sum)

  #----

  #Adds skater strength to pbp data
  play_df <- play_prep %>%
    dplyr::left_join(data.frame(game_seconds = 0:(max(play_prep$period)*1200-1),
              home_skaters = home_skaters,
              away_skaters = away_skaters),
              by = "game_seconds")

  #deals with extra skater for pulled goalie
  play_df$home_skaters <- ifelse(play_df$home_goalie == "", play_df$home_skaters+1, play_df$home_skaters)
  play_df$away_skaters <- ifelse(play_df$away_goalie == "", play_df$away_skaters+1, play_df$away_skaters)
  #deals with more than 2 penalties
  play_df$home_skaters <- ifelse(play_df$home_skaters < 3,3, play_df$home_skaters)
  play_df$away_skaters <- ifelse(play_df$away_skaters < 3,3, play_df$away_skaters)

  #Converts player ids to player names
  play_df[,c(17:19,26:39)] <-
    play_df[,c(17:19,26:39)] %>% sapply(convert_ids, player_df = players)

  #final sorting of columns and removing unnecessary rows
  play_df <- play_df %>%
    dplyr::select(-penalty_length) %>%
    dplyr::select(Season, game_id:away_team, home_score, away_score, play_index:event_type,
           event_team:event_player_3, event_detail:y_coord,
           home_skaters, away_skaters, event_angle:minus_player_6) %>%
    dplyr::filter(event_type != '')

  #Deals with weird date issues
  date <- play_df$game_date[1]
  play_df$game_date <- date

  print("    Finished")
  return(play_df)
}

#' Play by Play Scraper
#'
#' This function returns a data frame of the play by play in a tidy format
#' @param game_ids character or numeric vector of unique game ids
#' @keywords play by play
#' @export
#' @import dplyr
#' @examples
#' get_play_by_play(c("22207010","22207013"))
get_play_by_play <- function(game_ids) {
  game_list <- lapply(game_ids, complete_game_scrape)
  game_list <- game_list[which(!is.na(game_list))]
  game_data <- do.call("bindingFunction", game_list)
  return(game_data)
}

#' Schedule ID Scraper
#'
#' This function returns a vector of game ids given a prespecified team and season
#' @param Season takes in the season as year1year2 (yyyyyyyy), Defaults to 20182019
#' @param teams takes a specific team abbreviation, such as "BUF". Defaults to all teams
#' @keywords schedule
#' @export
#' @import dplyr
#' @examples
#' get_id_schedule("20182019", "BUF")
get_id_schedule <- function(Season = "20182019", teams = NA){
  all_teams <- c("BOS", "BUF","CTW","MET", "MIN")
  if(is.na(teams)){
    teams <- all_teams
  }
  if(is.na(Season)){
    print("Please Enter Season")
    return(NA)
  }
  Season <- as.character(Season)

  #team ids change every season
  the1819ids <- c(3629910:3629913, 3629918)
  the1718ids <- 2840761:2840764
  the1617ids <- c(2150454, 2150455, 2150457, 2150459)
  the1516ids <- 2150711:2150714

  #Gets relevant team ids
  team_ids <- if(Season == "20152016") {
    the1516ids[match(teams,all_teams)]
  } else if(Season == "20162017") {
    the1617ids[match(teams,all_teams)]
  } else if(Season == "20172018"){
    the1718ids[match(teams,all_teams)]
  } else{
    the1819ids[match(teams,all_teams)]
  }

  #Gets season id
  seasonid <- as.character(seasons$id[which(seasons$Season == Season)])

  #Creates urls for each team schedule page
  team_urls <- paste("https://www.nwhl.zone/schedule/team_instance/",
                     team_ids[which(!is.na(team_ids))],
                     "?subseason=",
                     seasonid,
                     sep = "")

  # Creates empty vector
  all_games <- c()
  #Iterates through each team
  for(i in team_urls){
    #Pulls urls for each game
    html <- paste(readLines(i), collapse="\n")
    game_ids <- stringr::str_extract_all(html,"(?<=[/])\\d{8}(?=[?])")
    #adds to vector
    all_games <- c(all_games,game_ids[[1]])
  }
  #since duplicate games
  all_games <- unique(all_games)

  return(all_games)
}

#' Date ID Scraper
#'
#' This function returns a vector of game ids given a specific date
#' @param Season takes in the season as year1year2 (yyyyyyyy)
#' @param Year takes in a specific year formatted as yyyy
#' @param Month takes in a specifc month formatted as mm
#' @param Day takes in a specific day formatted as dd
#' @keywords date
#' @import dplyr
#' @export
#' @examples
#' get_id_date(Season = "20182019", Year = "2018, Month = "12", Day = "09")
get_id_date <- function(Season = NA, Year = NA, Month = NA, Day = NA){
  #Gets season id
  seasonid <- as.character(seasons$id[which(seasons$Season == Season)])
  leagueid <- as.character(seasons$League[which(seasons$Season == Season)])

  # Gets daily schedule
  url <- paste("https://www.nwhl.zone/schedule/day/league_instance/",
      leagueid,
      "/",
      Year,
      "/",
      Month,
      "/",
      Day,
      "?subseason=", seasonid, sep = "")

  #Iterates through each team
  html <- paste(readLines(url), collapse="\n")
  game_ids <- stringr::str_extract_all(html,"(?<=[/])\\d{8}(?=[?])")

  return(game_ids[[1]])
}

game_summary <- function(pbp_df){
  # Pass in play by play dataframe for individual game and returns player summaries for game
  # Thanks to @EvolvingWild for providing their NHL game summary as a baseline
  season <- dplyr::first(pbp_df$Season)
  game_id <- dplyr::first(pbp_df$game_id)
  date <- dplyr::first(pbp_df$game_date)
  home <- dplyr::first(pbp_df$home_team)
  away <- dplyr::first(pbp_df$away_team)

  print(game_id)
  team_ids <- dplyr::select(get_team_info(game_id = game_id),
                     roster_id,
                     abbrev)

  roster <- get_roster_info(game_id = game_id) %>%
    dplyr::filter(roster_type == "player") %>%
    dplyr::select(first_name, last_name, position, roster_id) %>%
    dplyr::mutate(Player = paste(first_name,last_name)) %>%
    dplyr::select(-first_name, -last_name) %>%
    dplyr::left_join(team_ids, by = c("roster_id")) %>%
    dplyr::select(-roster_id)

  pbp_player_1 <- pbp_df %>%
    dplyr::group_by(Season, game_id, game_date, home_team, away_team, event_player_1, event_team) %>%
    dplyr::summarise(G = sum(event_type == "Goal"),
              SOG = sum(event_type == "Shot" | event_type == "Goal"),
              FOW = sum(event_type == "Faceoff"),
              Blk = sum(event_type == "BlockedShot"),
              TO = sum(event_type == "Turnover"),
              PEN = sum(event_type == "Penalty"),
              PIM = sum(convert_numeric(substr(event_detail,1,1)), na.rm = T),
              PPG = sum(event_type == "Goal" & ((event_team == home_team & away_skaters < 5) | (event_team == away_team & home_skaters < 5))),
              SHG = sum(event_type == "Goal" & ((event_team == home_team & home_skaters < 5) | (event_team == away_team & away_skaters < 5)))
              ) %>%
    dplyr::rename(Player = event_player_1)

  pbp_player_2 <- pbp_df %>%
    dplyr::group_by(Season, game_id, game_date, home_team, away_team, event_player_2, event_team) %>%
    dplyr::summarise(A1 = sum(event_type == "Goal"),
              PPA1 = sum(event_type == "Goal" & ((event_team == home_team & away_skaters < 5) | (event_team == away_team & home_skaters < 5))),
              SHA1 = sum(event_type == "Goal" & ((event_team == home_team & home_skaters < 5) | (event_team == away_team & away_skaters < 5)))
    ) %>%
    dplyr::filter(!(A1 == 0 & PPA1 == 0 & SHA1 == 0)) %>%
    dplyr::rename(Player = event_player_2)

  pbp_player_flipped <- pbp_df %>%
    dplyr::group_by(Season, game_id, game_date, home_team, away_team, event_player_2, event_team) %>%
    dplyr::summarise(FOL = sum(event_type == "Faceoff"),
              SV = sum(event_type == "Shot")) %>%
    dplyr::filter(FOL > 0 | SV > 0) %>%
    dplyr::mutate(event_team = ifelse(event_team == home_team, away_team,home_team)) %>%
    dplyr::rename(Player = event_player_2)

  pbp_player_3 <- pbp_df %>%
    dplyr::group_by(Season, game_id, game_date, home_team, away_team, event_player_3, event_team) %>%
    dplyr::summarise(A2 = sum(event_type == "Goal"),
              PPA2 = sum(event_type == "Goal" & ((event_team == home_team & away_skaters < 5) | (event_team == away_team & home_skaters < 5))),
              SHA2 = sum(event_type == "Goal" & ((event_team == home_team & home_skaters < 5) | (event_team == away_team & away_skaters < 5)))
    ) %>%
    dplyr::rename(Player = event_player_3)

  pbp_h_goalie <- pbp_df %>%
    dplyr::group_by(Season, game_id, game_date, home_team, away_team, event_team, home_goalie) %>%
    dplyr::summarise(GA = sum(event_type == "Goal" & event_team == away_team)) %>%
    dplyr::rename(Player = home_goalie) %>%
    dplyr::ungroup() %>%
    dplyr::filter(away_team == event_team) %>%
    dplyr::mutate(event_team = home_team)

  pbp_a_goalie <- pbp_df %>%
    dplyr::group_by(Season, game_id, game_date, home_team, away_team, event_team, away_goalie) %>%
    dplyr::summarise(GA = sum(event_type == "Goal" & event_team == home_team)) %>%
    dplyr::rename(Player = away_goalie) %>%
    dplyr::ungroup() %>%
    dplyr::filter(home_team == event_team) %>%
    dplyr::mutate(event_team = away_team)


  pbp_plus_1 <- pbp_df %>% dplyr::select(event_team, plus_player_1) %>% dplyr::rename(Player = plus_player_1)
  pbp_plus_2 <- pbp_df %>% dplyr::select(event_team, plus_player_2) %>% dplyr::rename(Player = plus_player_2)
  pbp_plus_3 <- pbp_df %>% dplyr::select(event_team, plus_player_3) %>% dplyr::rename(Player = plus_player_3)
  pbp_plus_4 <- pbp_df %>% dplyr::select(event_team, plus_player_4) %>% dplyr::rename(Player = plus_player_4)
  pbp_plus_5 <- pbp_df %>% dplyr::select(event_team, plus_player_5) %>% dplyr::rename(Player = plus_player_5)
  pbp_plus_6 <- pbp_df %>% dplyr::select(event_team, plus_player_6) %>% dplyr::rename(Player = plus_player_6)
  pbp_plus <- do.call("rbind",list(pbp_plus_1,pbp_plus_2,pbp_plus_3,pbp_plus_4,pbp_plus_5, pbp_plus_6)) %>%
    dplyr::group_by(event_team,Player) %>%
    dplyr::summarise(Plus = dplyr::n()) %>%
    dplyr::filter(!is.na(Player))

  pbp_minus_1 <- pbp_df %>% dplyr::mutate(event_team = ifelse(event_team == home_team, away_team, home_team)) %>%
    dplyr::select(event_team, minus_player_1) %>% dplyr::rename(Player = minus_player_1)
  pbp_minus_2 <- pbp_df %>% dplyr::mutate(event_team = ifelse(event_team == home_team, away_team, home_team)) %>%
    dplyr::select(event_team, minus_player_2) %>% dplyr::rename(Player = minus_player_2)
  pbp_minus_3 <- pbp_df %>% dplyr::mutate(event_team = ifelse(event_team == home_team, away_team, home_team)) %>%
    dplyr::select(event_team, minus_player_3) %>% dplyr::rename(Player = minus_player_3)
  pbp_minus_4 <- pbp_df %>% dplyr::mutate(event_team = ifelse(event_team == home_team, away_team, home_team)) %>%
    dplyr::select(event_team, minus_player_4) %>% dplyr::rename(Player = minus_player_4)
  pbp_minus_5 <- pbp_df %>% dplyr::mutate(event_team = ifelse(event_team == home_team, away_team, home_team)) %>%
    dplyr::select(event_team, minus_player_5) %>% dplyr::rename(Player = minus_player_5)
  pbp_minus_6 <- pbp_df %>% dplyr::mutate(event_team = ifelse(event_team == home_team, away_team, home_team)) %>%
    dplyr::select(event_team, minus_player_6) %>% dplyr::rename(Player = minus_player_6)
  pbp_minus <- do.call("rbind",list(pbp_minus_1,pbp_minus_2,pbp_minus_3,pbp_minus_4,pbp_minus_5, pbp_minus_6)) %>%
    dplyr::group_by(event_team,Player) %>%
    dplyr::summarise(Minus = n()) %>%
    dplyr::filter(!is.na(Player))

  player_data <- pbp_player_1 %>%
    dplyr::bind_rows(pbp_player_2) %>%
    dplyr::bind_rows(pbp_player_3) %>%
    dplyr::bind_rows(pbp_player_flipped) %>%
    dplyr::bind_rows(pbp_h_goalie) %>%
    dplyr::bind_rows(pbp_a_goalie) %>%
    dplyr::bind_rows(pbp_plus) %>%
    dplyr::bind_rows(pbp_minus) %>%
    dplyr::left_join(roster, by = c("Player","event_team"="abbrev")) %>%
    dplyr::rename(Team = event_team) %>%
    dplyr::filter(!is.na(Player))

  player_data <- player_data %>%
    dplyr::group_by(Player,Team, position) %>%
    dplyr::summarise_if(is.numeric, sum, na.rm = T)

  player_data$Season <- season
  player_data$game_id <- game_id
  player_data$game_date <- date
  player_data$home_team <- home
  player_data$away_team <- away

  player_data <- dplyr::mutate(player_data,
                        GS = ifelse(position == "G",
                                            0.14*SV - GA,
                                            G + 0.64*(A1+A2) + 0.11*SOG + 0.12*(FOW-FOL) - 0.17*(PIM/2)),
                        PTS = G+A1+A2,
                        PrPTS = G+A1,
                        GF. = ifelse((Plus+Minus) != 0 ,Plus/(Plus+Minus),NA_integer_)) %>%
    dplyr::rename(eGF = Plus, eGA = Minus)

  player_data <- dplyr::select(player_data,
                        Season, Player,Team,position,game_id:away_team,
                        G,A1,A2,PTS,PrPTS,GS,SOG,eGF,eGA,GF.,PPG,PPA1,PPA2,SHG,SHA1,SHA2,FOW,FOL,PIM,Blk,TO,SV,GA)

  return(player_data)
}

#' Player Summary Function
#'
#' Given a tidy play by play file from get_play_by_play function, summarizes player stats at a game level
#' @param pbp_df a dataframe consisting of play by play data from get_play_by_play function
#' @export
#' @import dplyr
#' @keywords player
get_player_summary <- function(pbp_df){
  player_games <- pbp_df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(game_id = as.character(game_id)) %>%
    dplyr::group_by(game_id) %>%
    dplyr::do(data.frame(game_summary(.)))
  return(player_games)
}

game_team_summary <- function(pbp_df){
  season <- dplyr::first(pbp_df$Season)
  game_id <- dplyr::first(pbp_df$game_id)
  date <- dplyr::first(pbp_df$game_date)
  home <- dplyr::first(pbp_df$home_team)
  away <- dplyr::first(pbp_df$away_team)

  print(game_id)

  pbp_stats_home <- pbp_df %>%
    dplyr::group_by(Season, game_id, game_date, home_team, away_team) %>%
    dplyr::summarise(GF = sum(event_type == "Goal" & event_team == home_team),
              GA = sum(event_type == "Goal" & event_team == away_team),
              SF = sum(event_type == "Shot" & event_team == home_team),
              SA = sum(event_type == "Shot" & event_team == away_team),
              SF_5v5 = sum(event_type == "Shot" & event_team == home_team & home_skaters == 5 & 5 == away_skaters),
              SA_5v5 = sum(event_type == "Shot" & event_team == away_team & home_skaters == 5 & 5 == away_skaters),
              BlkF = sum(event_type == "BlockedShot" & event_team == home_team),
              BlkA = sum(event_type == "BlockedShot" & event_team == away_team),
              GF_5v5 = sum(event_type == "Goal" & event_team == home_team & home_skaters == 5 & 5 == away_skaters),
              GA_5v5 = sum(event_type == "Goal" & event_team == away_team & home_skaters == 5 & 5 == away_skaters),
              GF_pp = sum(event_type == "Goal" & event_team == home_team & home_skaters > away_skaters),
              GA_pp = sum(event_type == "Goal" & event_team == away_team & home_skaters > away_skaters),
              GF_sh = sum(event_type == "Goal" & event_team == home_team & home_skaters < away_skaters),
              GA_sh = sum(event_type == "Goal" & event_team == away_team & home_skaters < away_skaters),
              FOW = sum(event_type == "Faceoff" & event_team == home_team),
              FOL = sum(event_type == "Faceoff" & event_team == away_team),
              periods = max(period)
    ) %>%
    dplyr::mutate('Sh' = round(GF/SF,2),
           'Sv' = round(1 - GA/SA,2),
           PDO = Sh + Sv,
           SF. = round(SF/(SF+SA),2),
           SF_5v5. = round(SF_5v5/(SF_5v5+SA_5v5),2),
           Team = home_team) %>%
    dplyr::rename('Sh%' = Sh,
           'Sv%' = Sv,
           'SF%' = SF.,
           'SF_5v5%' = SF_5v5.) %>%
    dplyr::select(Season:away_team, Team, GF:SA, 'SF%', SF_5v5, SA_5v5, 'SF_5v5%', BlkF:PDO)

  pbp_stats_away <- pbp_df %>%
    dplyr::group_by(Season, game_id, game_date, home_team, away_team) %>%
    dplyr::summarise(GF = sum(event_type == "Goal" & event_team == away_team),
              GA = sum(event_type == "Goal" & event_team == home_team),
              SF = sum(event_type == "Shot" & event_team == away_team),
              SA = sum(event_type == "Shot" & event_team == home_team),
              SF_5v5 = sum(event_type == "Shot" & event_team == away_team & home_skaters == 5 & 5 == away_skaters),
              SA_5v5 = sum(event_type == "Shot" & event_team == home_team & home_skaters == 5 & 5 == away_skaters),
              BlkF = sum(event_type == "BlockedShot" & event_team == away_team),
              BlkA = sum(event_type == "BlockedShot" & event_team == home_team),
              GF_5v5 = sum(event_type == "Goal" & event_team == away_team & home_skaters == 5 & 5 == away_skaters),
              GA_5v5 = sum(event_type == "Goal" & event_team == home_team & home_skaters == 5 & 5 == away_skaters),
              GF_pp = sum(event_type == "Goal" & event_team == away_team & home_skaters > away_skaters),
              GA_pp = sum(event_type == "Goal" & event_team == home_team & home_skaters > away_skaters),
              GF_sh = sum(event_type == "Goal" & event_team == away_team & home_skaters < away_skaters),
              GA_sh = sum(event_type == "Goal" & event_team == home_team & home_skaters < away_skaters),
              FOW = sum(event_type == "Faceoff" & event_team == away_team),
              FOL = sum(event_type == "Faceoff" & event_team == home_team),
              periods = max(period)
    ) %>%
    dplyr::mutate('Sh' = round(GF/SF,2),
           'Sv' = round(1 - GA/SA,2),
           PDO = Sh + Sv,
           SF. = round(SF/(SF+SA),2),
           SF_5v5. = round(SF_5v5/(SF_5v5+SA_5v5),2),
           Team = away_team) %>%
    dplyr::rename('Sh%' = Sh,
           'Sv%' = Sv,
           'SF%' = SF.,
           'SF_5v5%' = SF_5v5.) %>%
    dplyr::select(Season:away_team, Team, GF:SA, 'SF%', SF_5v5, SA_5v5, 'SF_5v5%', BlkF:PDO)

  pbp_team <- dplyr::bind_rows(pbp_stats_home, pbp_stats_away)
  return(pbp_team)
}

#' Team Summary Function
#'
#' Given a tidy play by play file from get_play_by_play function, summarizes team stats at a game level
#' @param pbp_df a dataframe consisting of play by play data from get_play_by_play function
#' @export
#' @import dplyr
#' @keywords team
get_team_summary <- function(pbp_df){
  team_games <- pbp_df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(game_id = as.character(game_id)) %>%
    dplyr::group_by(game_id) %>%
    dplyr::do(data.frame(game_team_summary(.)))
  return(team_games)
}
