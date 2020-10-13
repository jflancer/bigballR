#' Individual Game Play-By-Play Scraper
#'
#' This function retrieves and cleans play-by-play information for an individual game.
#' Warns users of potential errors and mistakes made by the game trackers. The number of player discrepancies warning
#' counts displays the number of events players committed when it is found they were not on the court at the time of the
#' event. The substitution mistake warning indicates an unclean substitution was entered. (ex. 2 players enter and 1 leaves)
#' @param  game_id string made up of digits given to each unique game. This can be found in the play-by-play url for each game.
#' @param use_file Boolean. If true, read from html file rather than url. File path constructed from `base_path`
#' @param save_file Boolean. If true, save html from url to file. File path constructed from `base_path`
#' @param base_path String. Specify base location of html file save, ex. "/Users/jake/html_files/"
#' @param overwrite Boolean. If true, save file will overwrite an existing file at same path. Otherwise will read from existing file
#' @import dplyr
#' @importFrom XML readHTMLTable
#' @export
#' @return data frame containing play-by-play data for a game, where each row represents an individual event from the game.
#' \itemize{
#' \item{ID} - Numeric game id that is given for each unique game
#' \item{Date} - Game date
#' \item{Home} - Home team name
#' \item{Away} - Away team name
#' \item{Time} - String for game time in format reported originally by NCAA
#' \item{Game_Time} - String reporting game time elapsed. For example, a normal game starts at 00:00 and ends at 40:00
#' \item{Game_Seconds} - Number of seconds elapsed in the game
#' \item{Half_Status} - Number indicating which half: 1,2 for regulation, 3+ for OTs
#' \item{Home_Score} - Score for home team after event occurred as reported by NCAA
#' \item{Away_Score} - Score for away team after event occurred as reported by NCAA
#' \item{Event_Team} - Which team was repsonsibile for the play-by-play entry
#' \item{Event_Description} - The text description of the event on the NCAA site
#' \item{Player_1} - The primary player responsible for the event
#' \item{Player_2} - The secondary player within an event. As of now this is only the assister on a made shot.
#' \item{Event_Type} - String representing the event that occurred as reported by the description
#' \item{Event_Result} - Takes values of made/missed for shot attempts, otherwise NA
#' \item{Shot_Value} - Numeric value of points awarded by shot type. Ranges from 1-3 for shot attempts, otherwise NA
#' \item{Event_Length} - Estimate of time before events calculated from event time - last previous event time
#' \item{Home.1} - One of the players on the court for the home team
#' \item{Home.2} - One of the players on the court for the home team
#' \item{Home.3} - One of the players on the court for the home team
#' \item{Home.4} - One of the players on the court for the home team
#' \item{Home.5} - One of the players on the court for the home team
#' \item{Away.1} - One of the players on the court for the away team
#' \item{Away.2} - One of the players on the court for the away team
#' \item{Away.3} - One of the players on the court for the away team
#' \item{Away.4} - One of the players on the court for the away team
#' \item{Away.5} - One of the players on the court for the away team
#' \item{Status} - This reports the cleanliness of the data frame. Will be CLEAN if no errors are found. Otherwise will say
#' the number of errors that occurred or a potential substitution mistake occurred.
#' }
#' @examples
#' scrape_game(4674164)
scrape_game <- function(game_id, save_file=F, use_file=F, base_path = NA, overwrite=F) {
  #track status of cleanliness of data for game
  status <- "CLEAN"

  base_url <- "http://stats.ncaa.org/game/play_by_play/"
  url <- paste0(base_url, game_id)
  file_dir <- paste0(base_path, "play_by_play/")
  file_path <- paste0(file_dir, game_id, ".html")
  isUrlRead <- F

  # Give user option to save raw html file (to make future processing more efficient)
  if (save_file & !is.na(base_path) & (!file.exists(file_path) | overwrite)) {
    isUrlRead <- T
    html <- readLines(url)
    dir.create(file_dir, recursive = T, showWarnings = F)
    writeLines(html, file_path)
  } else if (file.exists(file_path)) {
    html <- readLines(file_path)
  }

  if (use_file & !is.na(base_path)) {
    table <- XML::readHTMLTable(file_path)
  } else {
    isUrlRead <- T
    table <- XML::readHTMLTable(url)
  }

  if (length(table) == 0) {
    message("Game Not Found")
    return(data.frame())
  }

  # Pull scores for each half
  half_scores <- table[[1]]

  # Get the data frames for the regulation portion
  first_half <- table[[6]] %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(Half_Status = 1)
  second_half <- table[[8]] %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(Half_Status = 2)
  game <- dplyr::bind_rows(first_half, second_half)

  # Check if overtime period(s) exist
  if (ncol(half_scores) == 4) {
    numbOTs <- 0
  } else{
    # Iterate through overtimes and add to game data frame
    numbOTs <- length(half_scores) - 4
    for (i in 1:numbOTs) {
      ot_data <- table[[8 + i * 2]]  %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::mutate(Half_Status = 2 + i)
      game <- dplyr::bind_rows(game, ot_data)
    }
  }

  # Essentially, there are two different codes/systems used by the NCAA to track games
  # This makes the content completely different and needs to be adjusted for
  # 'V2' is dubbed version 2, as it has more detail and began usage more recently
  # Can find the format by looking at the first entries as they are constant and unique to each version
  # As V1 is older and uses less detail, we will format the data in accordance with V1
  format <-
    if (((first_half[1, 1] == "20:00:00") &
        (first_half[1, 2] == "game start" |
         first_half[1, 2] == "period start")) |
        any(grepl("commercial",game[,2]))) {
      "V2"
    } else{
      "V1"
    }

  # Removes unneccesary extraneous rows that may arise
  game <- dplyr::filter(game, !is.na(Score))

  # Get the game metadata
  meta <- table[[3]]

  # Get Game Date- removing start time because I don't really see a use in play by play
  datetime <- colnames(meta)[2]
  date <- substr(datetime, 1, 10)
  date <- ifelse(is.null(date),"",date)

  # Get the teams playing
  away_team <- colnames(game)[2]
  home_team <- colnames(game)[4]

  # Get game time- formatted as 20:00 counting down to 0 for each half
  time <- substr(game[, 1], 1, 5)

  # Convert game time to seconds- goes from 0 at start to 2400+ at end of game
  time_in_seconds <-
    unlist(lapply(strsplit(time, ":"), function(x) {
      if (nchar(x[1]) == 2) {
        (as.numeric(x[1]) * 60 + as.numeric(x[2]))
      } else{
        NA
      }
    }))

  game_time <- ifelse(game[, 5] < 2,
                      1200 + 1200 * (game[, 5] - 1),
                      2400 + 300 * (game[, 5] - 2)) - time_in_seconds

  # Differently formatted time that goes from 0 at game start to 40:00+ at end of game
  mins <- game_time %/% 60
  mins <- ifelse(mins < 10, paste0("0", mins), as.character(mins))
  secs <- game_time %% 60
  secs <- ifelse(secs < 10, paste0("0", secs), as.character(secs))
  game_display <- paste0(mins, ":", secs)

  # Separates score column into a separated home and away score
  scores <- strsplit(game[, 3], "-")
  home_score <- unlist(lapply(scores, function(x) {
    x[2]
  }))
  away_score <- unlist(lapply(scores, function(x) {
    x[1]
  }))

  # Data begins formatted with away team events in one column and home team events in another
  # With the formatting in use, there can never be a home and away event at the same time
  # This allows the home and away events to be merged into an events column
  player1_h <- game[, 4]
  player1_a <- game[, 2]
  player1 <- player1_h
  player1[which(player1 == "" | is.na(player1))] <- player1_a[which(player1_a != "")]

  events <- player1
  #Pulls the event team by looking at which entry side it comes from
  event_team <- ifelse(player1_h == player1, home_team, away_team)

  # Handle PBP Version ####
  # Here is where the data is converted to a standard code
  if (format == "V2") {
    # Version 2 Cleaning

    # Uses: player name, event
    event_split <- strsplit(events, ",")

    event_split <- lapply(event_split, function(x){
      if(length(x) == 3){
        temp <- x
        temp[1] <- paste0(temp[1], temp[2])
        temp[2] <- temp[3]
        temp <- temp[1:2]
        return(temp)
      } else{
        return(x)
      }
    })

    # This pulls player names from left of comma
    players <-
      unlist(lapply(event_split, function(x) {
        x[1]
      }))
    # Converts to formatting of names used in V1 which is FIRST.LAST
    players <- gsub("[^[:alnum:] ]", "", players)
    player_name <- gsub("\\s+", ".", toupper(players))
    # Remove any notation of JR/SR/II/III from player name
    player_name <- gsub("\\.JR\\.|\\.SR\\.|\\.J\\.R\\.|\\.JR\\.|JR\\.|SR\\.|\\.SR|\\.JR|\\.SR|JR|SR|\\.III|III|\\.II|II","", player_name)
    player_name <- trimws(player_name)

    # Now getting events from left of comma
    events <-
      unlist(lapply(event_split, function(x) {
        x[2]
      }))

    # Call function that converts the formatting of events from V2 to V1
    events <- convert_events(events)

  } else {
    # Version 1 Cleaning

    # Follows format LAST,FIRST event_description
    event_split <- strsplit(events, " ")

    # Find the all uppercase words to get player names
    players <-
      unlist(lapply(event_split, function(x) {
        paste(x[which(x == toupper(x) | x == "Team,")], collapse = " ")
      }))

    # Take all other words as a component of the event
    events <-
      unlist(lapply(event_split, function(x) {
        paste(x[which(x != toupper(x) & x != "Team,")], collapse = " ")
      }))

    # Clean the player names into a proper format
    clean_players <- strsplit(players, ",")
    first <- unlist(lapply(clean_players, function(x) {
      x[2]
    }))
    last <- unlist(lapply(clean_players, function(x) {
      x[1]
    }))
    first <- ifelse(is.na(first), "", first)
    first <- gsub("[^[:alnum:] ]", "", first)
    last <- gsub("[^[:alnum:] ]", "", last)

    player_name <- paste0(first, ".", last)
    player_name <-
      ifelse(substr(player_name, 1, 1) == ".", "TEAM", player_name)
    player_name <- gsub("\\.JR\\.|\\.SR\\.|\\.J\\.R\\.|\\.JR\\.|JR\\.|SR\\.|\\.SR|\\.JR|\\.SR|JR|SR|\\.III|III|\\.II|II","", player_name)
  }

  # Now cleaning can ignore version

  # Separate event into first word and rest of word
  first_word <-
    unlist(lapply(strsplit(events, " "), function(x) {
      x[1]
    }))

  remaining <- unlist(lapply(strsplit(events, " "),
                             function(x) {
                               paste(x[2:length(x)], collapse = " ")
                             }))

  # Shots are preceded by "made" or "missed", this strips that from the event type
  event_type <-
    ifelse(first_word %in% c("made", "missed"), remaining, events)

  # This pulls the event result only in the case of shots
  event_result <-
    ifelse(first_word %in% c("made", "missed"),
           first_word,
           NA_character_)

  # Now put together created variables into first data frame
  dirty_game <- data.frame(
    ID = game_id,
    Date = date,
    Home = home_team,
    Away = away_team,
    Time = time,
    Game_Time = game_display,
    Game_Seconds = game_time,
    Half_Status = game$Half_Status,
    Home_Score = home_score,
    Away_Score = away_score,
    Event_Team = event_team,
    Player_1 = player_name,
    Event_Type = event_type,
    Event_Result = event_result,
    Event_Description = player1,
    stringsAsFactors = F
  )  %>%
    # Function is in use as scorekeepers often don't follow same ordering
    # This formats as Row 1: Shot, Row 2: Assist, Next: Substitutions, Final: Everything Else
    dplyr::group_by(Game_Seconds, Home_Score, Away_Score) %>%
    dplyr::do(order_seconds(.)) %>%
    dplyr::ungroup() %>%
    # Data formats assists independently of shots in following row
    # This makes it easier to use data but combining shot+assist into one row with a player_2 column for the assist player
    dplyr::mutate(
      Player_2 = ifelse(lead(Event_Type) == "Assist", lead(Player_1), NA_character_),
      Event_Description = ifelse(
        lead(Event_Type) == "Assist" &
          !is.na(lead(Event_Type)),
        paste(Event_Description, "-", lead(Event_Description)),
        Event_Description
      )
    ) %>%
    dplyr::filter(Event_Type != "Assist")

  # Now Check to See if Players Were Recorded in the Game
  if (length(unique(dirty_game$Player_1)) == 1) {

    # No Player Cleaning ####
    # Found no player names in data
    # Does final cleaning of data without finding on off
    # Gets the length of each event and assigns a shot value
    mild_game <- dirty_game %>%
      dplyr::mutate(Event_Length = Game_Seconds - dplyr::lag(Game_Seconds))
    mild_game$Event_Length[1] <- mild_game$Game_Seconds[1]

    # Creates the final dataframe to be used without players
    clean_game <- mild_game %>%
      dplyr::mutate(
        Home_Score = as.numeric(Home_Score),
        Away_Score = as.numeric(Away_Score),
        Shot_Value = dplyr::case_when(
          Event_Type == "Two Point Jumper" ~ 2,
          Event_Type == "Layup" ~ 2,
          Event_Type == "Three Point Jumper" ~ 3,
          Event_Type == "Dunk" ~ 2,
          Event_Type == "Hook" ~ 2,
          Event_Type == "Free Throw" ~ 1,
          Event_Type == "Tip In" ~ 2
        ),
        Status = "NO_PLAYER", #set status variable mentioned earlier
        Sub_Deviate = nrow(.)
      ) %>%
      bind_cols(as.data.frame(matrix(rep("NO_PLAYER", nrow(mild_game)*10),
                       ncol = 10,
                       nrow = nrow(mild_game))) %>%
                  rename(Home.1 = V1, Home.2 = V2, Home.3 = V3, Home.4 = V4, Home.5 = V5,
                         Away.1 = V6, Away.2 = V7, Away.3 = V8, Away.4 = V9, Away.5 = V10
                         )) %>%
      dplyr::select(
        ID:Away,
        Half_Status,
        Time:Event_Team,
        Event_Description,
        Player_1,
        Player_2,
        Event_Type,
        Event_Result,
        Shot_Value,
        Event_Length,
        Home.1:Away.5,
        Status,
        Sub_Deviate
      )

    # Report results
    # Include game info, that pbp was missing players, and which format was used
    message(paste(
      date,
      home_team,
      "v",
      away_team,
      "| Status: No Players",
      format,
      "|",
      game_id
    ))

    return(clean_game)
  } else {
    # Search for extraneous error substitutions
    # All substitutions events must have two concurring rows, an entry and an exit.
    # Find errors when an odd number of substitutions occur together
    mins_errors <- dirty_game %>%
      dplyr::filter(Event_Type %in% c("Leaves Game", "Enters Game"),
             Game_Seconds != 1200) %>%
      dplyr::group_by(Game_Seconds) %>%
      dplyr::summarise(count = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(count %% 2 != 0)
    # Report substitition mistake messages to users
    # Possibly find a way to deal with this but for now just reporting
    if (nrow(mins_errors) > 0) {
      message(
        paste(
          "Potential Substitution Entry Mistakes within: Half",
          # FIX THIS TO JUST SHOW HALF NOT SPECIFIC TIME
          paste(unique(mins_errors$Game_Seconds %/% 1200)+1, collapse = ", ")
        )
      )
      # Changes the status variable to note a sub mistake was made
      status <- "SUB_MISTAKE"
    }

    # On Court ####

    # Create empty matrix variables to store on court
    home_player_matrix <- NA
    away_player_matrix <- NA

    # Since starters aren't identified for each half, need to go through process to find them
    for (i in 1:(2 + numbOTs)) {
      #Iterates through the number of game session
      half_data <- dplyr::filter(dirty_game, Half_Status == i)

      #Get vectors for players leaving and entering game for both teams
      home_leaving <-
        dplyr::filter(half_data,
                      Event_Team == Home,
                      Event_Type == "Leaves Game",
                      Player_1 != "TEAM",
                      Time != "00:00",
                      (Time != "20:00" & Half_Status %in% 1:2) | (Time != "05:00" & Half_Status >2)
                      )$Player_1
      home_entering <-
        dplyr::filter(half_data,
                      Event_Team == Home,
                      Event_Type == "Enters Game",
                      Player_1 != "TEAM",
                      Time != "00:00",
                      (Time != "20:00" & Half_Status %in% 1:2) | (Time != "05:00" & Half_Status >2)
                      )$Player_1
      away_leaving <-
        dplyr::filter(half_data,
                      Event_Team == Away,
                      Event_Type == "Leaves Game",
                      Player_1 != "TEAM",
                      Time != "00:00",
                      (Time != "20:00" & Half_Status %in% 1:2) | (Time != "05:00" & Half_Status >2)
                      )$Player_1
      away_entering <-
        dplyr::filter(half_data,
                      Event_Team == Away,
                      Event_Type == "Enters Game",
                      Player_1 != "TEAM",
                      (Time != "20:00" & Half_Status %in% 1:2) | (Time != "05:00" & Half_Status >2)
                      )$Player_1

      # Find players explicitly defined as starting
      true_home_starters <- (half_data %>%
        dplyr::filter(Home == Event_Team,
               (Time == "20:00" & Half_Status %in% 1:2) | (Time == "05:00" & Half_Status >2),
               Time != "00:00",
               Event_Type == "Enters Game"
               ))$Player_1

      # Figure out starters by finding players that have a "Leaves Game" entry before an "Enters Game" entry
      if (length(home_leaving) > 0) {
        home_starters <- c()
        for (j in 1:length(home_leaving)) {
          if (!home_leaving[j] %in% home_entering[1:(j - 1)] &
              !home_leaving[j] %in% home_leaving[1:j - 1]) {
            home_starters <- c(home_starters, home_leaving[j])
          }
        }
      }

      # Find players explicitly defined as beginning from bench
      true_home_nonstarters <- (half_data %>%
                               dplyr::filter(Home == Event_Team,
                                             (Time == "20:00" & Half_Status %in% 1:2) | (Time == "05:00" & Half_Status >2),
                                             Event_Type == "Leaves Game"
                               ))$Player_1

      # Ignore if a player subs on for themselves at the beginning of a half
      temp_swap <- true_home_starters
      true_home_starters <- true_home_starters[which(!true_home_starters %in% true_home_nonstarters)]
      true_home_nonstarters <- true_home_nonstarters[which(!true_home_nonstarters %in% temp_swap)]

      # Remove defined non-starters from starters
      home_starters <- home_starters[which(!home_starters %in% true_home_nonstarters)]
      true_home_starters <- true_home_starters[which(!true_home_starters %in% home_starters)]
      home_starters <- c(home_starters, true_home_starters)

      # To handle data entry errors, often 5 starters cannot be found using proper method above
      home_starters <- if (length(home_starters) < 5) {

        # First remove any extraneous Player.1 entries dealing with team events
        # I think I've narrowed it down so only "TEAM" can show up in certain cases, but couldn't see a need to remove the others just in case
        home_split <-
          dplyr::filter(
            half_data,
            Event_Team == Home,!Player_1 %in% c(
              "TEAM.TEAM",
              "TEAM.TEAM 30",
              " TEAM.TEAM",
              "TEAM",
              "TEAM.TEAM ",
              "TEAM.TEAM 20"
            )
          )

        # Handle case if a player was subbed out for themselves and started the game
        # Has actually happened on several occassions
        error_catch <- c()
        for (time in unique(home_split$Game_Seconds)) {
          # Idk just ignoring late subs when game gets weird
          temp <- dplyr::filter(home_split, Game_Seconds == time, Game_Seconds < max(Game_Seconds)-60)

          ons <-
            temp$Player_1[which(temp$Event_Type == "Enters Game")]
          offs <-
            temp$Player_1[which(temp$Event_Type == "Leaves Game")]
          error_catch <- c(error_catch, ons[ons %in% offs])
        }
        error_catch <- error_catch[!error_catch %in% home_starters]

        # Looks for players that registered events but never subbed in/out, this implies they are a starter
        non_subs <-
          unique(home_split[which(!home_split$Player_1 %in% c(home_leaving, home_entering,home_starters,true_home_nonstarters)),]$Player_1)

        # See if player recorded an event before ever subbing out of the game
        play_before_sub <- home_split %>%
          group_by(Player_1) %>%
          filter(first(Game_Seconds) < first(.$Game_Seconds[which(.$Event_Type == "Leaves Game")])) %>%
          distinct(Player_1) %>%
          filter(!Player_1 %in% home_starters) %>%
          unlist(., use.names = F)

        all_starters <- unique(c(home_starters, non_subs, play_before_sub, error_catch))
        # all_starters <- all_starters[which(!all_starters %in% home_bench)]

        # If these methods find more than five starters, just chooses the first five found until a better way is suggested
        # Warn user that this is being used
        if (length(all_starters) > 5) {
          message(
            paste(
              "Using approximate starter finder, choosing:\n",
              paste(all_starters[1:5], collapse = ", "),
              "\nfrom: ",
              paste(all_starters, collapse = ", ")
            )
          )
          all_starters[1:5]
        # If 5, checks have successfully found 5 starters
        } else if(length(all_starters) == 5){
          all_starters[1:5]
        # Handle case when less than 5 starters are found even after error checks
        } else {
          # Just takes first n players that have recorded an event in the hal
          # all_found <- unique(c(home_starters, play_before_sub, non_subs, error_catch))

          all_half_players <- half_data %>%
            filter(Event_Team == Home,
                   !Event_Type %in% c("Enters Game", "Leaves Game"),
                   Player_1 != "TEAM",
                   !Player_1 %in% all_starters) %>%
            .$Player_1 %>%
            unlist() %>%
            unique() %>%
            .[1:(5-length(all_starters))]

          # If able to find 5 players, return them as starter and warn user
          if(length(c(all_starters, all_half_players)) == 5) {
            message("Warning In Substitution Data - Not Enough Starters Found. Using Estimate")
            c(all_starters, all_half_players)
          } else {
            all_starters[1:5]
          }
        }
      } else {
        home_starters[1:5]
      }

      # Attempting to guess on who is on the court when a player plays the entire half and doesn't register a stat
      # Best guess I could think of was look at the last player to record a stat in prior halfs
      if(any(is.na(home_starters))){
        numb.players <- sum(is.na(home_starters))
        half_using <- if(i ==1){2:(numbOTs+1)} else {1:i}
        prior_half <- filter(dirty_game,
                             Half_Status %in% half_using,
                             Event_Team == Home,
                             !Player_1 %in% c(home_starters,home_enter_players, home_leaving,"TEAM"),
                             !Event_Type %in% c("Enters Game"))
        players <- if(i == 1){
          unique(prior_half$Player_1, fromLast = T)[1:numb.players]
        } else {
             rev(unique(prior_half$Player_1, fromLast = T))[1:numb.players]
          }
        home_starters[is.na(home_starters)] <- players
        message(paste("5 starters not found for half",i, "choosing",players, collapse = "/"))
      }

      # Repeated process is done for the away team, refer to comments above
      true_away_starters <- (half_data %>%
                               dplyr::filter(Away == Event_Team,
                                             (Time == "20:00" & Half_Status %in% 1:2) | (Time == "05:00" & Half_Status >2),
                                             Time != "00:00",
                                             Event_Type == "Enters Game"
                               ))$Player_1

      if (length(away_leaving) > 0) {
        away_starters <- c()
        # away_bench <- c()
        for (j in 1:length(away_leaving)) {
          if (!away_leaving[j] %in% away_entering[1:(j - 1)]) {
            away_starters <- c(away_starters, away_leaving[j])
            # away_bench <- c(away_bench, away_entering[j])
          }
        }
      }

    true_away_nonstarters <- (half_data %>%
                                dplyr::filter(Away == Event_Team,
                                              (Time == "20:00" & Half_Status %in% 1:2) | (Time == "05:00" & Half_Status >2),
                                              Event_Type == "Leaves Game"
                                ))$Player_1

    temp_swap <- true_away_starters
    true_away_starters <- true_away_starters[which(!true_away_starters %in% true_away_nonstarters)]
    true_away_nonstarters <- true_away_nonstarters[which(!true_away_nonstarters %in% temp_swap)]

    away_starters <- away_starters[which(!away_starters %in% true_away_nonstarters)]
    true_away_starters <- true_away_starters[which(!true_away_starters %in% away_starters)]
    away_starters <- c(away_starters, true_away_starters)

    away_starters <- if (length(away_starters) < 5) {
        away_split <-
          dplyr::filter(
            half_data,
            Event_Team == Away,!Player_1 %in% c(
              "TEAM.TEAM",
              "TEAM.TEAM 30",
              " TEAM.TEAM",
              "TEAM",
              "TEAM.TEAM ",
              "TEAM.TEAM 20"
            )
          )
        error_catch <- c()
        for (time in unique(away_split$Game_Seconds)) {
          temp <- dplyr::filter(away_split, Game_Seconds == time, Game_Seconds < max(Game_Seconds)-60)
          ons <-
            temp$Player_1[which(temp$Event_Type == "Enters Game")]
          offs <-
            temp$Player_1[which(temp$Event_Type == "Leaves Game")]
          error_catch <- c(error_catch, ons[ons %in% offs])
        }
        error_catch <- error_catch[!error_catch %in% away_starters]

        non_subs <-
          unique(away_split[which(!away_split$Player_1 %in% c(away_leaving, away_entering,true_away_nonstarters,away_starters)),]$Player_1)

        # See if player recorded an event before subbing out at start
        play_before_sub <- away_split %>%
          group_by(Player_1) %>%
          filter(first(Game_Seconds) < first(.$Game_Seconds[which(.$Event_Type == "Leaves Game")])) %>%
          distinct(Player_1) %>%
          filter(!Player_1 %in% away_starters) %>%
          unlist(., use.names = F)

        all_starters <- unique(c(away_starters, non_subs, play_before_sub, error_catch))
        # all_starters <- all_starters[which(!all_starters %in% away_bench)]

        if (length(all_starters) > 5) {
          message(
            paste(
              "Using approximate starter finder, choosing:\n",
              paste(all_starters[1:5], collapse = ", "),
              "\nfrom: ",
              paste(all_starters, collapse = ", ")
            )
          )
          all_starters[1:5]
        } else if(length(all_starters) == 5){
          all_starters
        } else {
          # all_found <- unique(c(away_starters, play_before_sub, non_subs, error_catch))
          # Just takes first n players that have recorded an event in the half
          all_half_players <- half_data %>%
            filter(Event_Team == Away,
                   !Event_Type %in% c("Enters Game", "Leaves Game"),
                   Player_1 != "TEAM",
                   !Player_1 %in% all_starters) %>%
            .$Player_1 %>%
            unlist() %>%
            unique() %>%
            .[1:(5-length(all_starters))]

          # If able to find 5 players, return them as starter and warn user
          if(length(c(all_starters, all_half_players)) == 5) {
            message("Warning In Substitution Data - Not Enough Starters Found. Using Estimate")
            c(all_starters, all_half_players)
          } else {
            all_starters[1:5]
          }
        }
      } else {
          away_starters[1:5]
      }

      if(any(is.na(away_starters))){
        numb.players <- sum(is.na(away_starters))
        half_using <- if(i ==1){2:(numbOTs+1)} else {1:i}
        prior_half <- filter(dirty_game,
                             Half_Status %in% half_using,
                             Event_Team == Away,
                             !Player_1 %in% c(away_starters,away_entering,away_leaving,"TEAM"),
                             !Event_Type %in% c("Enters Game"))
        players <- if(i == 1){
          unique(prior_half$Player_1, fromLast = T)[1:numb.players]
        } else {
          rev(unique(prior_half$Player_1, fromLast = T))[1:numb.players]
        }
        away_starters[1:5][is.na(away_starters[1:5])] <- players
        message(paste("5 starters not found for half", i, "choosing", players, collapse = "/"))
      }

      # Now an empty matrix is built that will be iterated through to store the lineups
      # Repeat process is done for home and away team

      home_mat <- matrix(
          c(home_starters,home_starters,rep(NA_character_, nrow(half_data) * 5 - 5)),
          nrow = nrow(half_data) + 1, ncol = 5, byrow = T)

      away_mat <- matrix(
          c(away_starters,away_starters,rep(NA_character_, nrow(half_data) * 5 - 5)),
          nrow = nrow(half_data) + 1, ncol = 5, byrow = T)

      # Vectors of substitutes are used and diminished as events happen to track who is subbed
      home_exit_players <- if(length(home_leaving)>0){home_leaving}else{"HOPEFULLY THIS IS NOBODY'S NAME"}
      home_enter_players <- if(length(home_entering)>0){home_entering}else{"HOPEFULLY THIS IS NOBODY'S NAME"}
      away_exit_players <-  if(length(away_leaving)>0){away_leaving}else{"HOPEFULLY THIS IS NOBODY'S NAME"}
      away_enter_players <- if(length(away_entering)>0){away_entering}else{"HOPEFULLY THIS IS NOBODY'S NAME"}

      # Go through each row of the data frame of events for each half
      for (k in 1:(nrow(half_data))) {
        #First case looks at if it is a home substitution and the player matches expectations (next in vector and already on court)
        if (half_data$Event_Type[k] == "Leaves Game" &
            half_data$Event_Team[k] == home_team &
            half_data$Player_1[k] == home_exit_players[1] &
            !home_enter_players[1] %in% home_mat[k,] &
            half_data$Time[k] != "00:00") {
          #Then find row index of player leaving
          ind <- match(home_mat[k,], home_exit_players[1])
          ind <- which(!is.na(ind))
          #Create new row replacing the leaving player with the entering one
          new_players <- home_mat[k,]
          new_players[ind] <- home_enter_players[1]
          #Add the new lineup as the next row in the matrix
          home_mat[k + 1, ] <- new_players
          #Go to the next subbed players for entering and leaving
          home_enter_players <-
            home_enter_players[2:length(home_enter_players)]
          home_exit_players <-
            home_exit_players[2:length(home_exit_players)]
          #If a home sub occurs, the away lineup will stay the same
          away_mat[k + 1, ] <- away_mat[k,]
          # Now repeat the same process but for the away team
          # Since only one event can occur per row this can be handled with if/else if
        } else if (half_data$Event_Type[k] == "Leaves Game" &
                   half_data$Event_Team[k] == away_team &
                   half_data$Player_1[k] == away_exit_players[1] &
                   !away_enter_players[1] %in% away_mat[k,] &
                   half_data$Time[k] != "00:00") {
          ind <- match(away_mat[k,], away_exit_players[1])
          ind <- which(!is.na(ind))
          new_players <- away_mat[k,]
          new_players[ind] <- away_enter_players[1]
          away_mat[k + 1, ] <- new_players
          away_enter_players <-
            away_enter_players[2:length(away_enter_players)]
          away_exit_players <-
            away_exit_players[2:length(away_exit_players)]
          home_mat[k + 1, ] <- home_mat[k,]
          # Now handling some error situations with subs
          # Often if an entry error occurs, a player is eventually subbed in that is already on the court
          # There is no good way to deal with this, I've elected to just ignore the sub. and skip over the change
        } else if (half_data$Event_Type[k] == "Leaves Game" &
                   half_data$Event_Team[k] == away_team &
                   half_data$Player_1[k] == away_exit_players[1] &
                   away_enter_players[1] %in% away_mat[k,] &
                   half_data$Time[k] != "00:00") {
          away_enter_players <-
            away_enter_players[2:length(away_enter_players)]
          away_exit_players <-
            away_exit_players[2:length(away_exit_players)]
          home_mat[k + 1, ] <- home_mat[k,]
          away_mat[k + 1, ] <- away_mat[k,]
        } else if (half_data$Event_Type[k] == "Leaves Game" &
                   half_data$Event_Team[k] == home_team &
                   half_data$Player_1[k] == home_exit_players[1] &
                   home_enter_players[1] %in% home_mat[k,] &
                   half_data$Time[k] != "00:00") {
          home_enter_players <-
            home_enter_players[2:length(home_enter_players)]
          home_exit_players <-
            home_exit_players[2:length(home_exit_players)]
          home_mat[k + 1, ] <- home_mat[k,]
          away_mat[k + 1, ] <- away_mat[k,]
          # Finally, if there was no sub-type event we can just fill the new row with the prior
        } else{
          home_mat[k + 1, ] <- home_mat[k,]
          away_mat[k + 1, ] <- away_mat[k,]
        }
      }
      #This adds the matrix for each half to the game matrix
      home_player_matrix <-
        rbind(home_player_matrix, home_mat[-1,])
      away_player_matrix <-
        rbind(away_player_matrix, away_mat[-1,])
      ###END OF LOOP
    }

    # Player Cleaning ####

    #Remove the first row as it is made up of NAs from nature of how it's structured
    home_player_matrix <- home_player_matrix[-1,]
    away_player_matrix <- away_player_matrix[-1,]

    #Rename columns to denote players
    colnames(home_player_matrix) <-
      c("Home.1", "Home.2", "Home.3", "Home.4", "Home.5")
    colnames(away_player_matrix) <-
      c("Away.1", "Away.2", "Away.3", "Away.4", "Away.5")

    #Adds these columns to a new play by play data frame
    mild_game <-
      cbind(dirty_game,
            as.data.frame(home_player_matrix, row.names = F))
    mild_game <-
      cbind(mild_game,
            as.data.frame(away_player_matrix, row.names = F))

    #Add the event length variable which can often be helpful
    mild_game <- mild_game %>%
      dplyr::mutate(Event_Length = Game_Seconds - dplyr::lag(Game_Seconds))
    mild_game$Event_Length[1] <- mild_game$Game_Seconds[1]

    #Can now put together final data frame
    clean_game <- mild_game %>%
      dplyr::mutate(
        Home_Score = as.numeric(Home_Score),
        Away_Score = as.numeric(Away_Score),
        Shot_Value = dplyr::case_when(
          Event_Type == "Two Point Jumper" ~ 2,
          Event_Type == "Layup" ~ 2,
          Event_Type == "Three Point Jumper" ~ 3,
          Event_Type == "Dunk" ~ 2,
          Event_Type == "Free Throw" ~ 1,
          Event_Type == "Tip In" ~ 2,
          Event_Type == "Hook" ~ 2
        )
      ) %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      dplyr::select(
        ID:Event_Team,
        Event_Description,
        Player_1,
        Player_2,
        Event_Type,
        Event_Result,
        Shot_Value,
        Event_Length,
        Home.1:Away.5
      ) %>%
      dplyr::mutate(Status = status)

    # Final round of checking for data entry mistakes by scorekeeper
    # Look for if a player is said to do an event and they aren't on the court as determined above
    player_errors <- apply(clean_game, 1, function(x) {
      if (sum(x[13:14] %in% c(x[19:28]), na.rm = T) == 0) {
        return(T)
      } else{
        return(F)
      }
    })

    # Oftentimes these errors simply occur when event ordering gets mixed up by scorekeeper
    # I don't really think this should be framed as a true error, as they are deadball events
    # This isn't necessarily an error
    entry_mistakes <-
      clean_game[which(
        player_errors &
          !clean_game$Event_Type %in% c(
            "Leaves Game",
            "Enters Game",
            "Free Throw"
          ) & clean_game$Player_1 != "TEAM"
      ),]
    # Provide a column for deviations, allowing user to filter pbp with too many errors
    clean_game$Sub_Deviate <- nrow(entry_mistakes)
    # Warns user of number of entry mistakes found - only report iif significant
    if (nrow(entry_mistakes) > 5) {
      message(paste(nrow(entry_mistakes), "on court player discrepancies"))
    }
    # Give user final message about the status of the game they've scraped
    message(paste(date, home_team, "v", away_team, "| ", format, "|", game_id))
    # Sys.sleep so the ncaa server isn't overworked
    if(isUrlRead) {
      Sys.sleep(2)
    }

    return(clean_game)
  }
}

#' Date Schedule Scrape
#'
#' This function returns a schedule for the given date and specified conference.
#' Results are included if applicable, as well as the play-by-play game id
#' @param  date a character object containing a date in the format mm/dd/yyyy. Defaults is previous day (yesterday)
#' @param conference the common name used for a conference, not sensitive to case, spacing, punctuation, etc.
#' @param conference.ID alternatively, if the conference ID is known it replace the conference name variable
#' @importFrom XML readHTMLTable
#' @import dplyr
#' @import stringr
#' @return data frame with each row representing an inidividual game
#' \itemize{
#' \item{Date} - Game date
#' \item{Start_Time} - Start time reported in eastern time zone (I believe)
#' \item{Home} - Home team
#' \item{Away} - Away team
#' \item{GameID} - If the game is finished and has play-by-play data available, the game ID used to scrape game data
#' \item{Home_Score} - If the game is finished, the final score for the home team
#' \item{Away_Score} - If the game is finished, the final score for the away team
#' \item{Attendance} - The attendance count reported by the NCAA
#' \item{Neutral_Site} - A logical variable that is true when the game was played at a designated neutral destination
#' }
#' @export
#' @examples
#' get_date_games(date = "12/11/2018", conference = "Ivy")
#' get_date_games(date = "12/11/2018", conference.ID = 865)
get_date_games <-
  function(date = as.character(format(Sys.Date() - 1, "%m/%d/%Y")),
           conference = "All",
           conference.ID = NA,
           use_file = F,
           save_file = F,
           base_path = NA) {
    #First convert the character date given by user into a date object
    dateform <- as.Date(as.character(date), format = "%m/%d/%Y")

    # Find the season id needed by the url given the date of the game
    # The pbp only goes back to 2011 in most cases, so no need to pull deeper
    seasonid <- case_when(
      # 19-20
      dateform > as.Date("2019-05-01") &
        dateform <= as.Date("2020-05-01") ~ 17060,
      #18-19
      dateform > as.Date("2018-05-01") &
        dateform <= as.Date("2019-05-01") ~ 16700,
      #17-18
      dateform > as.Date("2017-05-01") &
        dateform <= as.Date("2018-05-01") ~ 13533,
      #16-17
      dateform > as.Date("2016-05-01") &
        dateform <= as.Date("2017-05-01") ~ 13100,
      #15-16
      dateform > as.Date("2015-05-01") &
        dateform <= as.Date("2016-05-01") ~ 12700,
      #14-15
      dateform > as.Date("2014-05-01") &
        dateform <= as.Date("2015-05-01") ~ 12320,
      #13-14
      dateform > as.Date("2013-05-01") &
        dateform <= as.Date("2014-05-01") ~ 11700,
      #12-13
      dateform > as.Date("2012-05-01") &
        dateform <= as.Date("2013-05-01") ~ 10883,
      #11-12
      dateform > as.Date("2011-05-01") &
        dateform <= as.Date("2013-05-01") ~ 10480,
      #10-11
      dateform > as.Date("2010-05-01") &
        dateform <= as.Date("2011-05-01") ~ 10220,
      T ~ 0
    )
    if (seasonid == 0) {
      return("Season Not Available")
    }

    # If user doesn't know the conference id, they can enter a conference name
    # The naming conventions are handled below and stripped of any case, spaces, punctuation, etc.
    conferenceform <-
      tolower(sub("[^[:alnum:]=\\.]", "", conference))
    conferenceid <- dplyr::case_when(
      conferenceform == "aac" ~ 823,
      conferenceform == "acc" ~ 821,
      conferenceform == "asun" ~ 920,
      conferenceform == "americaneast" ~ 845,
      conferenceform == "atlantic10" ~ 820,
      conferenceform == "big12" ~ 25354,
      conferenceform == "bigeast" ~ 30184,
      conferenceform == "bigsky" ~ 825,
      conferenceform == "bigsouth" ~ 826,
      conferenceform %in% c("bigten", "big10") ~ 827,
      conferenceform == "bigwest" ~ 904,
      conferenceform == "cusa" ~ 24312,
      conferenceform == "caa" ~ 837,
      conferenceform == "horizon" ~ 881,
      conferenceform == "ivy" ~ 865,
      conferenceform == "maac" ~ 871,
      conferenceform == "mac" ~ 875,
      conferenceform == "meac" ~ 876,
      conferenceform == "mvc" ~ 884,
      conferenceform == "mwc" ~ 5486,
      conferenceform == "nec" ~ 846,
      conferenceform == "ovc" ~ 902,
      conferenceform == "pac12" ~ 905,
      conferenceform == "patriot" ~ 838,
      conferenceform == "sec" ~ 911,
      conferenceform == "swac" ~ 916,
      conferenceform == "socon" ~ 912,
      conferenceform == "southland" ~ 914,
      conferenceform == "summit" ~ 819,
      conferenceform == "sunbelt" ~ 818,
      conferenceform == "wac" ~ 923,
      conferenceform == "wcc" ~ 922,
      conferenceform == "all" ~ 0,
      T ~ 99999999
    )
    # When a bad entry is found return error
    if (conferenceid == 99999999) {
      message("Conference ID not found, using all")
      # return(NA)
      conferenceid = 0
    }
    # When the user gives their own conference ID, this replaces the text option
    if (!is.na(conference.ID)) {
      conferenceid = conference.ID
    }

    #formats date as found in the url
    date2 <- gsub("[/]", "%2F", date)

    #pulls the necessary url
    url <-
      paste0(
        "https://stats.ncaa.org/season_divisions/",
        seasonid,
        "/scoreboards?game_date=",
        date2,
        "&conference_id=",
        conferenceid,
        "&commit=Submit"
      )
    file_dir <- paste0(base_path, "date_games/")
    file_path <- paste0(file_dir, date2, "_", conferenceid, ".html")

    # Give user option to save raw html file (to make future processing more efficient)
    if (save_file & !is.na(base_path)) {
      html <- readLines(url)
      dir.create(file_dir, recursive = T, showWarnings = F)
      writeLines(html, file_path)
    }

    # Reads the html and pulls the table holding the scores
    if (use_file & !is.na(base_path)) {
      html <- readLines(file_path)
    } else {
      html <- readLines(url)
    }

    table <- tryCatch(XML::readHTMLTable(html)[[1]],
             error = function(e) {
               stop("No Games Table Found")
             })


    #The table is always read in the same messy way
    #Each game in the schedule starts on a row following pattern 1,6,11,etc. this gets all of those indices
    starting_rows <- (1:(nrow(table) / 5)) * 5 - 4

    # Pull game meta data from relevant part of the table
    game_date <- as.character(table$V1[starting_rows])
    attendance <- as.character(table$V7[starting_rows])

    away_team <- as.character(table$V3[starting_rows])
    home_team <- as.character(table$V2[starting_rows + 3])

    home_score <- as.character(table$V3[starting_rows + 3])
    away_score <- as.character(table$V5[starting_rows])

    #This searches for all game IDs on the schedule page, using links found in the html
    game_ids <-
      unlist(stringr::str_extract_all(html, "(?<=/contests/)\\d+(?=/box_score)"))
    game_ids <- game_ids[which(!game_ids %in% seasonid)]

    #Also creates variable used to find if a game was held at a neutral side
    isNeutral <- table$V6[starting_rows] != ""

    #Informs user of how many games and games with a relevant ID were found
    message(paste(date, "|", length(game_ids), "games found"))

    # Unfortunately, the game ID for boxscore isn't the same as the game ID for pbp
    # As a result, this function needs to convert from boxscore to pbp but as a result be slower
    # Need to read each box score page and find link to pbp page
    url2 <-
      paste0("https://stats.ncaa.org/contests/", game_ids, "/box_score")

    # Clean team names
    home_name = gsub(" [(].*[)]","", home_team)
    home_wins = unlist(stringr::str_extract_all(home_team, "(?<=[(])\\d+(?=-)"))
    home_losses = unlist(stringr::str_extract_all(home_team, "(?<=-)\\d+(?=[)])"))

    away_name = gsub(" [(].*[)]","", away_team)
    away_wins = unlist(stringr::str_extract_all(away_team, "(?<=[(])\\d+(?=-)"))
    away_losses = unlist(stringr::str_extract_all(away_team, "(?<=-)\\d+(?=[)])"))

    #Create dataframe
    game_data <- data.frame(
      Date = substr(game_date, 1, 10),
      Start_Time = substr(game_date, 12, 19),
      Home = home_name,
      Away = away_name,
      BoxID = game_ids,
      GameID = NA,
      Home_Score = home_score,
      Away_Score = away_score,
      Attendance = attendance,
      Neutral_Site = isNeutral,
      Home_Wins = as.numeric(home_wins),
      Home_Losses = as.numeric(home_losses),
      Away_Wins = as.numeric(away_wins),
      Away_Losses = as.numeric(away_losses),

      stringsAsFactors = F,
      row.names = NULL
    )

    # Have to iterate through every game for the given day and find all play by play ids on the box score page
    if(length(game_ids)>0){
      for (i in 1:length(url2)) {
        temp_html <- readLines(url2[i])
        new_id <- unlist(stringr::str_extract(temp_html, "(?<=play_by_play/)\\d+"))
        new_id <- unique(new_id[!is.na(new_id)])
        game_data$GameID[i] <- new_id
        Sys.sleep(0.5)
      }
    } else {
      message("No Game IDs Found")
    }

    return(game_data)
}

#' Team Schedule Scrape
#'
#' This function returns a data frame of the schedule for the specified team. This will include game ids used
#' for play-by-play scraping if the game has ended, along with the team scores and attendance. Note: currently,
#' the season/team.name parameters can only be used for the 2016-17, 2017-18, 2018-19 seasons
#' @param team.id The unique id given to each college/team for each season. This can be found in the url of the team page.
#' @param season Season following format yyy1-y2, ex "2018=19"
#' @param team.name Alternative to using the id, you can get a team from data(teamids) with a season and team name specification.
#' This inputs a team name, to be used along with season. This needs the school name not the complete team name, so "Duke" not "Duke Blue Devils".
#' @importFrom XML readHTMLTable
#' @import dplyr
#' @import stringr
#' @return data frame with each row representing an individual game
#' \itemize{
#' \item{Date} - Game date
#' \item{Home} - Home team
#' \item{Home_Score} - If the game is finished, the final score for the home team
#' \item{Away} - Away team
#' \item{Away_Score} - If the game is finished, the final score for the away team
#' \item{Game_ID} - If the game is finished and has play-by-play data available, the game ID used to scrape game data
#' \item{isNeutral} - A logical variable that is true when the game was played at a designated neutral destination
#' \item{Detail} - Additional detail such as if the game went into OT and # of OTs
#' }
#' @export
#' @examples
#' get_team_schedule(team.id = 450680)
#' get_team_schedule(season = "2018-19", team.name = "Penn")
get_team_schedule <-
  function(team.id = NA,
           season = NA,
           team.name = NA,
           use_file = T,
           save_file = T,
           base_path = NA,
           overwrite = F) {

    # If the user doesn't know id and instead gives a team name and season searches team DB for ID
    # This can only be done since 16-17 at the moment
    if (is.na(team.id) & !is.na(team.name) & !is.na(season)) {
      team.id <-
        bigballR::teamids$ID[which(bigballR::teamids$Team == team.name & bigballR::teamids$Season == season)]
    } else if(is.na(team.id) & is.na(team.name) & is.na(season)){
      message("Improper Request")
      return(NULL)
    }

    # Pull the relevant table from the team webpage
    url <- paste0("https://stats.ncaa.org/teams/", team.id)
    file_dir <- paste0(base_path, "team_schedule/")
    file_path <- paste0(file_dir, team.id, ".html")

    if (use_file & !is.na(base_path) & file.exists(file_path)) {
      html <- readLines(file_path)
    } else {
      html <- readLines(url)
    }

    # Give user option to save raw html file (to make future processing more efficient)
    if (save_file & !is.na(base_path) & (!file.exists(file_path) | overwrite)) {
      dir.create(file_dir, recursive = T, showWarnings = F)
      writeLines(html, file_path)
    }

    tables <- XML::readHTMLTable(html)
    df <- data.frame(as.matrix(tables[[2]]), stringsAsFactors = F)

    #New
    df <- df[seq(1,nrow(df), by = 2),]

    game_ids <-
      unlist(stringr::str_extract_all(html, "(?<=contests/)\\d+(?=[/])"))

    # Game IDs links to box score game id, not play by play id
    # Unfortunately need to now parse webpage for each game played to find game id
    url2 <-
      paste0("https://stats.ncaa.org/contests/", game_ids, "/box_score")

    new_ids <- c()
    message("Compiling Game IDs")
    pb = txtProgressBar(min = 0, max = length(url2), initial = 0)

    # Have to iterate through every game for the given day and find all play by play ids on the box score page
    for (i in 1:length(url2)) {
      file_dir <- paste0(base_path, "box_score/")
      file_path <- paste0(file_dir, game_ids[i], ".html")
      isUrlRead <- F

      if (use_file & !is.na(base_path) & file.exists(file_path)) {
        temp_html <- readLines(file_path)
      } else {
        isUrlRead <- T
        temp_html <- readLines(url2[i])
      }

      # Give user option to save raw html file (to make future processing more efficient)
      if (save_file & !is.na(base_path)) {
        dir.create(file_dir, recursive = T, showWarnings = F)
        writeLines(temp_html, file_path)
      }

      new_id <- unlist(stringr::str_extract(temp_html, "(?<=play_by_play[/])\\d+"))
      new_id <- unique(new_id[!is.na(new_id)])
      new_ids <- c(new_ids,new_id)
      if (isUrlRead) {
        Sys.sleep(0.5)
      }
      setTxtProgressBar(pb,i)
    }

    close(pb)
    message("\nParsing Schedule")
    # Handle opponent and neutral games as both are broken up using an '@' character
    parsed <- lapply(df$Opponent, strsplit, "@")
    parsed <- lapply(parsed, function(x) {
      x <- unlist(x)
      t <- stringr::str_extract(x, "(?<=[\\#[0-9]+] ).*")
      t[is.na(t)] <- x[is.na(t)]
      for(j in 1:length(t)) {
        i <- 1
        while (!substr(t[j], 1, i) %in% bigballR::teamids$Team && i <= nchar(t[j])) {
          i <- i + 1
        }
        t[j] = (substr(t[j], 1, i))
      }
      return(t)
    })


    # Pulls the opponent if they are the home team
    Home <-
      lapply(parsed, function(x) {
        if (x[1] == "" & !is.na(x[2]))
          return(x[2])
      })

    # Searches for neutral game and gets location
    Neutral <-
      lapply(parsed, function(x) {
        if (length(x) == 2 & x[1] != "")
          return(x[2])

      })

    #Iterate through games and finds the home and away team
    home_team <- rep(NA, length(parsed))
    away_team <- rep(NA, length(parsed))
    is_neutral <- rep(F, length(parsed))
    for (i in 1:length(parsed)) {
      if (!is.null(Home[[i]])) {
        home_team[i] <- trimws(Home[[i]])
      } else {
        away_team[i] <- trimws(parsed[[i]][[1]][1])
      }
      # Also updates variable to specify if there is a home game
      if (!is.null(Neutral[[i]])) {
        is_neutral[i] <- T
      }
    }

    team_name <- bigballR::teamids$Team[which(bigballR::teamids$ID == team.id)]

    #This cleans the score information
    # score <- strsplit(df$Result, " - ") old
    score <- strsplit(df$Result, "-")
    selected_score <-
      trimws(gsub("W", "", gsub("L", "", sapply(score, function(x) {
        x[1]
      }))))
    opponent_score <- trimws(sapply(score, function(x) {
      x[2]
    }))
    # Separate out the details, which is # of OTs
    detail <- unname(sapply(opponent_score, function(x) {
      a <- gsub("\\)", "", strsplit(x, "\\(")[[1]][2])
    }))
    opponent_score <-
      unname(sapply(opponent_score, function(x)
        strsplit(x, " \\(")[[1]][1]))

    #Put everything together into tidy data frame
    team_data <- data.frame(
      Date = df$Date,
      Home = ifelse(!is.na(home_team), home_team, team_name),
      Home_Score = ifelse(!is.na(home_team), opponent_score, selected_score),
      Away = ifelse(!is.na(away_team), away_team, team_name),
      Away_Score = ifelse(!is.na(away_team), opponent_score, selected_score),
      Game_ID = c(new_ids, rep(NA, length(df$Date) - length(new_ids))),
      Box_ID = c(game_ids, rep(NA, length(df$Date) - length(game_ids))),
      isNeutral = is_neutral,
      Detail = detail,
      stringsAsFactors = F
    )
    #Replace blank portions of schedule with dashes, as that is used on NCAA site but NA is better for this purpose
    team_data[team_data == "-"] <- NA

    #Give user final status update and returns the df
    message(paste0(
      team_name[1],
      " complete--",
      nrow(team_data),
      "/",
      length(game_ids),
      " | games/ids found"
    ))

    return(team_data)
}

#' Team Roster Scrape
#'
#' This function returns a data frame of the roster for the specified team. This will include player names and positions
#' as well as jersey number, height and school year. Note: currently,
#' the season/team.name parameters can only be used for the 2016-17, 2017-18, 2018-19 seasons
#' @param team.id The unique id given to each college/team for each season. This can be found in the url of the team page.
#' @param season Alternative to using the id, you can get a team from data(teamids) with a season and team name specification.
#' String for the season stored as yyy1-y2 (2018-19 is current)
#' @param team.name Alternative to using the id, you can get a team from data(teamids) with a season and team name specification.
#' This inputs a team name, to be used along with season. This needs the school name not the complete team name, so "Duke" not "Duke Blue Devils".
#' @importFrom XML readHTMLTable
#' @import stringr
#' @import dplyr
#' @return data frame with each row representing a player on the roster
#' \itemize{
#' \item{Jersey} - Player jersey number
#' \item{Player} - Name of Player
#' \item{Pos} - Position (one of G,F,C) as designated by the NCAA
#' \item{Ht} - Height as reported by the NCAA
#' \item{Yr} - School year, as Fr, So, Jr, Sr
#' }
#' @export
#' @examples
#' get_team_roster(team.id = 450680)
#' get_team_roster(season = "2018-19", team.name = "Penn")

get_team_roster <-
  function(team.id = NA,
           season = NA,
           team.name = NA,
           use_file = F,
           save_file = F,
           base_path = NA,
           overwrite = F) {

    # If the user doesn't know id and instead gives a team name and season searches team DB for ID
    # This can only be done since 16-17 at the moment
    if (is.na(team.id) & !is.na(team.name) & !is.na(season)) {
      team.id <-
        bigballR::teamids$ID[which(bigballR::teamids$Team == team.name & bigballR::teamids$Season == season)]
    } else if(is.na(team.id) & is.na(team.name) & is.na(season)){
      message("Improper Request")
      return(NULL)
    }

    #Pull html for the team page
    url <- paste0("https://stats.ncaa.org/teams/", team.id)
    file_dir <- paste0(base_path, "team_roster/")
    file_path <- paste0(file_dir, team.id, ".html")
    isUrlRead <- F

    # Give user option to save raw html file (to make future processing more efficient)
    if (save_file & !is.na(base_path) & (!file.exists(file_path) | overwrite)) {
      isUrlRead <- T
      html <- readLines(url)
      dir.create(file_dir, recursive = T, showWarnings = F)
      writeLines(html, file_path)
    } else if (file.exists(file_path)) {
      html <- readLines(file_path)
    }

    if (use_file & !is.na(base_path)) {
      html <- readLines(file_path)
    } else {
      isUrlRead <- T
      html <- readLines(url)
    }

    #Find link to the team roster page
    roster_link <- html[which(grepl("Roster", html))]
    roster_link <-
      stringr::str_extract_all(roster_link, "(?<=\\\")(.*)(?=[\\\"])")
    roster_url <- paste0("https://stats.ncaa.org", roster_link)
    #Read html for the roster page and format it so it can be usable
    html_roster <- readLines(roster_url)
    table <- XML::readHTMLTable(html_roster)[[1]][, 1:5] %>%
      mutate_all(as.character)
    # Return the more usable roster page
    player <- table$Player
    clean_name <- sapply(strsplit(player, ","), function(x){trimws(paste(x[2],x[1]))})
    format <- gsub("[^[:alnum:] ]", "", clean_name)
    format <- toupper(gsub("\\s+",".", format))
    player_name <- gsub("\\.JR\\.|\\.SR\\.|\\.J\\.R\\.|\\.JR\\.|JR\\.|SR\\.|\\.SR|\\.JR|\\.SR|JR|SR|\\.III|III|\\.II|II","", format)
    player_name <- trimws(player_name)

    table$Player <- player_name
    table$CleanName <- clean_name
    table$HtInches <- unname(sapply(table$Ht, function(x){
      a = as.numeric(strsplit(x,"-")[[1]])
      12*a[1] + a[2]
    }))

    if (isUrlRead) {
      Sys.sleep(0.5)
    }
  return(table)
}

#' Multiple Game Play-By-Play Scraper
#'
#' This is the suggested function for scraping play-by-play, as it handles and binds multiple games.
#' Please see the scrape_game() function documentation for a more detailed description.
#' @param game_ids A string/numeric object or vector containing game ids
#' @export
#' @examples
#' get_play_by_play(c(4671170, 4674164))
#' team_sched <- get_team_schedule(team.id = 450680)
#' get_play_by_play(team_sched$Game_ID)
get_play_by_play <- function(game_ids, use_file = F, save_file = F, base_path = NA, overwrite=F) {
  #Cleans list of game ids to remove nas
  game_ids <- game_ids[!is.na(game_ids)]
  #Scrape all game ids into list

  game_list <- lapply(game_ids, function(x) {
    # Add error handling so if one game throws an error it will report and continue iterating
    tryCatch(scrape_game(x, use_file = use_file, save_file = save_file, base_path = base_path, overwrite=overwrite), error = function(e){
      print(paste0("Error with game id: ", x, " // ", e))
      return(NA)
    })
  })

  dirty_ind <- which(is.na(game_list))
  #Remove any incorrect games found
  if(length(dirty_ind) > 0) game_list <- game_list[-dirty_ind]
  #Bind rows together and return combined dataframe
  game_data <- do.call("binder", game_list)
  if(length(dirty_ind) != 0) {
    message(paste(paste(game_ids[dirty_ind], collapse = ","), "removed"))
  }

  return(game_data)
}

#' Lineup Compiler
#'
#' This function takes in a play-by-play dataframe, and generates all possible lineups for both teams.
#' It then calculates a variety of statistics/metrics at a lineup level.
#' @param play_by_play_data data frame consisting of play-by-play data from the functions scrape_game() or get_play_by_play()
#' @param keep.dirty logical variable to specify whether or not to filter out potentially inaccurrate data. When FALSE,
#' will remove all rows from games where the number of discrepencies is above the desired count. Defaults to FALSE.
#' @param garbage.filter logical variable to specify whether or not to filter out garbage time entries. Uses format similar
#' to cleaningtheglass.com definitions, but adjusted because of different time/scoring format. This is very subjective with no
#' true criteria or reasoning. Follows rule: a game that enters garbage time cannot exit even if a team comes back.
#' The remainder of a game is garbage time once a team is: Up by 25+ with 5 mins gone in 2nd half, up 20 with 10 mins gone in 2nd half,
#' up 15 with 16 mins gone in 2nd half. Defaults to FALSE, which means garbage time will not be removed
#' @param error.thresh numeric variable that lets user set their preferred discrepancy threshold with the keep.dirty variable. This means
#' when less than the threshold occurs in a game, it will be considered clean. As defined in scrape_game(), a discrepancy occurs when a
#' player registers an event when they are not found to be on the court. Defaults to 5 discrepancies.
#' @import dplyr
#' @export
#' @return data frame with each row representing a unique lineup. All stats for entire lineup or opponent (o- prefix)
#' \itemize{
#' \item{P1} - Player in lineup
#' \item{P2} - Player in lineup
#' \item{P3} - Player in lineup
#' \item{P4} - Player in lineup
#' \item{P5} - Player in lineup
#' \item{Team} - Team for the lineup
#' \item{Mins} - Minutes the lineup was on the court
#' \item{PTS} - Points scored
#' \item{FGA} - Field goal attempts
#' \item{TO} - Turnovers
#' \item{TPA} - Three point attempts
#' \item{FGM} - Field goals made
#' \item{TPM} - Three points made
#' \item{FTA} - Free throw attempts
#' \item{FTM} - Free throws made
#' \item{ORB} - Offensive rebounds
#' \item{DRB} - Defensive rebounds
#' \item{RIMA} - Rim attempts: defined as layups, dunks, tip-ins, hook attempts
#' \item{BLK} - Blocked shots
#' \item{AST} - Assists
#' \item{POSS} - (Offensive) Possessions: (FGA + .475 x FTA - ORB + TO + oFGA + .475 x oFTA - oORB + oTO) /2
#' \item{TS.} - True shooting percentage: (PTS / 2) / (FGA + .475 x FTA),
#' \item{eFG.} - Effective field goal percentage: (FGM + 0.5 x TPM) / FGA,
#' \item{TPP} - Three point percentage: TPA/TPM
#' \item{FTP} - Free throw percentage: FTA/FTM
#' \item{TPrate} - Three point attempt rate: TPA/FGA
#' \item{ASTrate} - Assist rate: AST/FGM
#' \item{TOrate} - Turnover rate: TO/POSS
#' \item{FTrate} - Free throw rate: FTA/FGA
#' \item{BLKrate} - Block rate: BLK/FGA
#' \item{ORB.} - Offensive rebound percentage: ORB / (ORB + oDRB)
#' \item{DRB.} - Defensive rebound percentage: DRB / (DRB + oORB)
#' \item{OEFF} - Offensive efficiency: 100 * (PTS/POSS)
#' \item{DEFF} - Defensive efficiency: 100 * (oPTS/POSS)
#' \item{NETEFF} - Net efficiency: OEFF - DEFF
#' \item{PACE} - Average time per possession (Seconds): (Possessions / Mins) * 60
#' \item{ShotsPerPoss} - Estimate of shot attempts per possession: 1 + (ORB - TO) / POSS,
#' }
get_lineups <-
  function(play_by_play_data = NA,
           keep.dirty = F,
           garbage.filter = F,
           error.thresh = 5) {

    #First user can decide if they want to keep or remove data from potentially corrupted games
    if (keep.dirty == F) {
      # Takes only rows from clean games or when there are errors less than the specified threshold
      lineup_stuff <- play_by_play_data %>%
        dplyr::filter(Sub_Deviate <= error.thresh)

      #Report how many rows that were deemed dirty were removed
      message(paste0(round((nrow(play_by_play_data) - nrow(lineup_stuff)) /
                             nrow(play_by_play_data), 2
      ) * 100, "% dirty rows removed"))
    } else{
      lineup_stuff <- play_by_play_data
    }
    #User can also remove rows that are deemed to occur in garbage time
    if (garbage.filter == T) {
      # This roughly follows the cleaningtheglass criteria, but is made a little different
      # because of the different rules and time structures of college basketball
      # definitely willing to change this
      # follows rule a game that enters garbage time cannot exit even if a team comes back
      # A team is: Up by 25+ with 5 mins gone in 2nd half, up 20 with 10 mins gone in 2nd half,
      # up 15 with 16 mins gone in 2nd half
      lineup_stuff <- lineup_stuff %>%
        dplyr::group_by(ID) %>%
        dplyr::mutate(
          Garbage_Thresh = dplyr::case_when(
            abs(Home_Score - Away_Score) >= 25 & Game_Seconds >= 1500 ~ T,
            abs(Home_Score - Away_Score) >= 20 & Game_Seconds >= 1800  ~ T,
            abs(Home_Score - Away_Score) >= 15 & Game_Seconds >= 2160  ~ T,
            TRUE ~ F
          ),
          Garbage_Time = cumsum(Garbage_Thresh) >= 1
        ) %>%
        dplyr::filter(Garbage_Time == F) %>%
        dplyr::select(-Garbage_Thresh, -Garbage_Time) %>%
        dplyr::ungroup()
    }

    # if(ncol(lineup_stuff) == 18) {
    #   lineup_stuff <- cbind(lineup_stuff, matrix(rep("Team", nrow(lineup_stuff)*10),
    #                                              ncol = 10,
    #                                              nrow = nrow(lineup_stuff))
    # }

    # missing_players <- apply(lineup_stuff[,19:28], 2, function(x){sum(is.na(x))})
    missing_rows <- apply(lineup_stuff[,19:28], 1, function(x){sum(is.na(x))})
    message(paste("Forced to remove", length(which(missing_rows!=0)), "rows due to missing players in on/off"))

    lineup_stuff <- lineup_stuff %>%
      filter(missing_rows==0)

    # Now sorts the home and away player alphabetically so players are always in the same column for a given lineup
    lineup_stuff <- apply(lineup_stuff, 1, function(x)
    {
      home_players <- sort(x[19:23])
      away_players <- sort(x[24:28])
      return(c(x[1:18], home_players, away_players, x[29:30]))
    })

    #Converts the sorted back into a data frame
    lineup_stuff2 <-
      data.frame(matrix(unlist(lineup_stuff), ncol = 30, byrow = T), stringsAsFactors = F)

    # if(ncol(play_by_play_data) == 31){
      # colnames(lineup_stuff2) <- colnames(play_by_play_data)[-30]
    # } else {
      colnames(lineup_stuff2) <- colnames(play_by_play_data)
    # }

    #Get all home lineups and calculate a variety of stats for each lineup
    #o is used to denote opponents
    suppressMessages(
    home_lineups <- lineup_stuff2 %>%
      # dplyr::mutate_if(is.factor, as.character) %>%
      dplyr::group_by(Home.1, Home.2, Home.3, Home.4, Home.5, Home) %>%
      dplyr::mutate(Shot_Value = as.numeric(Shot_Value),
             Event_Length = as.numeric(Event_Length)) %>%
      dplyr::summarise(
        #can sub event lengths to get total amount of time across entries
        Mins = sum(Event_Length / 60, na.rm = T),
        #points
        PTS = sum(
          (Event_Team == Home) * (Event_Result == "made") * Shot_Value, na.rm = T),
        oPTS = sum(
          (Event_Team == Away) * (Event_Result == "made") * Shot_Value, na.rm = T),
        #field goal attempts
        FGA = sum((Shot_Value %in% c(2, 3)) * (Event_Team == Home) * 1, na.rm = T),
        oFGA = sum((Shot_Value %in% c(2, 3)) * (Event_Team == Away) * 1, na.rm = T),
        #field goal makes
        FGM = sum((Shot_Value %in% c(2, 3)) * (Event_Team == Home) * (Event_Result == "made") * 1, na.rm = T),
        oFGM = sum((Shot_Value %in% c(2, 3)) * (Event_Team == Away) * (Event_Result == "made") * 1, na.rm = T),
        #three point attempts
        TPA = sum((Shot_Value == 3) * (Event_Team == Home) * 1, na.rm = T),
        oTPA = sum((Shot_Value == 3) * (Event_Team == Away) * 1, na.rm = T),
        #three point makes
        TPM = sum((Shot_Value == 3) * (Event_Team == Home) * (Event_Result == "made") * 1, na.rm = T),
        oTPM = sum((Shot_Value == 3) * (Event_Team == Away) * (Event_Result == "made") * 1, na.rm = T),
        #free throw attempts
        FTA = sum((Shot_Value == 1) * (Event_Team == Home) * 1, na.rm = T),
        oFTA = sum((Shot_Value == 1) * (Event_Team == Away) * 1, na.rm = T),
        #free throw makes
        FTM = sum((Shot_Value == 1) * (Event_Team == Home) * (Event_Result == "made") * 1, na.rm = T),
        oFTM = sum((Shot_Value == 1) * (Event_Team == Away) * (Event_Result == "made") * 1, na.rm = T),
        #rough estimate of rim attempts using terminology of ncaa
        RIMA = sum((
          Event_Type %in% c("Dunk", "Layup", "Hook", "Tip-In")
        ) * (Event_Team == Home) * 1, na.rm = T),
        oRIMA = sum((
          Event_Type %in% c("Dunk", "Layup", "Hook", "Tip-In")
        ) * (Event_Team == Away) * 1, na.rm = T),
        RIMM = sum((Event_Result == "made") * (
          Event_Type %in% c("Dunk", "Layup", "Hook", "Tip-In")
        ) * (Event_Team == Home) * 1, na.rm = T),
        oRIMM = sum((Event_Result == "made") * (
          Event_Type %in% c("Dunk", "Layup", "Hook", "Tip-In")
        ) * (Event_Team == Away) * 1, na.rm = T),
        #offensive rebounds
        ORB = sum((Event_Type == "Offensive Rebound") * (Event_Team == Home) * 1, na.rm = T),
        oORB = sum((Event_Type == "Offensive Rebound") * (Event_Team == Away) * 1, na.rm = T),
        #defensive rebounds
        DRB = sum((Event_Type == "Defensive Rebound") * (Event_Team == Home) * 1, na.rm = T),
        oDRB = sum((Event_Type == "Defensive Rebound") * (Event_Team == Away) * 1, na.rm = T),
        #blocked shots
        BLK = sum((Event_Type == "Blocked Shot") * (Event_Team == Home) * 1, na.rm = T),
        oBLK = sum((Event_Type == "Blocked Shot") * (Event_Team == Away) * 1, na.rm = T),
        #turnovers
        TO = sum((Event_Type == "Turnover") * (Event_Team == Home) * 1, na.rm = T),
        oTO = sum((Event_Type == "Turnover") * (Event_Team == Away) * 1, na.rm = T),
        #assists
        AST = sum((!is.na(Player_2)) * (Event_Team == Home) * 1, na.rm = T),
        oAST =  sum((!is.na(Player_2)) * (Event_Team == Away) * 1, na.rm = T)
      ) %>%
      dplyr::rename(
        P1 = Home.1,
        P2 = Home.2,
        P3 = Home.3,
        P4 = Home.4,
        P5 = Home.5,
        Team = Home
      ))
    #same done for away team
    suppressMessages(
    away_lineups <- lineup_stuff2 %>%
      # dplyr::mutate_if(is.factor, as.character) %>%
      dplyr::group_by(Away.1, Away.2, Away.3, Away.4, Away.5, Away) %>%
      dplyr::mutate(Shot_Value = as.numeric(Shot_Value),
             Event_Length = as.numeric(Event_Length)) %>%
      dplyr::summarise(
        Mins = sum(Event_Length / 60, na.rm = T),
        PTS = sum((Event_Team == Away) * (Event_Result == "made") * Shot_Value, na.rm = T),
        oPTS = sum((Event_Team == Home) * (Event_Result == "made") * Shot_Value,na.rm = T),
        FGA = sum((Shot_Value %in% c(2, 3)) * (Event_Team == Away) * 1, na.rm = T),
        oFGA = sum((Shot_Value %in% c(2, 3)) * (Event_Team == Home) * 1, na.rm = T),
        FGM = sum((Shot_Value %in% c(2, 3)) * (Event_Team == Away) * (Event_Result == "made") * 1, na.rm = T),
        oFGM = sum((Shot_Value %in% c(2, 3)) * (Event_Team == Home) * (Event_Result == "made") * 1, na.rm = T),
        TPA = sum((Shot_Value == 3) * (Event_Team == Away) * 1, na.rm = T),
        oTPA = sum((Shot_Value == 3) * (Event_Team == Home) * 1, na.rm = T),
        TPM = sum((Shot_Value == 3) * (Event_Team == Away) * (Event_Result == "made") * 1, na.rm = T),
        oTPM = sum((Shot_Value == 3) * (Event_Team == Home) * (Event_Result == "made") * 1, na.rm = T),
        FTA = sum((Shot_Value == 1) * (Event_Team == Away) * 1, na.rm = T),
        oFTA = sum((Shot_Value == 1) * (Event_Team == Home) * 1, na.rm = T),
        FTM = sum((Shot_Value == 1) * (Event_Team == Away) * (Event_Result == "made") * 1, na.rm = T),
        oFTM = sum((Shot_Value == 1) * (Event_Team == Home) * (Event_Result == "made") * 1, na.rm = T),
        RIMA = sum((
          Event_Type %in% c("Dunk", "Layup", "Hook","Tip-In")
        ) * (Event_Team == Away) * 1, na.rm = T),
        oRIMA = sum((
          Event_Type %in% c("Dunk", "Layup", "Hook","Tip-In")
        ) * (Event_Team == Home) * 1, na.rm = T),
        RIMM = sum((Event_Result == "made") * (
          Event_Type %in% c("Dunk", "Layup", "Hook", "Tip-In")
        ) * (Event_Team == Away) * 1, na.rm = T),
        oRIMM = sum((Event_Result == "made") * (
          Event_Type %in% c("Dunk", "Layup", "Hook", "Tip-In")
        ) * (Event_Team == Home) * 1, na.rm = T),
        ORB = sum((Event_Type == "Offensive Rebound") * (Event_Team == Away) * 1, na.rm = T),
        oORB = sum((Event_Type == "Offensive Rebound") * (Event_Team == Home) * 1, na.rm = T),
        DRB = sum((Event_Type == "Defensive Rebound") * (Event_Team == Away) * 1, na.rm = T),
        oDRB = sum((Event_Type == "Defensive Rebound") * (Event_Team == Home) * 1, na.rm = T),
        BLK = sum((Event_Type == "Blocked Shot") * (Event_Team == Away) * 1, na.rm = T),
        oBLK = sum((Event_Type == "Blocked Shot") * (Event_Team == Home) * 1, na.rm = T),
        TO = sum((Event_Type == "Turnover") * (Event_Team == Away) * 1, na.rm = T),
        oTO = sum((Event_Type == "Turnover") * (Event_Team == Home) * 1, na.rm = T),
        AST = sum((!is.na(Player_2)) * (Event_Team == Away) * 1, na.rm = T),
        oAST =  sum((!is.na(Player_2)) * (Event_Team == Home) * 1, na.rm = T)
      ) %>%
      dplyr::rename(
        P1 = Away.1,
        P2 = Away.2,
        P3 = Away.3,
        P4 = Away.4,
        P5 = Away.5,
        Team = Away
      ))

    #combine lineups from home and away and calculate a variety of stats
    suppressMessages(
    lineups <- dplyr::bind_rows(home_lineups, away_lineups) %>%
      dplyr::group_by(P1, P2, P3, P4, P5, Team) %>%
      dplyr::summarise_if(is.numeric, sum) %>%
      dplyr::mutate(
        #estimate of possesions using common formula
        POSS = ceiling(FGA + .475*FTA - ORB + TO),
        oPOSS = ceiling(oFGA + .475*oFTA - oORB + oTO),
        ePOSS = (POSS + oPOSS) / 2,
        #efficiency scaled to points per 100 possessions
        ORTG = PTS / ePOSS * 100,
        DRTG = oPTS / ePOSS * 100,
        NETRTG = ORTG - DRTG,
        # field goal percentage
        FG. = FGM / FGA,
        oFG. = oFGM / oFGA,
        #three point percentage
        TPP = TPM / TPA,
        oTPP = oTPM / oTPA,
        #free throw percentage
        FTP = FTM / FTA,
        oFTP = oFTM / oFTA,
        #effective shooting percentage
        eFG. = (FGM + 0.5 * TPM) / FGA,
        oeFG. = (oFGM + 0.5 * oTPM) / oFGA,
        #true shooting percentage
        TS. = (PTS / 2) / (FGA + .475 * FTA),
        oTS. = (oPTS / 2) / (oFGA + .475 * oFTA),
        # rim field goal percentage
        RIM. = RIMM / RIMA,
        oRIM. = oRIMM / oRIMA,
        # midrange field goal percentage
        MID. = (FGM - RIMM - TPM) / (FGA - RIMA - TPA),
        oMID. = (oFGM - oRIMM - oTPM) / (oFGA - oRIMA - oTPA),
        #% of fga that are threes
        TPrate = TPA / FGA,
        oTPrate = oTPA / oFGA,
        #% of fga at the rim
        RIMrate = RIMA / FGA,
        oRIMrate = oRIMA / oFGA,
        #%midrange fga
        MIDrate = (FGA - TPA - RIMA) / FGA,
        oMIDrate = (oFGA - oTPA - oRIMA) / oFGA,
        #rate of free throw attempts per field goal attempt
        FTrate = FTA / FGA,
        oFTrate = oFTA / oFGA,
        #percentage of makes that are assisted
        ASTrate = AST / FGM,
        oASTrate = oAST / oFGM,
        #percentage of possessions ending with turnovers
        TOrate = TO / POSS,
        oTOrate = oTO / POSS,
        #rate that team blocks shots (so defensively) per opponent attempt
        BLKrate = BLK / oFGA,
        oBLKrate = oBLK / FGA,
        #rebounding percentages
        ORB. = ORB / (ORB + oDRB),
        DRB. = DRB / (DRB + oORB),
        # time per possession in  seconds
        PACE = (Mins / ePOSS) * 30,
        #estimate of shots per possesion
        ShotsPerPoss = 1 + (ORB - TO) / POSS,
        oShotsPerPoss = 1 + (oORB - oTO) / oPOSS
      ) %>%
      #no need to have long decimals so round everything
      dplyr::mutate_if(is.numeric, ~ round(., 3)) %>%
      dplyr::ungroup() %>%
      # dplyr::filter(Mins > 0) %>%
      dplyr::select(P1:Team, Mins, PTS, oPTS, POSS:oShotsPerPoss, dplyr::everything()))
    #change any NA/infinite/etc. that comes up in calculations to 0
    lineups[is.na(lineups)] <- 0
    lineups[,7:74] <- apply(lineups[,7:74], 2, function(x){ifelse(is.infinite(x),0,x)})

    return(lineups)
}

#' On-Off Comparison Function
#'
#' This function passes in lineup data and calculates the on/off lineup statistics for all lineup combinations of players specified.
#' This allows users to view on/off statistics for individual players, as well as combinations of multiple players. Users can also
#' specify if they'd like specific players to be included or excluded from all lineups in use
#' @param Players character vector of players desired to be compared with on/off
#' @param Lineup_Data data frame made up of lineups collected from the get_lineups() function
#' @param Included character vector of players. These players will be on the court for every lineup considered.
#' @param Excluded character vector of players. These players will be off the court for every lineup considered.
#' @import dplyr
#' @export
#' @return data frame with each row representing an on/off combination. Explanations of statistics found in get_lineups()
#' @examples Duke_Lineups = get_lineups(get_team_schedule(season="2018-19", team.name = "Duke))
#' Get all on and off combinations for the first 3 players when Tre Jones is on the court
#' on_off_generator(Players = c("CAM.REDDISH","RJ.BARRETT","ZION.WILLIAMSON"), Lineup_Data = Duke_Lineups, Included = "TRE.JONES")
on_off_generator <-
  function(Players,
           Lineup_Data,
           Included = NA,
           Excluded = NA) {

    #first find which team is being looked for from the players mentioned
    find_team <- unique(
      dplyr::filter(
        Lineup_Data,
        P1 %in% Players |
          P2 %in% Players |
          P3 %in% Players |
          P4 %in% Players |
          P5 %in% Players
      )$Team
    )
    #if the wrong number of teams are identified
    if (length(find_team) != 1) {
      stop("ERROR- Player team not found")
      # return(NULL)
    }
    # Generates all relevant lineups using the included and excluded variables
    data <- if (!is.na(Included[1]) | !is.na(Excluded[1])) {
      get_player_lineups(Lineup_Data, Included, Excluded)
    } else{
      Lineup_Data %>%
        dplyr::filter(Team == find_team)
    }

    # Create matrix for on/off of each player identified
    players <-
      matrix(rep(NA, nrow(data) * length(Players)), ncol = length(Players))
    colnames(players) <- Players
    # Find if the player is in each lineup
    for (i in 1:length(Players)) {
      players[, i] <- apply(data, 1, function(x) {
        playerIn <- Players[i] %in% x[1:5]
      })
    }
    # get all possible combinations of identified players
    poss_ind <- rep(list(c(T, F)), length(Players))
    combos <- expand.grid(poss_ind)

    #calculate stats for each combination generated above
    final <- data.frame()
    for (i in 1:nrow(combos)) {
      relRow <- apply(players, 1,
                      function(x) {
                        if (sum(x == combos[i,]) == length(Players)) {
                          return(T)
                        } else{
                          return(F)
                        }
                      })

      on <- data[relRow == T,] %>%
        dplyr::summarise_if(is.numeric, sum) %>%
        dplyr::mutate(Status = paste(Players, ifelse(combos[i,], "On", "Off"), collapse = " | "))
      final <- dplyr::bind_rows(final, on)
    }
    #convert to get metrics defined previously
    final <- final %>%
      dplyr::mutate(
        ORTG = PTS / ePOSS * 100,
        DRTG = oPTS / ePOSS * 100,
        NETRTG = ORTG - DRTG,
        FG. = FGM / FGA,
        oFG. = oFGM / oFGA,
        TPP = TPM / TPA,
        oTPP = oTPM / oTPA,
        FTP = FTM / FTA,
        oFTP = oFTM / oFTA,
        eFG. = (FGM + 0.5 * TPM) / FGA,
        oeFG. = (oFGM + 0.5 * oTPM) / oFGA,
        TS. = (PTS / 2) / (FGA + .475 * FTA),
        oTS. = (oPTS / 2) / (oFGA + .475 * oFTA),
        RIM. = RIMM / RIMA,
        oRIM. = oRIMM / oRIMA,
        MID. = (FGM - TPM - RIMM) / (FGA - TPA - RIMA),
        oMID. = (oFGM - oTPM - oRIMM) / (oFGA - oTPA - oRIMA),
        TPrate = TPA / FGA,
        oTPrate = oTPA / oFGA,
        RIMrate = RIMA / FGA,
        oRIMrate = oRIMA / oFGA,
        MIDrate = (FGA - RIMA - TPA) / FGA,
        oMIDrate = (oFGA - oRIMA - oTPA) / oFGA,
        FTrate = FTA / FGA,
        oFTrate = oFTA / oFGA,
        ASTrate = AST / FGM,
        oASTrate = oAST / oFGM,
        TOrate = TO / POSS,
        oTOrate = oTO / POSS,
        BLKrate = BLK / oFGA,
        oBLKrate = oBLK / FGA,
        ORB. = ORB / (ORB + oDRB),
        DRB. = DRB / (DRB + oORB),
        PACE = (Mins / ePOSS) * 30,
        ShotsPerPoss = 1 + (ORB - TO) / POSS,
        oShotsPerPoss = 1 + (oORB - oTO) / oPOSS,
      ) %>%
      dplyr::select(Status, Mins:oPTS, POSS:NETRTG, everything())
    final[is.na(final)] <- 0
    final[,2:69] <- apply(final[,2:69], 2, function(x){ifelse(is.infinite(x),0,x)})

    return(final)
  }

#' Player Lineup Finder
#'
#' This function finds all lineups from a given lineup data source that include/exclude certain players. It acts
#' as a quick way to filter lineups for players
#' @param Lineup_Data a data frame of lineups created from get_lineups()
#' @param Included a character vector of players to be included in all lineups
#' @param Excluded a character vector of players to be excluded from all lineups
#' @import dplyr
#' @export
#' @return data frame of lineups with statistics documented in get_lineups()
get_player_lineups <-
  function(Lineup_Data = NA,
           Included = NA,
           Excluded = NA) {

    if(any(is.na(Included)) & any(is.na(Excluded))) {
      return(Lineup_Data)
    }

    #Figures out team that is being looked for from the vectors of players
    # find_team <- unique(
    #   dplyr::filter(
    #     Lineup_Data,
    #     P1 %in% Included |
    #       P2 %in% Included |
    #       P3 %in% Included |
    #       P4 %in% Included |
    #       P5 %in% Included |
    #       P1 %in% Excluded |
    #       P2 %in% Excluded |
    #       P3 %in% Excluded |
    #       P4 %in% Excluded |
    #       P5 %in% Excluded
    #   )$Team
    # )
    #
    # if (length(find_team) > 1) {
    #   stop("ERROR- MULTIPLE TEAMS SELECTED")
    # }

    # #get all lineups at first
    # data <- Lineup_Data %>%
    #   dplyr::filter(Team == find_team)

    #create variable storing whether the lineup includes/excludes correct players
    relRow <- rep(T , nrow(Lineup_Data))
    relRow2 <- rep(T , nrow(Lineup_Data))
    #iterates through included and finds rows that are needed
    if (!any(is.na(Included))) {
      for (i in 1:length(Included)) {
        relRow <-
          relRow * apply(Lineup_Data, 1, function(x)
            (Included[i] %in% x[1:5]))
      }
    }
    #iterates through rows for excluded and finds needed
    if (!any(is.na(Excluded))) {
      for (i in 1:length(Excluded)) {
        relRow2 <-
          relRow2 * apply(Lineup_Data, 1, function(x)
            (!Excluded[i] %in% x[1:5]))
      }
    }
    #take all rows where both cases are true
    new_df <-Lineup_Data[which(relRow == 1 & relRow2 == 1),]
    return(new_df)
}

#' Player Stats Calculator
#'
#' This function calculates many player stats for either individual games or aggregate to get multi-game stats.
#' @param play_by_play_data a data frame of play-by-play data from either get_play_by_play() or scrape_game()
#' @param keep.dirty logical object to remove entries from potentially corrupted games. Explained in get_lineups()
#' @param garbage.filter logical object to remove garbage time minutes from the sample. Explained in get_lineups()
#' @param error.thresh determine how many discrepancies can still occur for a game to be included. Explained in get_lineups()
#' @param multi.games Logical object. When false stats will be calculated on a game level. When true all games will be aggregated
#' this can be used to get season or multi-game player stats. Defaults to FALSE.
#' @return dataframe with each row as a player game or multi-game, with accompanying stats and details. Stats explained in get_lineups
#' but now at the individual player level rather than lineup level. New variables in use:
#' \itemize{
#' \item{GS} - Game Score: PTS + 0.4 x FGM - 0.7 x FGA - 0.4 x (FTA - FTM) + 0.7 x ORB + 0.3 x DRB + STL + 0.7 x AST + 0.7 x BLK - 0.4 x PF - TOV
#' \item{PBACK} - Successful putback: A rim attempt made preceded by an offensive rebound from the same player
#' @export
#' @import dplyr
get_player_stats <-
  function(play_by_play_data = NA,
           keep.dirty = F,
           garbage.filter = F,
           error.thresh = 5,
           multi.games = F) {

    # Uses same filtering of lineups found in documentation for get lineups
    if (length(play_by_play_data) == 0) {
      message("INPUT NOT FOUND")
      return(NULL)
    }
    if (keep.dirty == F) {
      player_filtered <- play_by_play_data %>%
        dplyr::filter(Sub_Deviate <= error.thresh)
      if(nrow(player_filtered) != nrow(play_by_play_data)) {
        message(paste0(round((nrow(play_by_play_data) - nrow(player_filtered)) /
                               nrow(play_by_play_data), 2
        ) * 100, "% dirty entries removed"))
      }
    } else{
      player_filtered <- play_by_play_data
    }
    if (garbage.filter == T) {
      total <- nrow(player_filtered)
      player_filtered <- player_filtered %>%
        dplyr::group_by(ID) %>%
        dplyr::mutate(
          Garbage_Thresh = dplyr::case_when(
            abs(Home_Score - Away_Score) >= 25 & Game_Seconds >= 1500 ~ T,
            abs(Home_Score - Away_Score) >= 20 &
              Game_Seconds >= 1800  ~ T,
            abs(Home_Score - Away_Score) >= 15 &
              Game_Seconds >= 2160  ~ T,
            TRUE ~ F
          ),
          Garbage_Time = cumsum(Garbage_Thresh) >= 1
        ) %>%
        dplyr::filter(Garbage_Time == F) %>%
        dplyr::select(-Garbage_Thresh, -Garbage_Time) %>%
        dplyr::ungroup()

      if(nrow(player_filtered) != total) {
        message(paste0(round((
          total - nrow(player_filtered)
        ) / total, 2) * 100, "% garbage time entries removed"))
      }
    }

    ### Individual results
    #First calculates main counting stats at a game level for each player
    player_stats <- player_filtered %>%
      dplyr::group_by(ID, Date, Home, Away, Event_Team, Player_1) %>%
      dplyr::summarise(
        PTS = sum((Event_Result == "made") * Shot_Value, na.rm = T),
        FGA = sum((Shot_Value %in% c(2, 3)), na.rm = T),
        FGM = sum((Shot_Value %in% c(2, 3)) * (Event_Result == "made"), na.rm = T),
        TPA = sum((Shot_Value == 3), na.rm = T),
        TPM = sum((Shot_Value == 3) * (Event_Result == "made"), na.rm = T),
        RIMA = sum((
          Event_Type %in% c("Dunk", "Layup", "Hook", "Tip-In")
        ), na.rm = T),
        RIMM = sum((
          Event_Type %in% c("Dunk", "Layup", "Hook", "Tip-In") * (Event_Result == "made")
        ), na.rm = T),
        PBACKA = sum((Event_Type %in% c("Dunk", "Layup", "Hook", "Tip-In")) * (lag(Event_Type) == "Offensive Rebound") * (lag(Player_1) == Player_1),
                     na.rm = T
        ),
        PBACKM = sum((Event_Type %in% c("Dunk", "Layup", "Hook", "Tip-In")) * (Event_Result == "made") * (lag(Event_Type) == "Offensive Rebound") * (lag(Player_1) == Player_1),
                    na.rm = T
        ),
        FTA = sum((Shot_Value == 1), na.rm = T),
        FTM = sum((Shot_Value == 1) * (Event_Result == "made"), na.rm = T),
        ORB = sum((Event_Type == "Offensive Rebound"), na.rm = T),
        DRB = sum((Event_Type == "Defensive Rebound"), na.rm = T),
        TOV = sum((Event_Type == "Turnover"), na.rm = T),
        STL = sum((Event_Type == "Steal"), na.rm = T),
        BLK = sum((Event_Type == "Blocked Shot"), na.rm = T),
        PF = sum((Event_Type == "Commits Foul"), na.rm = T)
      ) %>%
      dplyr::filter(Player_1 != "TEAM") %>%
      dplyr::rename(Player = Player_1,
             Team = Event_Team) %>%
      dplyr::ungroup()

    # Can then count assists form player 2 column
    assist_stats <- player_filtered %>%
      dplyr::group_by(ID, Player_2) %>%
      dplyr::summarise(AST = n()) %>%
      dplyr::rename(Player = Player_2) %>%
      dplyr::ungroup()

    # passes in pbp to calculate minutes for each player using extraneous function
    minutes <- get_mins(player_filtered)

    # merges all dataframes together by game and player
    # calculates some game level stats of use
    final_stats <-
      dplyr::left_join(player_stats, assist_stats, by = c("Player", "ID")) %>%
      dplyr::left_join(minutes, by = c("Player", "ID"))

    final_stats$AST[is.na(final_stats$AST)] <- 0
    final_stats <- final_stats %>%
      dplyr::mutate(
        FG. = FGM / FGA,
        TP. = TPM / TPA,
        FT. = FTM / FTA,
        TS. = (PTS / 2) / (FGA + .475 * FTA),
        eFG. = (FGM + 0.5 * TPM) / FGA,
        RIM. = RIMM / RIMA,
        MIDA = FGA - TPA - RIMA,
        MIDM = FGM - TPM - RIMM,
        MID. = (FGM - RIMM - TPM) / (FGA - RIMA - TPA),
        PBACK. = PBACKM / PBACKA,
        GS = PTS + 0.4 * FGM - 0.7 * FGA - 0.4 * (FTA - FTM) + 0.7 *
          ORB + 0.3 * DRB + STL + 0.7 * AST + 0.7 * BLK - 0.4 * PF - TOV
      ) %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::select(
        ID:Player, MINS, POSS, FGM, FGA, FG.,
        TPM, TPA, TP., FTM, FTA, FT., RIMM, RIMA, RIM., MIDM, MIDA, MID.,
        TS., eFG., PBACKM, PBACKA, PBACK., ORB, DRB, AST, STL, BLK, TOV,
        PF, PTS, GS
      )
    final_stats[is.na(final_stats)] <- 0

    # User has option to aggregate game stats into player stats over all games in play by play
    # This essentially does the same processes as above but changes the grouping to exclude game specific ids
    if (multi.games == T) {
      multi_game <- final_stats %>%
        dplyr::group_by(Player, Team) %>%
        dplyr::mutate(GP = n()) %>%
        dplyr::group_by(Player, Team, GP) %>%
        dplyr::summarise_if(is.numeric, sum) %>%
        dplyr::mutate(
          FG. = FGM / FGA,
          TP. = TPM / TPA,
          FT. = FTM / FTA,
          TS. = (PTS / 2) / (FGA + .475 * FTA),
          eFG. = (FGM + 0.5 * TPM) / FGA,
          RIM. = RIMM / RIMA,
          MID. = (FGM - RIMM - TPM) / (FGA - RIMA - TPA),
          PBACK. = PBACKM / PBACKA,
          GS = PTS + 0.4 * FGM - 0.7 * FGA - 0.4 * (FTA - FTM) + 0.7 *
            ORB + 0.3 * DRB + STL + 0.7 * AST + 0.7 * BLK - 0.4 * PF - TOV
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate_if(is.numeric, round, 3) %>%
        dplyr::select(
          Player, Team, GP, MINS, POSS, FGM, FGA, FG.,
          TPM, TPA, TP., FTM, FTA, FT., RIMM, RIMA, RIM., MIDM, MIDA, MID.,
          TS., eFG., PBACKM, PBACKA, PBACK., ORB, DRB, AST, STL, BLK, TOV,
          PF, PTS, GS
        )

      multi_game[is.na(multi_game)] <- 0
      return(multi_game)
    } else{
      return(final_stats)
    }
}

convert_events <- function(events) {
  #passes in all idenitified types of events in V2 pbp type and converts it to grammar used in V1 and adopted by this scraper
  events2 <- case_when(
    grepl("2pt", events) &
      grepl("jumpshot", events) &
      grepl("missed", events) ~ "missed Two Point Jumper",
    grepl("3pt", events) &
      grepl("jumpshot", events) &
      grepl("missed", events) ~ "missed Three Point Jumper",
    grepl("2pt", events) &
      grepl("jumpshot", events) &
      grepl("made", events) ~ "made Two Point Jumper",
    grepl("3pt", events) &
      grepl("jumpshot", events) &
      grepl("made", events) ~ "made Three Point Jumper",
    grepl("rebound defensivedeadball", events) ~ "Deadball Rebound",
    grepl("rebound offensivedeadball", events) ~ "Deadball Rebound",
    grepl("rebound defensive", events) ~ "Defensive Rebound",
    grepl("layup", events) &
      grepl("missed", events) ~ "missed Layup",
    grepl("layup", events) &
      grepl("made", events) ~ "made Layup",
    grepl("steal", events) ~ "Steal",
    grepl("foulon", events) ~ "Draw Foul",
    grepl("assist", events) ~ "Assist",
    grepl("foul ", events) ~ "Commits Foul",
    grepl("substitution out", events) ~ "Leaves Game",
    grepl("substitution in", events) ~ "Enters Game",
    grepl("hookshot", events) & grepl("made", events) ~ "made Hook",
    grepl("hookshot", events) &
      grepl("missed", events) ~ "missed Hook",
    grepl("freethrow", events) &
      grepl("made", events) ~ "made Free Throw",
    grepl("freethrow", events) &
      grepl("missed", events) ~ "missed Free Throw",
    grepl("timeout", events) ~ "Timeout",
    grepl("block", events) ~ "Blocked Shot",
    grepl("rebound offensive", events) ~ "Offensive Rebound",
    grepl("dunk", events) & grepl("made", events) ~ "made Dunk",
    grepl("dunk", events) & grepl("missed", events) ~ "missed Dunk",
    grepl("rebound ", events) ~ "Deadball Rebound",
    grepl(" jumpball won", events) ~ "won Jumpball",
    grepl(" jumpball lost", events) ~ "lost Jumpball",
    grepl(" jumpball heldball", events) ~ "held Jumpball",
    grepl("alleyoop", events) & grepl("made", events) ~ "made Dunk",
    grepl("alleyoop", events) &
      grepl("missed", events) ~ "missed Dunk",
    grepl("turnover", events) ~ "Turnover",
    is.na(events) ~ NA_character_,
    #if this comes up, I have not discovered the event and need to classify and convert it
    TRUE ~ "ERROR CHECK THE EVENT"
  )
  return(events2)
}
order_seconds <- function(by_second_pbp) {
  # Ordering the play by play to handle when enterer uses different ordering
  # to calculate assists, needs to format as shot attempt / assist / events / subs
  # this typically follows the true progression of events and makes it easier to follow along

  # If the block of events in each team/second/score has a shot attempt and an assist
  df1 <-
    if (sum(
      c(
        "Assist",
        "Three Point Jumper",
        "Two Point Jumper",
        "Layup",
        "Hook",
        "Dunk",
        "Tip In"
      ) %in% by_second_pbp$Event_Type
    ) == 2) {
      #finds the shot entry
      shot <-
        by_second_pbp[which(
          by_second_pbp$Event_Type %in% c(
            "Three Point Jumper",
            "Two Point Jumper",
            "Layup",
            "Hook",
            "Dunk",
            "Tip In"
          )
        ),]
      #finds the assist entry
      assist <-
        by_second_pbp[which(by_second_pbp$Event_Type == "Assist"),]
      #finds all other entries
      everything_else <-
        by_second_pbp[which(
          !by_second_pbp$Event_Type %in% c(
            "Assist",
            "Three Point Jumper",
            "Two Point Jumper",
            "Layup",
            "Hook",
            "Dunk",
            "Tip In"
          )
        ),]
      #reorderering a new dataframe to go shot/assist/everything else
      together <- rbind(shot, assist)
      together <- rbind(together, everything_else)
      together
    } else{
      by_second_pbp
    }
  #now look to see if the event includes a substitution
  df2 <-
    if (sum(c("Leaves Game", "Enters Game") %in% df1$Event_Type) == 2 &
        length(unique(df1$Event_Type)) > 2) {
      #first find subs
      subs <-
        df1[which(df1$Event_Type %in% c("Leaves Game", "Enters Game")),]
      #get all else
      everything_else <-
        df1[which(!df1$Event_Type %in% c("Leaves Game", "Enters Game")),]
      #put it all together
      together <- rbind(everything_else, subs)
      together
    } else {
      df1
    }
  #return resulting ordered dataframe
  return(df2)
}
get_mins <- function(player_filtered) {
  #since players can appear in 10 different columns, iterate through each to calculate on court stats efficiently
  #create column names for all of the home/away matrix
  cols <- paste0(rep(c("Home", "Away"), each = 5), ".", rep(1:5, 2))
  #will store player calculatings for each column
  player_data <- data.frame()
  #iterate through each player column
  for (i in 1:10) {
    #group by the given player column in iteration
    #calculate stats needed to get the players minutes and possessions on the court for
    player <- player_filtered %>%
      dplyr::group_by_at(vars(cols[i], "ID")) %>%
      dplyr::summarise(
        Mins = sum(Event_Length, na.rm = T) / 60,
        FGA = sum((Shot_Value %in% c(2, 3)) * (Event_Team == Home) *
                    1, na.rm = T),
        oFGA = sum((Shot_Value %in% c(2, 3)) * (Event_Team == Away) *
                     1, na.rm = T),
        TO = sum((Event_Type == "Turnover") * (Event_Team == Home) *
                   1, na.rm = T),
        oTO = sum((Event_Type == "Turnover") * (Event_Team == Away) *
                    1, na.rm = T),
        ORB = sum((Event_Type == "Offensive Rebound") * (Event_Team == Away) *
                    1,
                  na.rm = T
        ),
        oORB = sum((Event_Type == "Offensive Rebound") * (Event_Team == Home) *
                     1,
                   na.rm = T
        ),
        FTA = sum((Shot_Value == 1) * (Event_Team == Away) * 1, na.rm = T),
        oFTA = sum((Shot_Value == 1) * (Event_Team == Home) * 1, na.rm = T)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::rename(Player =  cols[i]) %>%
      dplyr::mutate(POSS = (FGA + .475 * FTA - ORB + TO + oFGA + .475 * oFTA - oORB + oTO) /
               2) %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::ungroup()
    #once this is calculated add to the data frame of all player columns
    player_data <- rbind(player_data, player)
  }
  #can now group by each player name and get their total minutes and possessions
  final_df <- player_data %>%
    dplyr::group_by(Player, ID) %>%
    dplyr::summarise(MINS = sum(Mins),
              POSS = ceiling(sum(POSS))) %>%
    dplyr::ungroup()
}
binder <- dplyr::bind_rows

#' Minutes Distribution Plot
#'
#' This function takes in play by play data and a team name and returns a plot
#' showing the distributon of when each player was on the court
#' @param pbp_data a data frame of pbp created from get_play_by_play() / scrape_game()
#' @param team a single team name
#' @param threshold minimum minutes played to include in plot
#' @import dplyr
#' @import ggplot2
#' @export
#' @return ggplot object containing minutes distribution

plot_mins_dist <- function(play_by_play_data = NA, team = NA, threshold = NA, split_position = F) {
  if(all(is.na(play_by_play_data)) | is.na(team)) {
    stop("Missing parameters")
  }

  if(is.na(threshold)) {
    threshold <- 0
  }

  home_team <- play_by_play_data %>%
    dplyr::filter(Home == team) %>%
    dplyr::select(Home,Away,Game_Seconds, Home.1:Home.5) %>%
    dplyr::mutate(Game_Mins = Game_Seconds %/% 60)
  away_team <- play_by_play_data %>%
    dplyr::filter(Away == team) %>%
    dplyr::select(Home,Away,Game_Seconds,Away.1:Away.5) %>%
    dplyr::mutate(Game_Mins = Game_Seconds %/% 60)

  all_players <- data.frame(Game_Mins = NA, Player = NA, Home = NA, Away = NA)
  for(i in 1:5) {
    player_row <- home_team[,c(i+3,9,1,2)]
    colnames(player_row)[1] <- "Player"
    all_players <- rbind(all_players, player_row)
    player_row <- away_team[,c(i+3,9,1,2)]
    colnames(player_row)[1] <- "Player"
    all_players <- rbind(all_players, player_row)
  }

  player_mins <- all_players %>%
    dplyr::group_by(Player, Game_Mins, Home, Away) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::group_by(Player, Game_Mins) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::filter(!is.na(Player), !is.na(Game_Mins))

  totals <- expand.grid(unique(player_mins$Player), 0:40, stringsAsFactors = F)
  colnames(totals) <- c("Player", "Game_Mins")
  totals <- left_join(totals, player_mins, by = c("Player", "Game_Mins"))
  totals$count[is.na(totals$count)] <- 0

  labels <- sapply(totals$Player, function(x){
    spl <- strsplit(x, split = "[.]")[[1]]
    first <- paste0(substr(spl[1],1,1), tolower(substr(spl[1],2,nchar(spl[1]))))
    last <- paste0(substr(spl[2],1,1), tolower(substr(spl[2],2,nchar(spl[2]))))
    if(length(spl) > 2) {
      paste(first, last, spl[3:length(spl)])
    } else {
      paste(first,last)
    }
  }, USE.NAMES = F)

  if(split_position) {
    year <- substr(first(play_by_play_data$Date),7,10)
    month <- substr(first(play_by_play_data$Date),1,2)
    year <- ifelse(as.numeric(month)<=5, as.numeric(year)-1, year)

    season <- paste0(as.numeric(year), "-", as.numeric(year)-1999)
    roster <- get_team_roster(team.name = team, season = season)
    totals <- left_join(totals, roster, by = "Player")
    totals$CleanName <- paste(totals$Jersey,"-", totals$CleanName)
  }

  totals$CleanName <- if(is.null(totals$CleanName)) labels else totals$CleanName
  totals$CleanName <- ifelse(totals$CleanName == "NA - NA", labels, totals$CleanName)

  p <- totals %>%
    dplyr::group_by(CleanName) %>%
    dplyr::mutate(Total_Mins = sum(count)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Total_Mins > threshold) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(reorder(CleanName, Total_Mins), Game_Mins, fill = count)) +
    ggplot2::coord_flip() +
    ggplot2::geom_hline(yintercept = seq(0,40,by=10), linetype = "dashed", color = "darkgrey") +
    ggplot2::scale_fill_gradient2(low = "white", high = "steelblue") +
    ggplot2::labs(x = "", y = "Minute", fill = "GP", caption = "Jake Flancer (@JakeFlancer) | Data: NCAA.com") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "gray75"),
      panel.background = ggplot2::element_rect(fill = "gray75"),
      legend.background = ggplot2::element_rect(fill = "gray75"),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "Helvetica", color = "gray25", face = "bold")) +
    ggplot2::ggtitle(paste(team,"minutes distribution"))

  if(split_position) {
    p +
      ggplot2::facet_wrap(.~Pos, ncol = 1, scales = "free_y") +
      ggplot2::theme(strip.background = ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(family = "Helvetica", color = "gray25", face = "bold"))
  } else {
    p
  }

}

#' Duo Plot
#'
#' Creates network plot of team ratings with players together on the court
#' Uses empirical bayesian formula to estimate ratings relative to team average:
#' regressed ORTG = (n_bar*r_bar_o + PTS) / (n_bar + POSS)
#' n_bar = average possessions per lineup + regressed possessions
#' r_bar_o = team offensive rating (PTS/POSS)*100
#' @param lineup_data a data frame of lineups created from get_lineups()
#' @param team a single team name
#' @param min_mins minimum number of minutes required for duo to play together
#' @param regressed_poss effectively number of team average possessions used to "shrink" ratings
#' @import dplyr
#' @import ggplot2
#' @importFrom gtools combinations
#' @importFrom igraph graph_from_data_frame
#' @import ggraph
#' @importFrom ggraph guide_edge_colourbar
#' @export
#' @return ggplot object of network plot
plot_duos <- function(Lineup_Data = NA, team = NA, min_mins = 0, regressed_poss = 50) {
  if(all(is.na(Lineup_Data)) | is.na(team)) {
    stop("Missing Function Parameters")
  }

  lineup_data <- dplyr::filter(Lineup_Data, Team == team)

  players <- unique(unlist(lineup_data[,1:5]))
  players <- players[!is.na(players)]

  labels <- sapply(players, function(x){
    spl <- strsplit(x, split = "[.]")[[1]]
    first <- paste0(substr(spl[1],1,1), tolower(substr(spl[1],2,nchar(spl[1]))))
    last <- paste0(substr(spl[2],1,1), tolower(substr(spl[2],2,nchar(spl[2]))))
    if(length(spl) > 2) {
      paste(first, last, spl[3:length(spl)])
    } else {
      paste(first,last)
    }
  })

  r_bar_o <- (sum(lineup_data$PTS) / sum(lineup_data$ePOSS))
  r_bar_d <- (sum(lineup_data$oPTS) / sum(lineup_data$ePOSS))
  n_bar <- mean(lineup_data$ePOSS) + regressed_poss

  duos <- data.frame(gtools::combinations(n=length(players), r=2, v=players, repeats.allowed = F))
  colnames(duos) <- c("from","to")
  duos$mins <- NA
  duos$ortg <- NA
  duos$drtg <- NA
  for(i in 1:nrow(duos)){
    tmp <- get_player_lineups(lineup_data, Included = unlist(unname(duos[i,1:2])))
    duos$mins[i] <- sum(tmp$Mins)
    duos$ortg[i] <- sum(tmp$PTS) / sum(tmp$ePOSS)
    duos$drtg[i] <- sum(tmp$oPTS) / sum(tmp$ePOSS)
    duos$adjortg[i] <- (n_bar*r_bar_o + sum(tmp$PTS)) / (n_bar + sum(tmp$ePOSS))
    duos$adjdrtg[i] <- (n_bar*r_bar_d + sum(tmp$oPTS)) / (n_bar + sum(tmp$ePOSS))
  }
  duos[,1:2] <- apply(duos[,1:2],2,as.character)
  duos$netrtg <- duos$ortg - duos$drtg
  duos$adjrtg <- (duos$adjortg - duos$adjdrtg)

  scale <- round(c(min(duos$adjrtg)-.02,mean(duos$adjrtg),max(duos$adjrtg)+.02),2)

  dataset <- dplyr::filter(duos, mins >= min_mins)

  if(nrow(dataset) <= 0) {
    print("No Duos Found- Try Lowering Minutes")
    return(data.frame())
  }

  final_players <- unique(unlist(dataset[,1:2]))

  grph <- igraph::graph_from_data_frame(dataset,
                                directed = F,
                                vertices = data.frame(name = sort(final_players),
                                                      lab = sort(labels[which(names(labels) %in% sort(final_players))]))
  )

  ggraph::ggraph(grph, layout = "linear", circular = T) +
    ggraph::geom_edge_arc(ggplot2::aes(edge_width = mins,
                      color = adjrtg*100)) +
    ggraph::scale_edge_colour_gradientn(breaks = scale*100,
                               colors = c("steelblue","white","indianred"),
                               labels = scale*100,
                               name = "Adj. Net Efficiency per 100",
                               limits = c(min(scale)*100,max(scale)*100),
                               na.value = "transparent",
                               guide = ggraph::guide_edge_colorbar()) +
    ggraph::scale_edge_width(name = "Minutes Together") +
    ggraph::geom_node_text(ggplot2::aes(label = lab), color = "gray25", size = 4.5, fontface = "bold") +
    ggraph::geom_node_point(size = 15, alpha = 0.1, color = "gray50") +
    ggraph::theme_graph() +
    ggplot2::labs(title = paste(team, "Duos Performance"),
                  subtitle = paste("team performance when pairs of players are on the court together, min.", min_mins, "minutes"),
                  caption = "Jake Flancer (@JakeFlancer) | Data: NCAA.com") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0, size = 18, family = "Helvetica", color = "gray25", face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12, family = "Helvetica", color = "gray25", face = "bold"),
      plot.background = ggplot2::element_rect(fill = "gray75"),
      plot.caption = ggplot2::element_text(family = "Helvetica", color = "gray25", face = "bold"),
      legend.title = ggplot2::element_text(family = "Helvetica", color = "gray25", face = "bold")
      ) +
    ggplot2::scale_x_continuous(expand = c(.15, .15), limits = c(-1,1)) +
    ggplot2::scale_y_continuous(expand = c(.15, .15), limits = c(-1,1))
}
