# use this for existing old game
game_id <- "3954945"

# use this for new game
game_id <- "5254137"
save_file <- F
use_file <- F
base_path <- NA
overwrite <- F

scrape_game <- function(game_id, save_file=F, use_file=F, base_path = NA, overwrite=F) {

  #track status of cleanliness of data for game
  status <- "CLEAN"

  base_url <- "https://stats.ncaa.org/game/play_by_play/"
  url_text <- paste0(base_url, game_id)

  # new
  # base_url <- "https://stats.ncaa.org/contests/game_id/play_by_play/"
  # url_text <- glue::glue("https://stats.ncaa.org/contests/{game_id}/play_by_play/")
  file_dir <- paste0(base_path, "play_by_play/")
  file_path <- paste0(file_dir, game_id, ".html")
  isUrlRead <- F

  # Give user option to save raw html file (to make future processing more efficient)
  if (save_file & !is.na(base_path) & (!file.exists(file_path) | overwrite)) {
    isUrlRead <- T
    file_url <- url(url_text, headers = c("User-Agent" = "My Custom User Agent"))
    html <- readLines(con = file_url, warn=F)
    close(file_url)
    dir.create(file_dir, recursive = T, showWarnings = F)
    writeLines(html, file_path)
  } else if (file.exists(file_path) & use_file) {
    html <- readLines(file_path, warn=F)
  } else {
    isUrlRead <- T
    file_url <- url(url_text, headers = c("User-Agent" = "My Custom User Agent"))
    html <- readLines(con = file_url, warn=F)
    close(file_url)
  }

  table <- XML::readHTMLTable(html)

  # temp
  # old_table <- table
  # new_table <- table
  # table <- new_table

  if (length(table) == 0) {
    message("Game Not Found")
    return(data.frame())
  }

  # Pull scores for each half
  # half_scores <- table[[1]]
  half_scores <- table[[2]][1:2,]

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
          first_half[1, 2] == "period start"|
          first_half[1,2] == "jumpball startperiod")) |
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
    player_name <- gsub("(\\.JR\\.|\\.SR\\.|\\.J\\.R\\.|\\.JR\\.|JR\\.|SR\\.|\\.SR|\\.JR|\\.SR|\\.III|\\.II|\\.IV)$","", player_name)
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
    player_name <- ifelse(substr(player_name, 1, 2) == ".T", "TEAM", player_name)
    player_name <- gsub("\\s+", ".", player_name)
    player_name <- gsub("(\\.JR\\.|\\.SR\\.|\\.J\\.R\\.|\\.JR\\.|JR\\.|SR\\.|\\.SR|\\.JR|\\.SR|\\.III|\\.II|\\.IV)$","", player_name)
  }

  # Now format controls for version

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
    dplyr::mutate(
      Event_Priority = case_when(
        Event_Type == "won Jumpball" ~ 1,
        Event_Type == "lost Jumpball" ~ 2,
        Event_Type == "Offensive Rebound" ~ 3,
        Event_Type %in% c(
          "Three Point Jumper",
          "Two Point Jumper",
          "Layup",
          "Hook",
          "Dunk",
          "Tip In"
        ) ~ 4,
        Event_Type == "Assist" ~ 5, # Order assist directly after shot so rows can be merged
        Event_Type == "Turnover" ~ 6,
        Event_Type == "Steal" ~ 7,
        !Event_Type %in% c("Enters Game", "Leaves Game") ~ 6,
        T ~ 7
      ),
      Home_Score = as.numeric(Home_Score),
      Away_Score = as.numeric(Away_Score)
    ) %>%
    # Function is in use as scorekeepers often don't follow same ordering
    # This formats as Row 1: Shot, Row 2: Assist, Next: Substitutions, Final: Everything Else
    # dplyr::group_by(Game_Seconds, Home_Score, Away_Score) %>%
    dplyr::arrange(Half_Status, Game_Seconds, Home_Score, Away_Score, Event_Priority) %>%
    # dplyr::do(order_seconds(.)) %>%
    # dplyr::ungroup() %>%
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
    dplyr::filter(Event_Type != "Assist") %>%
    dplyr::select(-Event_Priority) %>%
    dplyr::mutate(
      Poss_Num = NA,
      Poss_Team = NA,
      Event_Length = Game_Seconds - dplyr::lag(Game_Seconds),
      Event_Length = ifelse(is.na(Event_Length), Game_Seconds, Event_Length),
      Shot_Value = dplyr::case_when(
        Event_Type == "Two Point Jumper" ~ 2,
        Event_Type == "Layup" ~ 2,
        Event_Type == "Three Point Jumper" ~ 3,
        Event_Type == "Dunk" ~ 2,
        Event_Type == "Free Throw" ~ 1,
        Event_Type == "Tip In" ~ 2,
        Event_Type == "Hook" ~ 2
      )
    )

  # Calculating Possessions ####
  poss_num <- 0
  nrows <- 0
  for(i in 1:max(dirty_game$Half_Status)) {
    half_data <- dplyr::filter(dirty_game, Half_Status == i)
    poss_team <- dplyr::first(half_data$Event_Team[!half_data$Event_Type %in% c("Leaves Game", "Enters Game")])
    other_team <- ifelse(poss_team == home_team, away_team, home_team)
    poss_switch = F
    poss_num <- poss_num + 1

    for(j in 1:nrow(half_data)) {
      # If row has event that ends possession, increment num and swap team
      team <- half_data$Event_Team[j]
      type <- half_data$Event_Type[j]
      result <- half_data$Event_Result[j]
      seconds <- half_data$Game_Seconds[j]

      # Using max(j-1, 1) to handle first entry
      swap <- poss_switch & seconds != half_data$Game_Seconds[max(j-1, 1)]
      if(swap) {
        poss_num <- poss_num + 1
        tmp <- poss_team
        poss_team <- other_team
        other_team <- tmp
        poss_switch <- F
      }

      # force switch possession if event is attributed to wrong possession team, if parse is false this puts it back to correct
      # revisit - might be a more efficient way
      if (!is.na(result) & team != poss_team | type == "Turnover" & poss_team != team) {
        poss_num <- poss_num + 1 - swap*1
        tmp <- poss_team
        poss_team <- other_team
        other_team <- tmp
        poss_switch <- F
      }

      and_one <- any(half_data$Event_Type[half_data$Game_Seconds == seconds] == "Free Throw") # Detect and-one to not switch possession on the made shot
      next_reb <-half_data$Event_Type[j+1] %in% c("Defensive Rebound", "Free Throw") # Catch final free throw misses -- note free throw sequences are occassionally out of order in pbp

      if(
        type %in% c("Defensive Rebound", "Turnover") |
        (type %in% c("Two Point Jumper", "Three Point Jumper", "Layup", "Dunk", "Tip In", "Hook") & result == "made" & !and_one) |
        (type == "Free Throw" & result == "made" & !next_reb)
      ) {

        # Free throw made + current or next time does not include a defensive rebound
        poss_switch = T
      }

      dirty_game$Poss_Num[j+nrows] <- poss_num
      dirty_game$Poss_Team[j+nrows] <- poss_team
    }
    nrows <- nrows + nrow(half_data)
  }

  pos_start <- dirty_game %>%
    dplyr::group_by(Poss_Num) %>%
    dplyr::mutate(
      Terminal = last(Event_Type)
    ) %>%
    dplyr::summarise(End = any(Terminal %in% c("Two Point Jumper", "Three Point Jumper", "Free Throw",
                                               "Dunk", "Layup", "Hook", "Tip In", "Steal", "Defensive Rebound"
    ))*1, .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Valid = dplyr::lag(End, default =0)) %>%
    dplyr::select(Poss_Num, Valid)

  dirty_game <- dirty_game %>%
    dplyr::left_join(pos_start, by = "Poss_Num") %>%
    dplyr::group_by(Poss_Num) %>%
    dplyr::mutate(
      Poss_Length = cumsum(Event_Length),
      isTransition = (first(Poss_Length) <= 10 & (first(Event_Type) %in% c(
        "Steal", "Dunk", "Layup", "Hook", "Tip In",
        "Two Point Jumper", "Three Point Jumper") |
          first(Event_Type) == "Draws Foul" & Event_Team == Poss_Team |
          first(Event_Type) == "Commits Foul" & Event_Team != Poss_Team) & Valid)
    ) %>%
    dplyr::select(-Valid) %>%
    dplyr::ungroup()

  # Test case for if shots are attributed correctly
  test_poss <- dirty_game %>% group_by(Event_Team, Poss_Team, Half_Status) %>% summarise(Pts = sum(!is.na(Event_Result), na.rm=T), .groups = "keep") %>% arrange(Half_Status)
  if(sum(test_poss$Pts>0) != (2+numbOTs)*2) {
    message("Warning: Possession Parsing Has Errors")
  }

  # Detect games with invalid substitutions - rather than parsing clearly flawed data, return with no sub format
  player_subs <- dirty_game %>%
    filter(Event_Type %in% c("Enters Game", "Exits Game")) %>%
    .$Player_1

  # 1. If a number is used for a substitution
  # 2. If TEAM is used
  # 3. If a team name is used
  invalid_sub <- any(grepl(paste0("\\.[0-9]+|\\.TEAM|",toupper(home_team),"|", toupper(away_team)), player_subs))

  # Now Check to See if Players Were Recorded in the Game
  if (length(unique(dirty_game$Player_1)) == 1 | invalid_sub | length(player_subs) == 0) {

    # No Player Cleaning
    # Found no player names in data
    # Does final cleaning of data without finding on off
    # Gets the length of each event and assigns a shot value
    # Creates the final dataframe to be used without players
    clean_game <- dirty_game %>%
      dplyr::mutate(
        Status = "NO_PLAYER", #set status variable mentioned earlier
        Sub_Deviate = nrow(.)
      ) %>%
      bind_cols(as.data.frame(matrix(rep(NA, nrow(dirty_game)*10),
                                     ncol = 10,
                                     nrow = nrow(dirty_game))) %>%
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
        Poss_Num,
        Poss_Team,
        Poss_Length,
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
      "| Status: No Substitution Data",
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
      dplyr::summarise(count = dplyr::n(), .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::filter(count %% 2 != 0)
    # Report substitition mistake messages to users
    # Possibly find a way to deal with this but for now just reporting
    if (nrow(mins_errors) > 0) {
      # message(
      #   paste(
      #     "Potential Substitution Entry Mistakes within: Half",
      #     # FIX THIS TO JUST SHOW HALF NOT SPECIFIC TIME
      #     paste(unique(mins_errors$Game_Seconds %/% 1200)+1, collapse = ", ")
      #   )
      # )
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
                      # Player_1 != "TEAM",
                      Time != "00:00",
                      (Time != "20:00" & Half_Status %in% 1:2) | (Time != "05:00" & Half_Status >2)
        )$Player_1
      home_entering <-
        dplyr::filter(half_data,
                      Event_Team == Home,
                      Event_Type == "Enters Game",
                      # Player_1 != "TEAM",
                      Time != "00:00",
                      (Time != "20:00" & Half_Status %in% 1:2) | (Time != "05:00" & Half_Status >2)
        )$Player_1
      away_leaving <-
        dplyr::filter(half_data,
                      Event_Team == Away,
                      Event_Type == "Leaves Game",
                      # Player_1 != "TEAM",
                      Time != "00:00",
                      (Time != "20:00" & Half_Status %in% 1:2) | (Time != "05:00" & Half_Status >2)
        )$Player_1
      away_entering <-
        dplyr::filter(half_data,
                      Event_Team == Away,
                      Event_Type == "Enters Game",
                      # Player_1 != "TEAM",
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
          # message(
          #   paste(
          #     "Using approximate starter finder, choosing:\n",
          #     paste(all_starters[1:5], collapse = ", "),
          #     "\nfrom: ",
          #     paste(all_starters, collapse = ", ")
          #   )
          # )
          all_starters[1:5]
          # If 5, checks have successfully found 5 starters
        } else if(length(all_starters) == 5){
          all_starters[1:5]
          # Handle case when less than 5 starters are found even after error checks
        } else {
          # Just takes first n players that have recorded an event in the half
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
          # message(
          #   paste(
          #     "Using approximate starter finder, choosing:\n",
          #     paste(all_starters[1:5], collapse = ", "),
          #     "\nfrom: ",
          #     paste(all_starters, collapse = ", ")
          #   )
          # )
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
        home_enter_players <- home_enter_players[!is.na(home_enter_players)]
        home_exit_players <- home_exit_players[!is.na(home_exit_players)]
        away_enter_players <- away_enter_players[!is.na(away_enter_players)]
        away_exit_players <- away_exit_players[!is.na(away_exit_players)]
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
      dplyr::bind_cols(list(dirty_game, as.data.frame(home_player_matrix, row.names = F), as.data.frame(away_player_matrix, row.names = F))
      )

    #Add the event length variable which can often be helpful
    home_starters <- unlist(mild_game[1,23:27])
    away_starters <- unlist(mild_game[1,28:32])

    #Can now put together final data frame
    clean_game <- mild_game %>%
      dplyr::mutate_if(is.factor, as.character) %>%
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
        Poss_Num,
        Poss_Team,
        Poss_Length,
        isTransition,
        Home.1:Away.5
      ) %>%
      dplyr::mutate(
        Status = status,
        # Greater than 25 and 10-5 minutes left
        # Greater than 20 and 5-2 minutes left
        # Greater than 15 and <2 minutes left
        Garbage_Thresh = dplyr::case_when(
          abs(Home_Score - Away_Score) >= 25 & Game_Seconds >= 1800 ~ T,
          abs(Home_Score - Away_Score) >= 20 & Game_Seconds >= 2100  ~ T,
          abs(Home_Score - Away_Score) >= 15 & Game_Seconds >= 2280  ~ T,
          TRUE ~ F
        ),
        # Only call garbage time if <= 3 starters are in... note: Ben Falk / CTG uses 2
        Starter_Thresh = ((Home.1 %in% home_starters) + (Home.2 %in% home_starters) +
                            (Home.3 %in% home_starters) + (Home.4 %in% home_starters) + (Home.5 %in% home_starters) +
                            (Away.1 %in% away_starters) + (Away.2 %in% away_starters) + (Away.3 %in% away_starters) +
                            (Away.4 %in% away_starters) + (Away.5 %in% away_starters)) <= 3,
        # If both thresholds are met we hit garbage time and stay in it
        isGarbageTime = cumsum(Garbage_Thresh*Starter_Thresh) >= 1
      ) %>%
      dplyr::select(-Garbage_Thresh, -Starter_Thresh) %>%
      # Making fix so that the players on the court to start a possession are credited for the entire possession
      dplyr::group_by(Poss_Num) %>%
      dplyr::mutate(
        Home.1 = first(Home.1),
        Home.2 = first(Home.2),
        Home.3 = first(Home.3),
        Home.4 = first(Home.4),
        Home.5 = first(Home.5),
        Away.1 = first(Away.1),
        Away.2 = first(Away.2),
        Away.3 = first(Away.3),
        Away.4 = first(Away.4),
        Away.5 = first(Away.5)
      ) %>%
      dplyr::ungroup()

    # Final round of checking for data entry mistakes by scorekeeper
    # Look for if a player is said to do an event and they aren't on the court as determined above
    player_errors <- apply(clean_game, 1, function(x) {
      if (sum(x[which(colnames(clean_game) == "Player_1"):which(colnames(clean_game) == "Player_2")] %in% c(x[which(colnames(clean_game) == "Home.1"):which(colnames(clean_game) == "Away.5")]), na.rm = T) == 0) {
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
    # Warns user of number of entry mistakes found - only report if significant

    warn <- ifelse(nrow(entry_mistakes)>15, paste(nrow(entry_mistakes), "deviations"), "")
    source <- ifelse(isUrlRead, "web", "local")
    # Give user final message about the status of the game they've scraped
    message(paste(date, home_team, "v", away_team, "| ", format, "|", game_id, "|", source, "|", warn))
    # Sys.sleep so the ncaa server isn't overworked
    if(isUrlRead) {
      Sys.sleep(2)
    }

    return(clean_game)
  }
}
