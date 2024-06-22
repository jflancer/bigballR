## Test all functions ################

x <- get_date_games("04/02/2024")

team_sched <- get_team_schedule(season = '2023-24', team.name = 'Purdue')
team_sched
# team_sched <- get_team_schedule(season = '2023-24', team.name = 'Colorado')
# team_sched

df <- get_play_by_play(team_sched$Contest_ID[1])

scrape_game(team_sched$Contest_ID[nrow(team_sched)])

get_player_stats(play_by_play_data = df)

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
    seasonid <- dplyr::case_when(
      # 23-24
      dateform > as.Date("2023-05-01") &
        dateform <= as.Date("2024-05-01") ~ 18221,
      # 22-23
      dateform > as.Date("2022-05-01") &
        dateform <= as.Date("2023-05-01") ~ 17940,
      # 21-22
      dateform > as.Date("2021-05-01") &
        dateform <= as.Date("2022-05-01") ~ 17783,
      # 20-21
      dateform > as.Date("2020-05-01") &
        dateform <= as.Date("2021-05-01") ~ 17420,
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
        dateform <= as.Date("2012-05-01") ~ 10480,
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
    url_text <-
      paste0(
        "http://stats.ncaa.org/season_divisions/",
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
      file_url <- url(url_text, headers = c("User-Agent" = "My Custom User Agent"))
      html <- readLines(con = file_url, warn=F)
      close(file_url)

      dir.create(file_dir, recursive = T, showWarnings = F)
      writeLines(html, file_path)
    }

    # Reads the html and pulls the table holding the scores
    if (use_file & !is.na(base_path)) {
      html <- readLines(file_path, warn=F)
    } else {
      file_url <- url(url_text, headers = c("User-Agent" = "My Custom User Agent"))
      html <- readLines(con = file_url, warn=F)
      close(file_url)
    }

    table <- XML::readHTMLTable(html)
    if(length(table) == 0) {
      stop("No Games Table Found")
    } else {
      table <- table[[1]]
    }

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

    # sees if a box score is available for each game
    box_score_present <- as.character(table$V1[starting_rows + 4]) == "Box Score"

    #This searches for all game IDs on the schedule page, using links found in the html
    game_ids <-
      unlist(stringr::str_extract_all(html, "(?<=/contests/)\\d+(?=/box_score)"))
    game_ids <- game_ids[which(!game_ids %in% seasonid)]

    # Handle cancelled games with missing game ids, or if game is missing a box score
    id_found <- rep(NA, length(away_score))
    id_found[!away_score %in% c("Canceled", "Ppd") & box_score_present] <- game_ids

    #Also creates variable used to find if a game was held at a neutral side
    isNeutral <- table$V6[starting_rows] != ""

    #Informs user of how many games and games with a relevant ID were found
    message(paste(date, "|", length(game_ids), "games found"))

    # Unfortunately, the game ID for boxscore isn't the same as the game ID for pbp
    # As a result, this function needs to convert from boxscore to pbp but as a result be slower
    # Need to read each box score page and find link to pbp page

    url2 <-
      paste0("http://stats.ncaa.org/contests/", id_found, "/box_score")

    # Clean team names
    home_name = gsub(" [(].*[)]","", home_team)
    home_wins = as.vector(stringr::str_extract_all(home_team, "(?<=[(])\\d+(?=-)", T))
    home_losses = as.vector(stringr::str_extract_all(home_team, "(?<=-)\\d+(?=[)])", T))

    away_name = gsub(" [(].*[)]","", away_team)
    away_wins = as.vector(stringr::str_extract_all(away_team, "(?<=[(])\\d+(?=-)", T))
    away_losses = as.vector(stringr::str_extract_all(away_team, "(?<=-)\\d+(?=[)])", T))

    if(length(home_wins) == 0){home_wins = NA}
    if(length(home_losses) == 0){home_losses = NA}
    if(length(away_wins) == 0){away_wins = NA}
    if(length(away_losses) == 0){away_losses = NA}

    #Create dataframe
    game_data <- data.frame(
      Date = substr(game_date, 1, 10),
      Start_Time = substr(game_date, 12, 19),
      Home = home_name,
      Away = away_name,
      BoxID = id_found,
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
    if(length(id_found)>0){
      pb = txtProgressBar(min = 0, max = length(id_found), initial = 0)
      for (i in 1:length(id_found)) {
        if(url2[i] !=  "http://stats.ncaa.org/contests/NA/box_score") {
          file_dir <- paste0(base_path, "box_score/")
          file_path <- paste0(file_dir, id_found[i], ".html")
          isUrlRead <- F

          # Assumes that if pbp is available from file it will always be used rather than re-scraping
          if (!is.na(base_path) & file.exists(file_path)) {
            temp_html <- readLines(file_path, warn=F)
          } else {
            isUrlRead <- T
            file_url <- url(url2[i], headers = c("User-Agent" = "My Custom User Agent"))
            temp_html <- readLines(con = file_url, warn=F)
            close(file_url)
          }

          # Give user option to save raw html file (to make future processing more efficient)
          if (save_file & !is.na(base_path) & !file.exists(file_path)) {
            dir.create(file_dir, recursive = T, showWarnings = F)
            writeLines(temp_html, file_path)
          }

          new_id <- unlist(stringr::str_extract(temp_html, "(?<=play_by_play/)\\d+"))
          new_id <- unique(new_id[!is.na(new_id)])
          if (length(new_id) == 1) {
            game_data$GameID[i] <- new_id
          }
          if(isUrlRead) {
            Sys.sleep(0.5)
          }
          setTxtProgressBar(pb,i)
        }
      }
      close(pb)
    } else {
      message("No Game IDs Found")
    }

    return(game_data)
}
