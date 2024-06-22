team.id <- NA
season = '2023-24'
team.name = 'Purdue'
use_file <- F
save_file <- F
base_path <- NA
overwrite <- F

get_team_schedule <-
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

    # Pull the relevant table from the team webpage
    url_text <- paste0("https://stats.ncaa.org/teams/", team.id)
    file_dir <- paste0(base_path, "team_schedule/")
    file_path <- paste0(file_dir, team.id, ".html")

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

    tables <- XML::readHTMLTable(html)
    if(!is.null(tables[[1]])) {
      df <- data.frame(as.matrix(tables[[1]]), stringsAsFactors = F)
    } else {
      message(paste(team.id, "has no schedule"))
      return(data.frame())
    }

    df <- df[seq(1,nrow(df), by = 2),]
    df <- df[!is.na(df$Opponent),]

    # fix strings like "Campbell 2022-23 MBB App State MTE" and "UC Santa Barbara @Phoenix, AZ (2022-23 MBB Jerry Colangelo Classic)"
    df$Opponent_with_detail <- df$Opponent |>
      str_remove(" 202.*$")
    # Keep two different versions: df$Opponent_with_detail includes neutral site descriptions,
    # df$Opponent does not.
    df$Opponent <- df$Opponent_with_detail |>
      str_remove(" \\@[A-Z].*$")


    game_ids <-
      unlist(stringr::str_extract_all(html, "(?<=contests/)\\d+(?=[/])"))

    # # Game IDs links to box score game id, not play by play id
    # # Unfortunately need to now parse webpage for each game played to find game id
    # url2 <-
    #   paste0("https://stats.ncaa.org/contests/", game_ids, "/box_score")
    #
    # new_ids <- c()
    # message("Compiling Game IDs")
    # pb = txtProgressBar(min = 0, max = length(url2), initial = 0)
    #
    # # Have to iterate through every game for the given day and find all play by play ids on the box score page
    # for (i in 1:length(url2)) {
    #   file_dir <- paste0(base_path, "box_score/")
    #   file_path <- paste0(file_dir, game_ids[i], ".html")
    #   isUrlRead <- F
    #
    #   # Assumes that if pbp is available from file it will always be used rather than re-scraping
    #   if (!is.na(base_path) & file.exists(file_path)) {
    #     temp_html <- readLines(file_path, warn=F)
    #   } else {
    #     isUrlRead <- T
    #     file_url <- url(url2[i], headers = c("User-Agent" = "My Custom User Agent"))
    #     temp_html <- readLines(con = file_url, warn=F)
    #     close(file_url)
    #   }
    #
    #   # Give user option to save raw html file (to make future processing more efficient)
    #   if (save_file & !is.na(base_path) & !file.exists(file_path)) {
    #     dir.create(file_dir, recursive = T, showWarnings = F)
    #     writeLines(temp_html, file_path)
    #   }
    #
    #   new_id <- unlist(stringr::str_extract(temp_html, "(?<=play_by_play[/])\\d+"))
    #   new_id <- unique(new_id[!is.na(new_id)])
    #   new_ids <- c(new_ids,new_id)
    #   if (isUrlRead) {
    #     Sys.sleep(0.5)
    #   }
    #   setTxtProgressBar(pb,i)
    # }
    #
    # close(pb)


    message("\nParsing Schedule")
    # Handle opponent and neutral games as both are broken up using an '@' character
    parsed <- lapply(df$Opponent, strsplit, "@")
    parsed <- lapply(parsed, function(x) {
      x <- unlist(x)
      t <- stringr::str_extract(x, "(?<=[\\#[0-9]+] ).*")
      t[is.na(t)] <- x[is.na(t)]
      if(!any(trimws(t) %in% bigballR::teamids$Team)) {
        for(j in 1:length(t)) {
          i <- 1
          while (!substr(t[j], 1, i) %in% bigballR::teamids$Team && i <= nchar(t[j])) {
            i <- i + 1
          }
          t[j] = substr(t[j], 1, i)
        }
      }

      return(t)
    })

    # Use df$Opponent_with_detail to get neutral site information
    parsed_detail <- lapply(df$Opponent_with_detail, strsplit, "@")
    parsed_detail <- lapply(parsed_detail, function(x) {
      x <- unlist(x)
      t <- stringr::str_extract(x, "(?<=[\\#[0-9]+] ).*")
      t[is.na(t)] <- x[is.na(t)]
      if(!any(trimws(t) %in% bigballR::teamids$Team)) {
        for(j in 1:length(t)) {
          i <- 1
          while (!substr(t[j], 1, i) %in% bigballR::teamids$Team && i <= nchar(t[j])) {
            i <- i + 1
          }
          t[j] = substr(t[j], 1, i)
        }
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
      lapply(parsed_detail, function(x) {
        if (length(x) == 2 & x[1] != "")
          return(x[2])

      })

    #Iterate through games and finds the home and away team
    home_team <- rep(NA, length(parsed))
    away_team <- rep(NA, length(parsed))
    is_neutral <- rep(F, length(parsed_detail))
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

    detail <- ifelse(selected_score %in% c("Canceled", "Ppd"), selected_score, detail)
    selected_score <- ifelse(selected_score %in% c("Canceled", "Ppd"), NA, selected_score)

    # pbp_ids <- selected_score
    # pbp_ids[which(!is.na(pbp_ids))] <- if(length(new_ids)>0) new_ids else NA
    # box_ids <- selected_score
    # box_ids[which(!is.na(box_ids))] <- if(length(game_ids)>0) game_ids else NA

    #Put everything together into tidy data frame
    team_data <- data.frame(
      Date = df$Date,
      Home = ifelse(!is.na(home_team), home_team, team_name),
      Home_Score = ifelse(!is.na(home_team), opponent_score, selected_score),
      Away = ifelse(!is.na(away_team), away_team, team_name),
      Away_Score = ifelse(!is.na(away_team), opponent_score, selected_score),
      Contest_ID = game_ids,
      # Box_ID = game_ids,
      # Game_ID = pbp_ids,
      # Box_ID = box_ids,
      isNeutral = is_neutral,
      Detail = detail,
      stringsAsFactors = F
    )
    #Replace blank portions of schedule with dashes, as that is used on NCAA site but NA is better for this purpose
    team_data[team_data == "-"] <- NA

    #Give user final status update and returns the df
    message(paste0(
      team_name[1],
      " complete -- ",
      nrow(team_data),
      "/",
      length(game_ids),
      " games/ids found"
    ))

    return(team_data)
  }

get_team_schedule(season = '2023-24', team.name = 'Purdue')
get_team_schedule(season = '2023-24', team.name = 'Baylor')
