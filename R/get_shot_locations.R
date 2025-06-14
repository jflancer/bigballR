get_shot_locations <- function(gameids) {
  
  process_game <- function(i) {
    url <- paste0("https://stats.ncaa.org/contests/", i, "/box_score")
    html <- rvest::read_html(url)
    shot_data <- html %>% rvest::html_nodes('.p-4  script')
    raw_data <- shot_data[2] %>% rvest::html_text(trim = TRUE)
    shots <- unlist(strsplit(raw_data, "\n", fixed = TRUE))
    matches <- gregexpr("(?<=addShot\\().+?(?=\\);)", shots, perl = TRUE)
    
    shot_text <- unlist(regmatches(shots, matches))
    shot_text <- gsub(", Jr.", " Jr.", shot_text)
    shot_text <- gsub(", III", " III", shot_text)
    shot_text <- gsub(", IV", " IV", shot_text)

    shots_df <- do.call(rbind.data.frame, strsplit(shot_text, ", ", fixed = TRUE))
    
    colnames(shots_df) <- c("raw_x", "raw_y", "shooting_org", "made", "play_id", "play_description", "classes", "show_highlight")
    
    shots_df$raw_x <- as.numeric(shots_df$raw_x)
    shots_df$raw_y <- as.numeric(shots_df$raw_y)
    
    shots_df <- shots_df %>%
      dplyr::mutate(
        x = (raw_x / 100 * 94),
        y = 50 - (raw_y / 100 * 50),
        Half_Status = as.numeric(stringr::str_extract(play_description, "\\d")),
        Player = stringr::str_extract(play_description, "(?<= (missed|made) by ).+?(?=\\()"),
        Team = stringr::str_replace(
          stringr::str_extract(play_description, "(?<=\\().+?(?=\\))"),
          "&amp;", "&"
        ),        
        Score = stringr::str_extract(play_description, "\\d+-\\d+$"),
        Time = stringr::str_extract(play_description, "\\d{2}:\\d{2}"),
        minutes = as.numeric(stringr::str_extract(Time, "^\\d{2}")),
        seconds = as.numeric(stringr::str_extract(Time, "(?<=:)\\d{2}")),
        Shot_Result = dplyr::if_else(made == "true", "made", "missed"),
        Game_Seconds = dplyr::if_else(
          Half_Status == 1, (20 * 60 - (minutes * 60 + seconds)),
          dplyr::if_else(
            Half_Status == 2, (40 * 60 - (minutes * 60 + seconds)),
            dplyr::if_else(
              Half_Status == 3, (45 * 60 - (minutes * 60 + seconds)),
              (50 * 60 - (minutes * 60 + seconds))
            )
          )
        ),
        ID = i
      )
    
    shots <- shots_df[, c(20, 11, 15, 19, 13, 12, 18, 9,10)]
    return(shots)
  }
  
  shot_list <- lapply(seq_along(gameids), function(idx) {
    i <- gameids[idx]
    result <- process_game(i)
    teams <- sort(unique(result$Team))
    team1 <- teams[1]
    team2 <- teams[2]
    
    message(sprintf("Game_ID: %s || %s v. %s || %d shots found", 
                    i, team1, team2, nrow(result)))

    Sys.sleep(2)
    return(result)
  })
  
  shot_locations <- dplyr::bind_rows(shot_list)
  
  shot_locations <- shot_locations %>%
    dplyr::group_by(ID, Team, Half_Status) %>%
    dplyr::mutate(Side = dplyr::if_else(sum(x > 50) > 2, "right", "left")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      bx = dplyr::if_else(Side == "right", 90, 4),
      by = 25,
      Shot_Dist = sqrt((x - bx)^2 + (y - by)^2)
    ) %>% 
    dplyr::select(-c(bx,by,Side))
  
  return(shot_locations)
}
