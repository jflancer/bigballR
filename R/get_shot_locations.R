## Shot Location Function (get_shot_locations)
get_shot_locations <- function(gameids) {

  process_game <- function(i) {
    url <- paste0("https://stats.ncaa.org/contests/", i, "/box_score")
    html <- rvest::read_html(url)
    Sys.sleep(2)
    shot_data <- html %>% rvest::html_nodes('.p-4  script')
    raw_data <- shot_data[2] %>% rvest::html_text(trim = TRUE)
    shots <- unlist(strsplit(raw_data, "\n", fixed = TRUE))
    matches <- gregexpr("(?<=addShot\\().+?(?=\\);)", shots, perl = TRUE)

    shot_text <- unlist(regmatches(shots, matches))
    shot_text <- gsub(", Jr.", " Jr.", shot_text)
    shot_text <- gsub(", JR.", " JR.", shot_text)
    shot_text <- gsub(", II", " II", shot_text)
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
        Game_Seconds = dplyr::case_when(
          Half_Status == 1 ~ 20 * 60 - (minutes * 60 + seconds),
          Half_Status == 2 ~ 40 * 60 - (minutes * 60 + seconds),
          Half_Status == 3 ~ 45 * 60 - (minutes * 60 + seconds),
          Half_Status == 4 ~ 50 * 60 - (minutes * 60 + seconds),
          Half_Status == 5 ~ 55 * 60 - (minutes * 60 + seconds),
          Half_Status == 6 ~ 60 * 60 - (minutes * 60 + seconds),
          Half_Status == 7 ~ 65 * 60 - (minutes * 60 + seconds),
          TRUE ~ 70 * 60 - (minutes * 60 + seconds)
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

## PBP + Shot Location Helper function (join_pbp_shots)
join_pbp_shots = function(pbp_data, shot_data) {
  pbp_ids = sort(unique(pbp_data$ID))
  shot_ids = sort(unique(shot_data$ID))

  if (!identical(pbp_ids, shot_ids)) {
    stop("PBP and Shot Locations do not match.")
  }

  pbp_data = pbp_data %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(row = row_number()) %>%
    dplyr::ungroup()

  shot_att = pbp_data %>%
    dplyr::filter(Shot_Value %in% c(2, 3))

  no_shot_att = pbp_data %>%
    dplyr::filter(is.na(Shot_Value) | Shot_Value == 1)

  shot_att = shot_att %>%
    dplyr::group_by(ID, Game_Seconds, Event_Result) %>%
    dplyr::mutate(shot_no = row_number()) %>%
    dplyr::ungroup()

  shot_data = shot_data %>%
    dplyr::group_by(ID, Game_Seconds, Shot_Result) %>%
    dplyr::mutate(shot_no = row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(ID, Game_Seconds, Shot_Result, shot_no, Team, Player, x, y)

  shot_att_joined = shot_att %>%
    dplyr::left_join(
      shot_data,
      by = join_by(ID, Game_Seconds, Event_Result == Shot_Result, shot_no))

  combined = dplyr::bind_rows(shot_att_joined, no_shot_att) %>%
    dplyr::group_by(ID) %>%
    dplyr::arrange(row, .by_group = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-row, -shot_no)

  return(combined)
}
