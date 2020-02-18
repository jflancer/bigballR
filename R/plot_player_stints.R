#' Stint Plot
#'
#' Plots all player and lineup stints for a single game, along with player ending +/-
#' Plot is colored by net points added/lost during stint
#' @param play_by_play_data a data frame of lineups created from get_lineups(). Must be a single game
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export
#' @return ggplot object of stint plot
plot_player_stints <- function(play_by_play_data = NA) {
  if(length(unique(play_by_play_data$ID)) > 1) {
    stop("You must only pass in one game of play by play")
  }

  game_data <- play_by_play_data %>%
    dplyr::filter(Game_Seconds <= 2400) %>%
    dplyr::select(Game_Seconds, Home_Score, Away_Score, Home.1:Away.5) %>%
    tidyr::complete(Game_Seconds = 0:2400) %>%
    dplyr::mutate(First = Game_Seconds <= 1200) %>%
    dplyr::group_by(First) %>%
    tidyr::fill(-First, .direction = "up") %>%
    dplyr::ungroup() %>%
    dplyr::select(-First) %>%
    dplyr::distinct() %>%
    dplyr::select(Game_Seconds, Home_Score, Away_Score, dplyr::everything())

  players <- unique(unlist(game_data[,4:13]))
  players <- players[!is.na(players)]

  all_stints <- lapply(players, get_player_stints, game_data = game_data) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      Player = paste0(
        substr(Player,1,1),
        tolower(substr(Player,2,  sapply(Player, function(x){gregexpr("\\.", x)[[1]]})-1)),
        " ",
        substr(Player,sapply(Player, function(x){gregexpr("\\.", x)[[1]]})+1,sapply(Player, function(x){gregexpr("\\.", x)[[1]]})+1),
        tolower(substr(Player,sapply(Player, function(x){gregexpr("\\.", x)[[1]]})+2, nchar(Player)))
      ),
      Player = reorder(Player, -Start, FUN = max)
    )

  home_stints <- game_data %>%
    dplyr::select(Game_Seconds:Home.5) %>%
    dplyr::mutate(isChange = (Home.1!=dplyr::lead(Home.1)) |
             (Home.2!=dplyr::lead(Home.2)) |
             (Home.3!=dplyr::lead(Home.3)) |
             (Home.4!=dplyr::lead(Home.4)) |
             (Home.5!=dplyr::lead(Home.5)),
           Stint = cumsum(isChange)
    ) %>%
    dplyr::group_by(Stint) %>%
    dplyr::summarise(Start = min(Game_Seconds),
              Stop = max(Game_Seconds),
              Score_Dif = max(Home_Score) - min(Home_Score) - max(Away_Score) + min(Away_Score)
    ) %>%
    dplyr::mutate(Team = T) %>%
    dplyr::filter(Start != Stop)

  away_stints <- game_data %>%
    dplyr::select(Game_Seconds:Away_Score, Away.1:Away.5) %>%
    dplyr::mutate(isChange = (Away.1!=lead(Away.1)) |
             (Away.2!=dplyr::lead(Away.2)) |
             (Away.3!=dplyr::lead(Away.3)) |
             (Away.4!=dplyr::lead(Away.4)) |
             (Away.5!=dplyr::lead(Away.5)),
           Stint = cumsum(isChange)
    ) %>%
    dplyr::group_by(Stint) %>%
    dplyr::summarise(Start = min(Game_Seconds),
              Stop = max(Game_Seconds),
              Score_Dif = -(max(Home_Score) - min(Home_Score) - max(Away_Score) + min(Away_Score))
    ) %>%
    dplyr::mutate(Team = F) %>%
    dplyr::filter(Start != Stop)

  team_stints <- home_stints %>%
    dplyr::bind_rows(away_stints) %>%
    dplyr::mutate(XStart = ifelse(Team,
                           min(ordered(all_stints$Player[which(all_stints$Team)])),
                           min(ordered(all_stints$Player[which(!all_stints$Team)]))
    ),
    XEnd = ifelse(Team,
                  max(ordered(all_stints$Player[which(all_stints$Team)])),
                  max(ordered(all_stints$Player[which(!all_stints$Team)]))
    ))

  player_pm <- all_stints %>%
    dplyr::group_by(Player, Team) %>%
    dplyr::summarise(PlusMinus = sum(Score_Dif)
    ) %>%
    dplyr::mutate(PlusMinusLabel = ifelse(PlusMinus>0, paste0("+",PlusMinus), paste0(PlusMinus)))

  ggplot2::ggplot(all_stints) +
    ggplot2::geom_linerange(ggplot2::aes(x=Player, ymin = Start, ymax = Stop)) +
    ggplot2::geom_rect(data = team_stints,
                       ggplot2::aes(xmin = 0,
                  xmax = Inf,
                  ymin = Start, ymax = Stop, fill = Score_Dif),
              alpha = 0.4
    ) +
    ggplot2::scale_fill_gradient2(limits = c(-max(abs(all_stints$Score_Dif)),
                                    max(abs(all_stints$Score_Dif))),
                         guide = 'none',
                         low = "firebrick3",
                         high = "slateblue3"
    ) +
    ggplot2::geom_linerange(ggplot2::aes(x=Player, ymin = Start, ymax = Stop, color = Score_Dif),
                   size = 2.4) +
    ggplot2::geom_rect(aes(xmin = 0, xmax = Inf, ymin = -200, ymax = 0), fill = "gray28") +
    ggplot2::geom_text(data = player_pm, ggplot2::aes(x = Player, y = -100,
                                    label = PlusMinusLabel, color = PlusMinus),

              size = 5.5) +
    ggplot2::scale_color_gradient2(limits = c(-max(abs(player_pm$PlusMinus))-2,
                                     max(abs(player_pm$PlusMinus))+2),
                          low = "firebrick3",
                          high = "slateblue3",
                          name = "Net Points"
    ) +
    ggplot2::facet_wrap(.~Team, nrow = 2, scales = "free_y", labeller = ggplot2::labeller(Team =
                                                                          c("TRUE" = first(play_by_play_data$Home),
                                                                            "FALSE" = first(play_by_play_data$Away)))
                        ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(breaks = c(0,1200,2400), labels = c("","H2","")) +
    ggplot2::geom_hline(yintercept = c(0,1200,2400), color = "gray30", size = 1.5) +
    ggplot2::ggtitle(paste("Team Performance by Player Stint:",
                  first(play_by_play_data$Away),
                  "at",
                  first(play_by_play_data$Home),
                  "-",
                  first(play_by_play_data$Date))
    ) +
    ggplot2::labs(caption = "Jake Flancer (@JakeFlancer) | Data: NCAA.com") +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 14),
          axis.ticks = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(fill = "gray75"),
          panel.background = ggplot2::element_rect(fill = "gray75"),
          legend.position = c(0,-0.2),
          legend.background = ggplot2::element_rect(fill = "gray75"),
          legend.text = ggplot2::element_text(size = 6),
          legend.title = ggplot2::element_text(size = 8, vjust = 0.75),
          legend.justification = c(0,-0.2),
          legend.direction = "horizontal",
          panel.grid = ggplot2::element_blank(),
          plot.margin= ggplot2::unit(c(1,0.5,3,0.5), "cm"),
          legend.spacing.x = ggplot2::unit(0.2, "cm"),
          strip.background = ggplot2::element_blank(),
          strip.text = ggplot2::element_text(hjust = 0.95, face = "bold", size = 14),
          plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 18),
          text = ggplot2::element_text(family = "Helvetica", color = "gray25", face = "bold"))
}

#' Helper function to get player stints
#' Used in plot_player_stints
#' @import dplyr
get_player_stints <- function(game_data, player_name) {
  home <- player_name %in% unlist(game_data[,4:8])

  cols = if(home) 4:8 else 9:13

  is_on <- apply(game_data[,cols], 1, function(x){player_name %in% x})

  subs <- (is_on) & (lag(is_on) == F) | (is_on == T) & (lead(is_on) == F)
  subs[c(1,length(subs))] <- is_on[c(1,length(subs))]

  stint_data <- game_data %>%
    dplyr::select(Game_Seconds, Home_Score, Away_Score) %>%
    dplyr::mutate(Sub = cumsum(subs)) %>%
    dplyr::group_by(Sub) %>%
    dplyr::summarise(Start = min(Game_Seconds),
              Stop = max(Game_Seconds),
              Score_Dif = max(Home_Score) - min(Home_Score) - max(Away_Score) + min(Away_Score)
    ) %>%
    dplyr::mutate(Score_Dif = if(home) Score_Dif else -Score_Dif,
           Player = player_name,
           Team = home
    ) %>%
    dplyr::filter(Start != Stop, Sub %%2 == 1)

  return(stint_data)
}


