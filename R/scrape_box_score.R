#' Get Box Score Data per game
#'
#' This function will return a dataframe with the box score information as well as
#' additional rows for the team totals and opponent totals for both teams.
#'
#'
#' @param game_id game_id
#'
#' Game ID can be retrived from the schedule function
#'
#' @keywords internal
#' @import rvest
#' @import dplyr
#' @export
#' @examples
#'
#'

scrape_box_score <- function(game_id){
  url = paste0('http://stats.ncaa.org/game/index/',game_id)
  require(rvest)
  require(dplyr)

  tables_test <- try(read_tables(url))
  if(class(tables_test) == "try-error"){
    return(NULL)
  }

  team1 <- tables_test[[1]]
  if(team1[team1$Player == 'Totals','MP'] == 0){
    first_half_url <- paste0(sub('\\?.*', '', url),"?period_no=1")
    second_half_url <- paste0(sub('\\?.*', '', url),"?period_no=2")

    fh_tables <- read_tables(first_half_url)
    sh_tables <- read_tables(second_half_url)

    tables_test[[1]] <- rbind(fh_tables[[1]],sh_tables[[1]])
    tables_test[[2]] <- rbind(fh_tables[[2]],sh_tables[[2]])
  }



  ind_df_one <- which(tables_test[[2]]["Player"] == "Totals")
  ind_df_two <- which(tables_test[[1]]["Player"] == "Totals")
  new_row_one <- tables_test[[2]][ind_df_one,]
  new_row_one[,1] <- "opp_totals"
  new_row_two <- tables_test[[1]][ind_df_two,]
  new_row_two[,1] <- "opp_totals"
  tables_test[[1]] <- rbind(tables_test[[1]],new_row_one)
  tables_test[[2]] <- rbind(tables_test[[2]],new_row_two)

  ## First table
  team_one_name <- read_html(url) %>% html_nodes(".header_menu+ .mytable .heading td") %>% html_text() %>% trimws()
  tables_test[[1]]$team <- rep(team_one_name,nrow(tables_test[[1]]))
  ## Second table
  team_two_name <- read_html(url) %>% html_nodes("br+ .mytable .heading td") %>% html_text() %>% trimws()
  tables_test[[2]]$team <- rep(team_two_name,nrow(tables_test[[2]]))
  box_score_df <-  bind_rows(tables_test)

  box_score_df$Fouls <- as.numeric(box_score_df$Fouls)
  box_score_df[,3:17] <-  box_score_df[,3:17] %>%
    mutate_all(list(~as.numeric(gsub('\\D+',"",.))))

  numeric_cols <- sapply(box_score_df, is.numeric) %>% which %>%
    names %>% setdiff(., c("id_variable", "dep_var"))

  box_score_df <- box_score_df %>%  group_by(team,Player) %>%
    summarise_at(numeric_cols,sum) %>% ungroup()
  box_score_df$game_id <- game_id
  return(box_score_df)
}







read_tables <- function(url){
  # helper function
  tables <- read_html(url) %>% html_nodes(".mytable") %>% html_table()
  box_score <- tables[[1]]
  tables[[1]] <- NULL
  tables_test <- lapply(tables,function(i) {
    colnames(i) <- i[2,]

    colnames(i)[which(colnames(i)=="Tot Reb")] <-  "totreb"
    i <- i[-(1:2),]
    ## Convert to numeric
    ind <- which(colnames(i) == "MP")
    i[,ind] <- sapply(strsplit(i[,ind],":"),
                      function(x) {
                        x <- as.numeric(x)
                        x[1]+x[2]/60
                      }
    )

    i[,ind:ncol(i)] <- sapply(i[,(ind:ncol(i))],as.character)
    i[,ind:ncol(i)] <- sapply(i[,(ind:ncol(i))],function(x){
      gsub("([0-9]+).*$", "\\1", x)
    })
    i[,ind:ncol(i)] <- sapply(i[,(ind:ncol(i))],as.numeric)

    i[is.na(i)] <- 0
    rownames(i) <- NULL
    i
  })
  return(tables_test)
}
