
#' Functiont o get player combos
#'
#' Pass in lineup data and get team stats when a specified grouping of players are on the court together.
#' This can work for any 1-5 player combinations. It also takes in the functions Included/Excluded, to filter
#' when certain players are on the court together.
#' @param Lineup_Data data frame made up of lineups collected from the get_lineups() function
#' @param n an integer between 1-5 specifying player combinations
#' @param min_mins a filter value to remove combinations below a minutes threshold. Using this beforehand greatly speeds up computation time.
#' @param Included character vector of players. These players will be on the court for every lineup considered.
#' @param Excluded character vector of players. These players will be off the court for every lineup considered.
#' @import dplyr
#' @importFrom gtools combinations
#' @export
#' @return data frame with each row representing a player combination. Explanations of statistics found in get_lineups()
#' @examples Duke_Lineups = get_lineups(get_play_by_play(get_team_schedule(season="2018-19", team.name = "Duke)))
#' Get all two-man player combinations when Tre Jones is on the court
#' get_player_combos(Lineup_Data = Duke_Lineups, n= 2, min_mins = 5, Included = "TRE.JONES")
get_player_combos <- function(Lineup_Data = NA, n = 2, min_mins = 0, Included = NA, Excluded = NA) {
  # Since lineups can have multiple teams, but player combinations are team specific, separate out into team groups and then apply
  combos <- get_player_lineups(Lineup_Data, Included, Excluded) %>%
    dplyr::group_by(Team) %>%
    dplyr::do(team_comb(., mins = min_mins, n = n))

  return(combos)
}

# Helper function for get_player_combos()
# Gets all n-man combinations within an individuatl team
team_comb <- function(team_lineups, mins = 0, n = 2) {

  # First get all players in the lineups
  players <- unique(unlist(team_lineups[,1:5], use.names = F))
  # Generate all combinations
  combs <- gtools::combinations(length(players), n, players)

  # Filter only combinations that have played above min_mins
  filt <- apply(combs, 1, function(x) {
    # Get total minutes for when each combo is on the court together
    sum(unlist(apply(team_lineups[,1:7], 1, function(y){
      if(sum(x %in% y[1:5]) == n) as.numeric(unlist(y[7]))
    })))
  }) > mins

  combs <- matrix(combs[filt,], ncol = n)
  # For each combination, calculate the team stats when they're on the court
  # Borrows code from on_off_generator, but this call is much reduced so to speed up does not directly call on_off_generator
  a <- apply(combs, 1, function(x){
    final <- get_player_lineups(team_lineups, Included = x) %>%
      dplyr::summarise_if(is.numeric, sum) %>%
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
      dplyr::select(Mins:oPTS, POSS:NETRTG, everything())
    final[is.na(final)] <- 0
    final[1:nrow(final),] <- apply(final[1:nrow(final),], 2, function(z){ifelse(is.infinite(z),0,z)})
    if(length(x) == 1) {
      z <- cbind(data.frame(X1 = matrix(rep(x,nrow(final)), nrow = nrow(final)), stringsAsFactors = F), final)
    } else {
      z <- cbind(data.frame(matrix(rep(x,nrow(final)), nrow = nrow(final)), stringsAsFactors = F), final)
    }
  }) %>% dplyr::bind_rows() %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("X")), list(~gsub("X","P",.)))
}
