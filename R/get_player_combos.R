
#' Functiont o get player combos
#'
#' Pass in lineup data and get team stats when a specified grouping of players are on the court together.
#' This can work for any 1-5 player combinations. It also takes in the functions Included/Excluded, to filter
#' when certain players are on the court together. Recommended to filter lineups by team before calling this function rather than after.
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
get_player_combos <- function(Lineup_Data = NA, n = 2, min_mins = 0, Included = NA, Excluded = NA, include_transition=F) {
  if(!n %in% 1:5) {
    stop("n must be an integer from 1 and 5")
  }

  if(!include_transition) {
    Lineup_Data <- dplyr::select(Lineup_Data, -matches("_trans|_half"))
  }

  if(!any(colnames(Lineup_Data) %in% c("_trans","_half"))) {
    include_transition=F
  }

  # Since lineups can have multiple teams, but player combinations are team specific, separate out into team groups and then apply
  combos <- get_player_lineups(Lineup_Data, Included, Excluded) %>%
    dplyr::group_by(Team) %>%
    dplyr::do(team_comb(., mins = min_mins, n = n, include_transition=include_transition))

  return(combos)
}

# Helper function for get_player_combos()
# Gets all n-man combinations within an individuatl team
team_comb <- function(team_lineups, mins = 0, n = 2, include_transition=F) {

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
      dplyr::summarise(across(where(is.numeric), sum)) %>%
      dplyr::mutate(
        ORTG = PTS / oPOSS * 100,
        DRTG = dPTS / dPOSS * 100,
        NETRTG = ORTG - DRTG,
        FG. = FGM / FGA,
        dFG. = dFGM / dFGA,
        TPP = TPM / TPA,
        dTPP = dTPM / dTPA,
        FTP = FTM / FTA,
        dFTP = dFTM / dFTA,
        eFG. = (FGM + 0.5 * TPM) / FGA,
        deFG. = (dFGM + 0.5 * dTPM) / dFGA,
        TS. = (PTS / 2) / (FGA + .475 * FTA),
        dTS. = (dPTS / 2) / (dFGA + .475 * dFTA),
        RIM. = RIMM / RIMA,
        dRIM. = dRIMM / dRIMA,
        MID. = (FGM - RIMM - TPM) / (FGA - RIMA - TPA),
        dMID. = (dFGM - dRIMM - dTPM) / (dFGA - dRIMA - dTPA),
        TPrate = TPA / FGA,
        dTPrate = dTPA / dFGA,
        RIMrate = RIMA / FGA,
        dRIMrate = dRIMA / dFGA,
        MIDrate = (FGA - TPA - RIMA) / FGA,
        dMIDrate = (dFGA - dTPA - dRIMA) / dFGA,
        FTrate = FTA / FGA,
        dFTrate = dFTA / dFGA,
        ASTrate = AST / FGM,
        dASTrate = dAST / dFGM,
        TOrate = TO / oPOSS,
        dTOrate = dTO / dPOSS,
        BLKrate = BLK / dFGA,
        oBLKrate = dBLK / FGA,
        ORB. = ORB / (ORB + dDRB),
        DRB. = DRB / (DRB + dORB),
        TimePerPoss = (oMins / oPOSS) * 60,
        dTimePerPoss = (dMins / dPOSS) * 60
      ) %>%
      summarise(across(where(is.numeric), function(x){ifelse(is.infinite(x) | is.na(x), 0, round(x, 2))}))

      if(include_transition) {
          final <- final %>%
            dplyr::mutate(
              ORTG_trans = PTS_trans / oPOSS_trans * 100,
              DRTG_trans = dPTS_trans / dPOSS_trans * 100,
              NETRTG_trans = ORTG_trans - DRTG_trans,
              FG._trans = FGM_trans / FGA_trans,
              dFG._trans = dFGM_trans / dFGA_trans,
              TPP_trans = TPM_trans / TPA_trans,
              dTPP_trans = dTPM_trans / dTPA_trans,
              FTP_trans = FTM_trans / FTA_trans,
              dFTP_trans = dFTM_trans / dFTA_trans,
              eFG._trans = (FGM_trans + 0.5 * TPM_trans) / FGA_trans,
              deFG._trans = (dFGM_trans + 0.5 * dTPM_trans) / dFGA_trans,
              TS._trans = (PTS_trans / 2) / (FGA_trans + .475 * FTA_trans),
              dTS._trans = (dPTS_trans / 2) / (dFGA_trans + .475 * dFTA_trans),
              RIM._trans = RIMM_trans / RIMA_trans,
              dRIM._trans = dRIMM_trans / dRIMA_trans,
              MID._trans = (FGM_trans - RIMM_trans - TPM_trans) / (FGA_trans - RIMA_trans - TPA_trans),
              dMID._trans = (dFGM_trans - dRIMM_trans - dTPM_trans) / (dFGA_trans - dRIMA_trans - dTPA_trans),
              TPrate_trans = TPA_trans / FGA_trans,
              dTPrate_trans = dTPA_trans / dFGA_trans,
              RIMrate_trans = RIMA_trans / FGA_trans,
              dRIMrate_trans = dRIMA_trans / dFGA_trans,
              MIDrate_trans = (FGA_trans - TPA_trans - RIMA_trans) / FGA_trans,
              dMIDrate_trans = (dFGA_trans - dTPA_trans - dRIMA_trans) / dFGA_trans,
              FTrate_trans = FTA_trans / FGA_trans,
              dFTrate_trans = dFTA_trans / dFGA_trans,
              ASTrate_trans = AST_trans / FGM_trans,
              dASTrate_trans = dAST_trans / dFGM_trans,
              TOrate_trans = TO_trans / oPOSS_trans,
              dTOrate_trans = dTO_trans / dPOSS_trans,
              BLKrate_trans = BLK_trans / dFGA_trans,
              oBLKrate_trans = dBLK_trans / FGA_trans,
              ORB._trans = ORB_trans / (ORB_trans + dDRB_trans),
              DRB._trans = DRB_trans / (DRB_trans + dORB_trans),
              TimePerPoss_trans = (oMins_trans / oPOSS_trans) * 60,
              dTimePerPoss_trans = (dMins_trans / dPOSS_trans) * 60,
              ORTG_half = PTS_half / oPOSS_half * 100,
              DRTG_half = dPTS_half / dPOSS_half * 100,
              NETRTG_half = ORTG_half - DRTG_half,
              FG._half = FGM_half / FGA_half,
              dFG._half = dFGM_half / dFGA_half,
              TPP_half = TPM_half / TPA_half,
              dTPP_half = dTPM_half / dTPA_half,
              FTP_half = FTM_half / FTA_half,
              dFTP_half = dFTM_half / dFTA_half,
              eFG._half = (FGM_half + 0.5 * TPM_half) / FGA_half,
              deFG._half = (dFGM_half + 0.5 * dTPM_half) / dFGA_half,
              TS._half = (PTS_half / 2) / (FGA_half + .475 * FTA_half),
              dTS._half = (dPTS_half / 2) / (dFGA_half + .475 * dFTA_half),
              RIM._half = RIMM_half / RIMA_half,
              dRIM._half = dRIMM_half / dRIMA_half,
              MID._half = (FGM_half - RIMM_half - TPM_half) / (FGA_half - RIMA_half - TPA_half),
              dMID._half = (dFGM_half - dRIMM_half - dTPM_half) / (dFGA_half - dRIMA_half - dTPA_half),
              TPrate_half = TPA_half / FGA_half,
              dTPrate_half = dTPA_half / dFGA_half,
              RIMrate_half = RIMA_half / FGA_half,
              dRIMrate_half = dRIMA_half / dFGA_half,
              MIDrate_half = (FGA_half - TPA_half - RIMA_half) / FGA_half,
              dMIDrate_half = (dFGA_half - dTPA_half - dRIMA_half) / dFGA_half,
              FTrate_half = FTA_half / FGA_half,
              dFTrate_half = dFTA_half / dFGA_half,
              ASTrate_half = AST_half / FGM_half,
              dASTrate_half = dAST_half / dFGM_half,
              TOrate_half = TO_half / oPOSS_half,
              dTOrate_half = dTO_half / dPOSS_half,
              BLKrate_half = BLK_half / dFGA_half,
              oBLKrate_half = dBLK_half / FGA_half,
              ORB._half = ORB_half / (ORB_half + dDRB_half),
              DRB._half = DRB_half / (DRB_half + dORB_half),
              TimePerPoss_half = (oMins_half / oPOSS_half) * 60,
              dTimePerPoss_half = (dMins_half / dPOSS_half) * 60,
              oTransPCT = oPOSS_trans / oPOSS,
              dTransPCT = dPOSS_trans / dPOSS
            ) %>%
            #no need to have long decimals so round everything
            dplyr::mutate(across(where(is.numeric), ~ round(., 3)))
      }

    if(length(x) == 1) {
      z <- cbind(data.frame(X1 = matrix(rep(x,nrow(final)), nrow = nrow(final)), stringsAsFactors = F), final)
    } else {
      z <- cbind(data.frame(matrix(rep(x,nrow(final)), nrow = nrow(final)), stringsAsFactors = F), final)
    }
  }) %>% dplyr::bind_rows() %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("X")), list(~gsub("X","P",.)))

  return(a)
}
