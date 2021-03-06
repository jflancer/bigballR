% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_functions.R
\name{get_lineups}
\alias{get_lineups}
\title{Lineup Compiler}
\usage{
get_lineups(play_by_play_data = NA, include_transition = F)
}
\arguments{
\item{play_by_play_data}{data frame consisting of play-by-play data from the functions scrape_game() or get_play_by_play()}
}
\value{
data frame with each row representing a unique lineup. All stats for entire lineup or opponent (o- prefix)
\itemize{
\item{P1} - Player in lineup
\item{P2} - Player in lineup
\item{P3} - Player in lineup
\item{P4} - Player in lineup
\item{P5} - Player in lineup
\item{Team} - Team for the lineup
\item{Mins} - Minutes the lineup was on the court
\item{PTS} - Points scored
\item{FGA} - Field goal attempts
\item{TO} - Turnovers
\item{TPA} - Three point attempts
\item{FGM} - Field goals made
\item{TPM} - Three points made
\item{FTA} - Free throw attempts
\item{FTM} - Free throws made
\item{ORB} - Offensive rebounds
\item{DRB} - Defensive rebounds
\item{RIMA} - Rim attempts: defined as layups, dunks, tip-ins, hook attempts
\item{BLK} - Blocked shots
\item{AST} - Assists
\item{POSS} - (Offensive) Possessions: (FGA + .475 x FTA - ORB + TO + oFGA + .475 x oFTA - oORB + oTO) /2
\item{TS.} - True shooting percentage: (PTS / 2) / (FGA + .475 x FTA),
\item{eFG.} - Effective field goal percentage: (FGM + 0.5 x TPM) / FGA,
\item{TPP} - Three point percentage: TPA/TPM
\item{FTP} - Free throw percentage: FTA/FTM
\item{TPrate} - Three point attempt rate: TPA/FGA
\item{ASTrate} - Assist rate: AST/FGM
\item{TOrate} - Turnover rate: TO/POSS
\item{FTrate} - Free throw rate: FTA/FGA
\item{BLKrate} - Block rate: BLK/FGA
\item{ORB.} - Offensive rebound percentage: ORB / (ORB + oDRB)
\item{DRB.} - Defensive rebound percentage: DRB / (DRB + oORB)
\item{ORTG} - Offensive efficiency: 100 * (PTS/POSS)
\item{DRTG} - Defensive efficiency: 100 * (oPTS/POSS)
\item{NETRTG} - Net efficiency: OEFF - DEFF
\item{TimePerPoss} - Average time per possession (Seconds): (Possessions / Mins) * 60
}
}
\description{
This function takes in a play-by-play dataframe, and generates all possible lineups for both teams.
It then calculates a variety of statistics/metrics at a lineup level.
}
