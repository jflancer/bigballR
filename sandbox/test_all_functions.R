## Test all functions ################

x <- get_date_games("04/02/2024")

team_sched <- get_team_schedule(season = '2023-24', team.name = 'Purdue')
team_sched
# team_sched <- get_team_schedule(season = '2023-24', team.name = 'Colorado')
# team_sched

df <- get_play_by_play(team_sched$Game_ID[1])
df <- get_play_by_play(3953847)

scrape_box(team_sched$Game_ID[1])
get_box_scores(team_sched$Game_ID[1])
get_box_scores(3953847)

get_lineups(df)
get_lineups(df, include_transition = T)
get_team_stats(df) |> print()
get_team_stats(df, include_transition = T) |> print()

scrape_game(team_sched$Contest_ID[nrow(team_sched)])

get_player_stats(play_by_play_data = df)

get_date_games('11/10/2023')

get_team_roster(season = '2023-24', team.name = 'Purdue')









