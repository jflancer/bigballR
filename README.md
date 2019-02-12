# bigballR

`bigballR` is an R package for working with NCAA Basketball data. This 
package primarily revolves around the use of schedule, roster, and play-by-play data via stats.ncaa.com, and
additionally has features to calculate lineups, on/off results, and player game and multi-game statistics.

## Installation

First install the package `devtools` if you haven't already
``` r
#install.packages("devtools")
devtools::install_github("jflancer/bigballR")
```

## Functionality

**Retrieving Game IDs and Other Information**

Manually, game ids can be found in the url when browsing games, for example:
```4674164``` is the game id for ```https://stats.ncaa.org/game/play_by_play/4674164```

- `get_date_games` data frame with game info and game ids for a given date.
- `get_team_schedule` data frame with game info and ids for a given team.
- `get_team_roster` data frame with player roster information for a given team. 

**Game Scraping Functions**
- `scrape_game` the base function, given a game id will return a cleaned play by play file.
- `get_play_by_play` builds off of scrape_game, allows for compiling from multiple game ids formatted in a vector.

**Data Manipulation Functions**
- `get_lineups` given play-by-play data retrieved from game scraping functions, will calculate a variety of stats for all lineups.
- `on_off_generator` calculate on/off statistics for specified players given lineup data from get_lineups().
- `get_player_stats` calculate a variety of player stats from play-by-play data at either a game or multi-game level.
- `get_player_lineups` acts as an easy way to filter lineup data, specifying players to exclude/include from the lineups.

**Datasets**
- `teamids` dataset includes stats.ncaa team name, team conference, season, and team id for 17-18 and 18-19 seasons

### Use

There are many different progressions and ways to use this package. As an example, here are some natural steps you could take.
``` r
# Get team schedule
# Note: if you don't know the proper team.name (case sensitive), you can look it up in data("teamids")
schedule <- get_team_schedule(season = "2018-19", team.name = "Duke")
# Get play by play for all games played so far in season
play_by_play <- get_play_by_play(schedule$Game_ID)
# Generate all lineups and stats from the play by play
lineups <- get_lineups(play_by_play_data = play_by_play, keep.dirty = T, garbage.filter = F)
# Look at Zion Williamson's on/off statistics with lineups that include Reddish and Barrett
zion_comparison <- on_off_generator("ZION.WILLIAMSON", lineups, Included = c("CAM.REDDISH","RJ.BARRETT"))
```

### `scrape_game` / `get_play_by_play`
Functions to retrieve play by play data. scrape_game() works for individual games while get_play_by_play can handle a vector of gameids and will aggregate into a single dataframe. Warns users of potential errors and mistakes made by the game trackers. The number of player discrepancies warning counts displays the number of events players committed when it is found they were not on the court at the time of the event. The substitution mistake warning indicates an unclean substitution was entered. (ex. 2 players enter and 1 leaves)
* game_id | string made up of digits given to each unique game. This can be found in the play-by-play url for each game.
* game_ids | same function as above, but can handle character vectors of any length.

### `get_date_games`
This function returns a schedule for the given date and specified conference. Results are included if applicable, as well as the play-by-play game id
* date | a character object containing a date in the format mm/dd/yyyy. Defaults is previous day (yesterday)
* conference | the common name used for a conference, not sensitive to case, spacing, punctuation, etc.
* conference.ID | alternatively, if the conference ID is known it replace the conference name variable.

### `get_team_schedule`
This function returns a data frame of the schedule for the specified team. This will include game ids used for play-by-play scraping if the game has ended, along with the team scores and attendance. Note: currently, the season/team.name parameters can only be used for the 2016-17, 2017-18, 2018-19 seasons.
* team.id | The unique id given to each college/team for each season. This can be found in the url of the team page.
* season | Season following format yyy1-y2, ex "2018=19"
* team.name | Alternative to using the id, you can use a team name. This follows the format found in data(ids).

### `get_team_roster`
This function returns a data frame of the roster for the specified team. This will include player names and positions as well as jersey number, height and school year. Note: currently, the season/team.name parameters can only be used for the 2016-17, 2017-18, 2018-19 seasons.
* This takes the same parameters and conditions as get_team_schedule() above

### `get_lineups`
This function takes in a play-by-play dataframe, and generates all possible lineups for both teams. It then calculates a variety of statistics/metrics at a lineup level.
* play_by_play_data | play-by-play data from the functions scrape_game() or get_play_by_play()
* keep.dirty | logical to specify whether or not to filter out potentially inaccurrate data. When FALSE, will remove all rows from games where the number of discrepencies is above the desired count.
* garbage.filter | logical variable to specify whether or not to filter out garbage time entries.
* error.thresh | lets user set their preferred discrepancy threshold with the keep.dirty variable. This means when less than the threshold occurs in a game, it will be considered clean. As defined in scrape_game(), a discrepancy occurs when a player registers an event when they are not found to be on the court.

### `on_off_generator`
This function passes in lineup data and calculates the on/off lineup statistics for all lineup combinations of players specified. This allows users to view on/off statistics for individual players, as well as combinations of multiple players. Users can also specify if they'd like specific players to be included or excluded from all lineups in use
* Players | players desired to be compared with on/off
* Lineup_Data | lineups collected from the get_lineups() function
* Included These Players will be on the court for every lineup considered.
* Excluded These players will be off the court for every lineup considered.

### `get_player_lineups`
This function finds all lineups from a given lineup data source that include/exclude certain players. It acts as a quick way to filter lineups for players.
* Takes same parameters as on_off_generator() with exception to the Players parameter

### `get_player_stats`
This function calculates many player stats for either individual games or aggregate to get multi-game stats.
* Takes all parameters described in get_lineups() function
* multi.games | When false stats will be calculated on a game level. When true all games will be aggregated.
