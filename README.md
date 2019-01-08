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
```4674164```` is the game id for ```https://stats.ncaa.org/game/play_by_play/4674164```

- `get_date_games` data frame with game info and game ids for a given date.
- `get_team_schedule` data frame with game info and ids for a given team.
- `get_team_roster` data frame with player roster information for a given team. 

**Game Scraping Functions**
- `scrape_game` the base function, given a game id will return a cleaned play by play file.
- `get_play_by_play` builds off of scrape_game(), allows for compiling from multiple game ids formatted in a vector.

**Data Manipulation Functions**
- `get_lineups` given play-by-play data retrieved from game scraping functions, will calculate a variety of stats for all lineups.
- `on_off_generator` calculate on/off statistics for specified players given lineup data from get_lineups().
- `get_player_stats` calculate a variety of player stats from play-by-play data at either a game or multi-game level.
- `get_player_lineups` acts as an easy way to filter lineup data, specifying players to exclude/include from the lineups.

### Use

A quick progression of functions
``` r
# Get the game ids using one of two functions or this could be manual
game_ids <- get_id_date(Season = "20182019", Year = "2018", Month = "12", Day = "09")
# game_ids <- get_id_schedule(Season = "20182019", teams = "BUF")
pbp_data <- get_play_by_play(game_ids)
player_stats <- get_player_summary(pbp_data)
team_stats <- get_team_summary(pbp_data)

```

### `get_id_date`
This function takes in several parameters and will return a vector of game ids for a given date
* Season | an 8 digit season, ex. "20182019"
* Year | a four digit year parameter yyyy, ex. "2018"
* Month | The specified month, must be using format mm, ex. April = "04"
* Day | The specified date, must be using format dd, ex. the eighth = "08"

### `get_id_schedule`
This function returns all game ids for given teams in a given season. Default is set to 20182019, and all teams.
* Season | an 8 digit season, ex. "20182019"
* Teams | the team abbreviations used by the NWHL- as follows "BOS", "BUF", "CTW", "MET", "MIN"

### `get_play_by_play`
This function will return a tidy play-by-play file given a single, or multiple game ids
* game_ids | vector of game ids in either character or numeric format

### `get_player_summary`
This function will give you player stats for all games specified in the play-by-play.
* pbp_df | dataframe consisting of play by play data from get_play_by_play

### `get_team_summary`
This function will give you team stats for all games specified in the play-by-play
* pbp_df | dataframe consisting of play by play data from get_play_by_play


