# PURPOSE: Test out the sportyR package for plotting WNBA shots


# Load necessary packages -------------------------------------------------

library(tidyverse)

# Next install the sportyR package if you do not have it installed already:
#install.packages("sportyR")
# Now load it
library(sportyR)


# Read in the WNBA shots data ---------------------------------------------

wnba_shots <- read_csv('http://www.stat.cmu.edu/cmsac/sure/2021/materials/data/eda_projects/wnba_shots_2021.csv')


# Plot the WNBA shots data using sportyR ----------------------------------

# Check out the documentation for this function:
help("geom_basketball")

# First build this court using geom_basketball for WNBA data:
geom_basketball(league = "WNBA", 
                # I just want a half court in this case 
                full_surf = FALSE, 
                # Need to rotate it based on the coordinates I have
                rotate = TRUE) +
  # Now build the point layer, but only do so for the shots that are NOT
  # free throw attempts based on the shot type text. I'm using the str_detect
  # function from the stringr package to detect if the type_text contains the
  # "Free Throw" string. But I'm specifying here to NOT include such rows by 
  # setting negate = TRUE (if I set negate = FALSE, the default, then it would
  # filter the shots to be only the free throw attempts):
  geom_point(data = filter(wnba_shots, # 
                           str_detect(type_text, "Free Throw", negate = TRUE)), 
             # Now this adjustment is just necessary for the WNBA data, first
             # center the x coordinate by subtracting 25 (WNBA court is 94 by 50)
             aes(x = coordinate_x - 25, 
                 # Now subtract 47 but add back 4 for the backboard (this is
                 # not going to be perfect but still pretty darn close...)
                 y = coordinate_y - 47 + 4,
                 # Color by the shot outcome
                 color = scoring_play),
             alpha = 0.25) +
  # Add manual shot colors
  scale_color_manual(values = c("darkblue", "darkorange"))


# Create team table -------------------------------------------------------

# Next I want to facet this chart by the team that attempted the shot
# Unfortunately, this dataset only has a team_id column indicating which team
# attempted this shot - but it does have the IDs and names for both the home
# and away teams. So I can construct a table of team information based on the 
# home or away columns to then join back to my original dataset.

# First make this table for away team columns
away_teams <- wnba_shots %>%
  # Just select the columns with the away team info:
  dplyr::select(away_team_id, away_team_name, away_team_abbrev) %>%
  # Now only grab the distinct rows in this table using these columns -
  # this reduces the dataset down to just the unique teams in the away teams
  # columns
  distinct()

# I can repeat this same process for the home team columns
home_teams <- wnba_shots %>%
  dplyr::select(home_team_id, home_team_name, home_team_abbrev) %>%
  distinct()

# As it turns out these tables match (just different order), that's not 
# surprising since at this point in the year every team has played at least
# one home and one away game.

# I'll make a new table modifying the home_teams table (could do this for the 
# away_teams) and simply rename the columns to just drop the home_ portion
team_table <- home_teams %>%
  rename(team_id = home_team_id, team_name = home_team_name,
         team_abbrev = home_team_abbrev)


# View shot charts by team ------------------------------------------------

# Now I can join this info over back to the original shot data using left_join
wnba_shots <- wnba_shots %>%
  left_join(team_table, by = "team_id")
# Can see that the team name and abbreviation columns have been added

# Make the same plot as above but facet by the team_name to display
# each team's shot charts separately
geom_basketball(league = "WNBA", full_surf = FALSE, rotate = TRUE) +
  geom_point(data = filter(wnba_shots, 
                           str_detect(type_text, "Free Throw", negate = TRUE)), 
             aes(x = coordinate_x - 25, y = coordinate_y - 47 + 4,
                 color = scoring_play),
             alpha = 0.25, size = 0.5) + # modifying the point size
  scale_color_manual(values = c("darkblue", "darkorange")) +
  # Facet by team name and make 4 columns of plots
  facet_wrap(~ team_name, ncol = 4)
  
