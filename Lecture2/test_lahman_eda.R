# PURPOSE: Initial Exploration of the Lahman data
# Author: Ron Yurko

# Access Packages ----

library(tidyverse)
library(Lahman)

# Load Lahman batting data ----

# Initialize the batting data
batting <- as_tibble(Batting)
dim(batting)
nrow(batting)
ncol(batting)

head(batting)

class(batting)

colnames(batting)


# Explore How Baseball has changed over time ------------------------------
summary(batting$yearID)

table(batting$lgID)

mlb_batting <- filter(batting, lgID %in% c('AA')) 
sel_batting <- select(batting, yearID, lgID, G, AB, R, H, HR, BB, SO)
sel_batting

mutate(sel_batting, batting_avg = H/AB)


## top 5 batting averages in descending order
batting %>% 
  filter(lgID %in% c("AL", "AL") , AB > 300) %>% 
  mutate(BA = H / AB) %>% 
  arrange(desc(BA)) %>% 
  dplyr::select(playerID, yearID, BA) %>% 
  slice(1:5) 

## top 5 batting averages in descending order
batting %>% 
  filter(lgID %in% c("AL", "AL") , AB > 300) %>% 
  group_by(yearID) %>% 
  mutate(hr = sum(HR), so = sum(SO), bb = sum(BB)) %>% 
  arrange(desc(hr))

year_batter_summary <- batting %>% 
  filter(lgID %in% c("AL", "NL")) %>% 
  group_by(yearID) %>% 
  summarize(total_hits = sum(H, na.rm = TRUE),
            total_hrs = sum(HR, na.rm = TRUE),
            total_sos = sum(SO, na.rm = TRUE),
            total_walks = sum(BB, na.rm = TRUE),
            total_atbats = sum(AB, na.rm = TRUE)) %>% 
  mutate(batting_avg = total_hits / total_atbats)


year_batter_summary %>% 
  arrange(desc(total_hrs)) %>% 
  slice(1:3)



# nice looking table output -----------------------------------------------

test_table <- year_batter_summary %>% 
  dplyr::select(yearID, batting_avg) %>% 
  rename(Year = yearID, `Batting AVG` = batting_avg)

library(gt)

year_batter_summary %>% 
  dplyr::select(yearID, batting_avg) %>% 
  rename(Year = yearID, `Batting AVG` = batting_avg) %>% 
  arrange(desc(`Batting AVG`)) %>% 
  slice(c(1:3, (n() -2): n())) %>% 
  gt() %>% 
  tab_header(
    title = "Best and worst MLB seasons by AVG",
    subtitle = "Top and bottom 3 displayed"
  )

library(usethis)
use_git_config(user.name = "test test", user.email = "test@gmail.com")




