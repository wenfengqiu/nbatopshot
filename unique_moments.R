# this script generates the unique moments defined in various ways
# import moment data from https://otmnft.com/moments/

library(tidyverse)

# import csv file
setwd("./nbatopshot/")
moment_df <- read_csv("moments_20220125.csv")

# obtain a player table
player_df <- moment_df %>% group_by(`Player Name`, `Tier`, `Series`) %>%
  summarise(counts = n())

# extract the list of players with exact one moment all time
rarest_df <- moment_df %>% filter(Series != "S21") %>% group_by(`Player Name`) %>%
  summarise(counts = n()) %>% filter(counts == 1)
rarest_df <- moment_df %>% right_join(rarest_df) %>% arrange(`Low Ask`) %>%
  filter(Series != "S21") %>% select(c("Player Name", "Series", "Tier", "Low Ask"))

# extract the list of players with more than one moment but only one common
multiple_player_list <- player_df %>% filter(counts > 1) %>% pull(`Player Name`) 
rare_df <- moment_df %>% filter(`Player Name` %in% multiple_player_list) %>%
  filter(Series != "S21") %>% filter(Tier == "Common") %>%
  group_by(`Player Name`) %>% summarise(counts = n(), ask = min(`Low Ask`)) %>%
  filter(counts == 2)




  
