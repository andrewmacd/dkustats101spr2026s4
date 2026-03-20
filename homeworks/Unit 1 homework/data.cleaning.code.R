library(tidyverse)

spotify_alltime_top100_songs <- read.csv("E:/Github/dkustats101spr2026s4/homeworks/Unit 1 homework/spotify_alltime_top100_songs.csv")
spotify_wrapped_2025_top50_songs <- read.csv("E:/Github/dkustats101spr2026s4/homeworks/Unit 1 homework/spotify_wrapped_2025_top50_songs.csv")

spotify_alltime_top100_songs <- spotify_alltime_top100_songs %>% 
  mutate(dataset_part = ifelse(dataset_part=="Spotify All-Time Most Streamed Top 100", "Top 100", ""))

spotify_wrapped_2025_top50_songs <- spotify_wrapped_2025_top50_songs %>% 
  mutate(dataset_part = ifelse(dataset_part=="Spotify Wrapped 2025 Top 50 Global Songs", "2025 Top 50", ""))

spotify <- spotify_alltime_top100_songs %>% 
  full_join(spotify_wrapped_2025_top50_songs)

spotify <- spotify %>% 
  relocate(wrapped_2025_rank, .after=alltime_rank)

spotify <- spotify %>% 
  relocate(streams_2025_billions, .after=total_streams_billions)

write.csv(spotify, "spotify.csv")