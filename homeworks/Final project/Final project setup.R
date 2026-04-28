library(lubridate)
library(dplyr)

trump_posts <- trump_posts_full %>%
  mutate(
    created_at = ymd_hms(created_at, tz = "UTC")
  ) %>%
  filter(
    created_at > ymd_hms("2025-01-20 00:00:00", tz = "UTC")
  )

write.csv(trump_posts, file="trump_posts.csv")