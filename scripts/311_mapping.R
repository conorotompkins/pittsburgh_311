library(tidyverse)
library(ggmap)

theme_set(theme_bw())

df_raw <- read_csv("data/pittsburgh_311.csv")

df <- df_raw

colnames(df) <- tolower(colnames(df))

df %>%
  mutate(date = ymd(str_sub(created_on, 1, 10)),
         time = hms(str_sub(created_on, 11, 18)),
         month = month(date, label = TRUE), 
         year = year(date),
         yday = yday(date)) -> df

df %>% 
  mutate(request_type = str_replace(request_type, "Snow/Ice removal", "Snow/Ice Removal")) -> df

df %>%
  filter(str_detect(request_type, "Snow")) -> df_snow

pgh_map <- get_map(location = "The Hill, Pittsburgh PA", zoom = 12)
ggmap(pgh_map) +
  geom_point(data = df_snow, aes(x = x, y = y, color = neighborhood), alpha = .5, size = 2) +
  scale_color_discrete(guide = FALSE)

df_snow %>% 
  filter(neighborhood == "Central Business District") -> df_snow_downtown

downtown_map <- get_map(location = "Downtown, Pittsburgh PA", zoom = 15)
ggmap(downtown_map) +
  geom_point(data = df_snow_downtown, aes(x = x, y = y, color = neighborhood), alpha = 1, size = 2) +
  scale_color_discrete(guide = FALSE)
