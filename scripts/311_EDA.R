library(tidyverse)
library(lubridate)

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
  count(request_type, sort = TRUE) %>% 
  top_n(10) -> df_top_requests

df %>% 
  right_join(df_top_requests) -> df

df %>% 
  count(year, yday, request_type) %>% 
  filter(str_detect(request_type, "Snow")) %>% 
  ggplot(aes(yday, nn, color = request_type)) +
  geom_point(alpha = .5) +
  geom_smooth() +
  facet_wrap(~year, ncol = 1)

df %>% 
  count(date, request_type) %>% 
  ggplot(aes(date, nn, fill = request_type)) +
  geom_bar(stat = "identity")

df %>% 
  count(date) %>%
  ggplot(aes(date, nn)) +
  geom_smooth()


