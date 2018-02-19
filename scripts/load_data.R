library(tidyverse)
library(lubridate)

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
  mutate(request_type = str_replace(request_type, "Snow/Ice removal", "Snow/Ice Removal")) %>% 
  ungroup() -> df