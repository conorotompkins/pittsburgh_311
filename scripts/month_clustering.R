library(tidyverse)
library(lubridate)

df_raw <- read_csv("data/pittsburgh_311.csv")

df <- df_raw

colnames(df) <- tolower(colnames(df))

df %>%
  mutate(date = ymd(str_sub(created_on, 1, 10)),
         time = hms(str_sub(created_on, 11, 18)),
         month = month(date, label = TRUE)) -> df

df %>% 
  filter(str_detect(request_type, "Snow/Ice")) %>%  
  count(request_type)

df %>% 
  mutate(request_type = str_replace(request_type, "Snow/Ice removal", "Snow/Ice Removal")) -> df

df %>% 
  filter(str_detect(request_type, "Snow/Ice")) %>%  
  count(request_type)

df %>% 
  count(request_type, sort = TRUE) %>% 
  filter(n > 200) -> df_top_requests

#df %>% 
#  count(date, request_origin) %>% 
#  ggplot(aes(date, n, color = request_origin)) +
#  geom_smooth()

df %>%
  right_join(df_top_requests) %>% 
  group_by(request_type, month) %>% 
  summarize(n = n()) %>% 
  complete(request_type, month) %>% 
  replace_na(replace = list(n = 0)) -> df_months

df_months %>% 
  group_by(request_type) %>% 
  mutate(request_type_total = sum(n)) -> df_months

df_months %>% 
  mutate(month_percentage = n / request_type_total) -> df_months

df_months %>% 
  filter(is.na(month_percentage))

df_months %>% 
  filter(is.nan(month_percentage))

df_months %>% 
  select(request_type, month, month_percentage) %>% 
  spread(month, month_percentage) %>% 
  ungroup() -> df_months

df_months %>% 
  ggplot(aes(Jan, Jul)) +
  geom_point()

