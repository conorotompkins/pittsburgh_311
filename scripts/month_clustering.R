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
  count(request_type, sort = TRUE)


#df %>% 
#  count(date, request_origin) %>% 
#  ggplot(aes(date, n, color = request_origin)) +
#  geom_smooth()

df %>%
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
  ggplot(aes(Jan, Jul, group = request_type)) +
  geom_point()

df_months %>% 
  select(request_type, month, month_percentage) %>% 
  spread(month, month_percentage) -> df_months



