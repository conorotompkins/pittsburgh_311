source("scripts/load_data.R")

df %>% 
  count(request_type, sort = TRUE) %>% 
  filter(n > 400) -> df_top_requests

df %>%
  select(-time) %>% 
  semi_join(df_top_requests) %>% 
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
