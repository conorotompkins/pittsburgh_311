source("scripts/load_data.R")

df %>%
  filter(!is.na(neighborhood)) -> df

df %>% 
  count(request_type, sort = TRUE) %>% 
  top_n(25) -> df_top_requests

request_list <- unique(df_top_requests$request_type)

df %>%
  semi_join(df_top_requests) %>% 
  group_by(neighborhood, request_type) %>% 
  summarize(n = n()) %>% 
  complete(neighborhood, request_type = request_list) %>% 
  replace_na(replace = list(n = 0)) -> df_nbh

df_nbh %>%
  filter(is.na(neighborhood))

df_nbh %>% 
  group_by(neighborhood) %>% 
  mutate(neighborhood_total = sum(n)) -> df_nbh

df_nbh %>% 
  mutate(neighborhood_percentage = n / neighborhood_total) %>% 
  arrange(neighborhood, desc(neighborhood_total), desc(neighborhood_percentage)) -> df_nbh

df_nbh %>% 
  select(request_type, neighborhood, neighborhood_percentage) %>% 
  spread(request_type, neighborhood_percentage) %>%
  ungroup() -> df_nbh