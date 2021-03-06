source("scripts/load_data.R")

theme_set(theme_bw(base_family = 18))

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


