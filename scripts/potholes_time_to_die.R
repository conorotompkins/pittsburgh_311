library(tidyverse)
library(janitor)

df <- read_csv("https://data.wprdc.org/datastore/dump/76fda9d0-69be-4dd5-8108-0de7907fc5a4")

df <- df %>% 
  clean_names() %>% 
  mutate(status = factor(status, labels = c("New", "Closed", "Open")))

df %>% 
  select(created_on, request_type, status) %>% 
  filter(str_detect(request_type, "Potholes")) %>% 
  count(created_on, status)
