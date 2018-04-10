source("scripts/load_data.R")

library(ggmap)
library(viridis)
library(scales)
library(knitr)
library(kableExtra)

theme_set(theme_bw(base_family = 18))

df %>% 
  filter(request_type %in% c("Potholes", "Snow/Ice Removal")) %>% 
  count(request_type, month) %>% 
  ggplot(aes(month, n, group = request_type, fill = request_type)) +
  geom_area() +
  scale_fill_viridis("Request type", discrete = TRUE, option = "D") +
  scale_y_continuous(expand = c(0, 0),
                     labels = comma) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(title = "The Two Seasons In Pittsburgh",
       subtitle = "311 requests",
       x = "",
       y = "Number of requests",
       caption = "Conor Tompkins - @conor_tompkins") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
ggsave("images/the_two_seaons_in_pittsburgh.png", height = 8, width = 12)
