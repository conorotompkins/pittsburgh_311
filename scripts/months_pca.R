library(broom)

options(scipen = 999)

source("scripts/month_clustering.R")

#sources
#https://rdrr.io/cran/broom/man/prcomp_tidiers.html
#https://poissonisfish.wordpress.com/2017/01/23/principal-component-analysis-in-r/
#http://rstatistics.net/principal-component-analysis/

set.seed(1234)

df_months %>% 
  ungroup() %>% 
  remove_rownames() %>% 
  column_to_rownames(var = "request_type") -> df_months

df_months %>% 
  prcomp(scale = TRUE) -> pc

#pc <- prcomp(df_months, scale = TRUE)

# information about rotation
pc %>% 
  tidy() %>% 
  head()
#head(tidy(pc))

# information about samples (request types)
pc %>% 
  tidy("samples") %>% 
  head()
#head(tidy(pc, "samples"))

# information about PCs
pc %>% 
  tidy("pcs")

#tidy(pc, "pcs") 
pc %>% 
  augment(data = df_months) -> au


#au <- augment(pc, data = df_months)
head(au)

ggplot(au, aes(.fittedPC1, .fittedPC2)) +
  geom_point() +
  geom_label(aes(label = .rownames)) +
  theme_bw()
ggsave("images/311_request_type_month_proportion_PCA.png", height = 12, width = 12)
