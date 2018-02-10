library(broom)

options(scipen = 999)

source("scripts/neighborhood_clustering.R")

#sources
#https://rdrr.io/cran/broom/man/prcomp_tidiers.html
#https://poissonisfish.wordpress.com/2017/01/23/principal-component-analysis-in-r/
#http://rstatistics.net/principal-component-analysis/

set.seed(1234)

df_nbh

df_nbh %>% 
  remove_rownames() %>% 
  column_to_rownames(var = "neighborhood") -> df_nbh


pc <- prcomp(df_nbh, scale = TRUE)

# information about rotation
head(tidy(pc))

# information about samples (neighborhoods)
head(tidy(pc, "samples"))

# information about PCs
head(tidy(pc, "pcs"))

au <- augment(pc, data = df_nbh)
head(au)

ggplot(au, aes(.fittedPC1, .fittedPC2)) +
  geom_point() +
  geom_label(aes(label = .rownames)) +
  theme_bw()
ggsave("images/311_neighborhood_PCA.png", height = 12, width = 12)
