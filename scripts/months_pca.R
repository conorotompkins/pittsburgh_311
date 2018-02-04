library(broom)
source("scripts/month_clustering.R")

df_months %>% 
  ungroup() %>% 
  remove_rownames() %>% 
  column_to_rownames(var = "request_type") -> df_months

pc <- prcomp(df_months, scale = TRUE)

# information about rotation
head(tidy(pc))

# information about samples (request types)
head(tidy(pc, "samples"))

# information about PCs
tidy(pc, "pcs") 

au <- augment(pc, data = df_months)
head(au)

ggplot(au, aes(.fittedPC1, .fittedPC2)) +
  geom_point() +
  geom_text(aes(label = .rownames), vjust = 1, hjust = 1) +
  theme_bw()
