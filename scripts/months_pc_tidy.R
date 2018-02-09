library(broom)
library(ggfortify)

options(scipen = 999)


df_months_pca <- df_months %>% 
  nest() %>% 
  mutate(pca = map(data, ~ prcomp(.x %>% select(-request_type), 
                                  center = TRUE, scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))

df_months_pca

var_exp <- df_months_pca %>% 
  unnest(pca_aug) %>% 
  summarize_at(.vars = vars(contains("PC")), .funs = funs(var)) %>% 
  gather(key = pc, value = variance) %>% 
  mutate(var_exp = variance/sum(variance),
         cum_var_exp = cumsum(var_exp),
         pc = str_replace(pc, ".fitted", ""))

var_exp
