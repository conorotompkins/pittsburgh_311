library(broom)
library(ggfortify)
library(ggrepel)

#souce https://tbradley1013.github.io/2018/02/01/pca-in-a-tidy-verse-framework/

options(scipen = 999)
set.seed(1234)

df_months_pca <- df_months %>% 
  nest() %>% 
  mutate(pca = map(data, ~ prcomp(.x %>% select(-request_type), 
                                  center = TRUE, scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))


#dont need this. use the tidy df from months_PCA.R
df_months_pca
df_months_pca$pca_aug[[1]] %>% 
  as_tibble() %>% 
  select(request_type, .fittedPC1, .fittedPC2) %>% 
  mutate(outlier = case_when(abs(.fittedPC1) > 2.5 & abs(.fittedPC2) > 1.5 ~ TRUE)) -> df_pca 

df_pca %>% 
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_point() +
  geom_label_repel(data = df_pca %>% filter(outlier), 
             aes(.fittedPC1, .fittedPC2, label = request_type)) +
  theme_bw()

#might not need this. just use the tidy df
var_exp <- df_months_pca %>% 
  unnest(pca_aug) %>% 
  summarize_at(.vars = vars(contains("PC")), .funs = funs(var)) %>% 
  gather(key = pc, value = variance) %>% 
  mutate(var_exp = variance/sum(variance),
         cum_var_exp = cumsum(var_exp),
         pc = str_replace(pc, ".fitted", ""))

var_exp

var_exp %>% 
  rename(
    `Variance Explained` = var_exp,
    `Cumulative Variance Explained` = cum_var_exp
  ) %>% 
  gather(key = key, value = value, `Variance Explained`:`Cumulative Variance Explained`) %>% 
  mutate(pc = factor(pc, levels = unique(.$pc))) %>% 
  ggplot(aes(pc, value, group = key)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~key, scales = "free_y") +
  theme_bw() +
  lims(y = c(0, 1)) +
  labs(y = "Variance",
       title = "Variance explained by each principal component")

#just geom_point
df_months_pca %>%
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, loadings = TRUE, loadings.label = TRUE,
                 loadings.label.repel = TRUE,
                 data = .y) +
        theme_bw() +
        labs(x = "Principal Component 1",
             y = "Principal Component 2",
             title = "First two principal components of PCA on 311 dataset")
    )
  ) %>%
  pull(pca_graph)

#geom_label, but overplotted
df_months_pca %>%
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, loadings = TRUE, loadings.label = TRUE,
                 loadings.label.repel = TRUE,
                 data = .y, label = TRUE,
                 label.label = "request_type",
                 label.repel = TRUE) +
        theme_bw() +
        labs(x = "Principal Component 1",
             y = "Principal Component 2",
             title = "First two principal components of PCA on 311 dataset")
    )
  ) %>%
  pull(pca_graph)
#ggsave("images/month_pca_graph.png")

#create outlier graph, use tidy df
#ggplot(aes(PC1, PC2))