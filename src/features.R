library('ProjectTemplate')
load.project()

# Check correlation between predictors
my_cor <- cor(train %>% select(-defects))
heatmaply_cor(my_cor)

# Filter features
feature_t_tests <- train %>%
  pivot_longer(-defects, names_to = 'feature', values_to = 'value') %>%
  group_by(feature) %>%
  wilcox_test(value ~ defects) %>%
  adjust_pvalue(method = "fdr") %>%
  arrange(-p) %>%
  add_significance()

