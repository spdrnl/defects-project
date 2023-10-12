# Example preprocessing script.
train <- train %>%
  select(-id) %>%
  mutate(defects = as.factor(defects))
  
