library('ProjectTemplate')
load.project()

# Prepare v-fold cross-validation

train_folds = vfold_cv(train)

# Try logistic model workflow
log_reg_spec <- logistic_reg(penalty = tune()) %>%
  set_engine("glmnet")

regression_recipe <- recipe(defects ~ ., data=train) %>%
  step_corr(threshold = 0.90) %>%
  step_best_normalize(all_numeric_predictors()) %>%
  step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
  step_normalize() %>%
  step_zv()

log_reg_workflow <- workflow() %>%
  add_recipe(regression_recipe) %>%
  add_model(log_reg_spec)

# Tune hyper-parameters
all_cores <- parallel::detectCores(logical = FALSE) / 2

library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

grid_control <- control_grid(
  allow_par = TRUE,
  parallel_over = "resamples"
)

startTime <- Sys.time() 
log_reg_tune_res <- log_reg_workflow %>% 
  tune_grid(
    resamples = train_folds,
    control = grid_control
  )
endTime <- Sys.time() 
print(endTime - startTime)


# Finalize the model with the best parameters on the complete train set
log_reg_final <-
  finalize_workflow(log_reg_workflow, select_best(log_reg_tune_res)) %>%
  fit(train)

# Predict the test set
log_reg_final %>% 
  predict(test) %>% 
  bind_cols(test) 

