library('ProjectTemplate')
library('tidyverse')
library('tidymodels')

load.project()

# Prepare v-fold cross-validation

train_folds = vfold_cv(train)

# Try logistic model workflow
basic_rec <- recipe(defects ~ ., data=train) %>%
  step_corr(threshold = 0.90) %>%
  step_best_normalize(all_numeric_predictors()) %>%
  step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
  step_normalize() %>%
  step_zv()

interaction_rec <- recipe(defects ~ ., data=train) %>%
  step_corr(threshold = 0.90) %>%
  step_best_normalize(all_numeric_predictors()) %>%
  step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
  step_normalize() %>%
  step_zv()

preproc <-
  list(basic = basic_rec,
       interact = interaction_rec
  )

log_reg_spec <- logistic_reg(penalty = tune()) %>%
  set_engine("glmnet")

log_reg_wfs <- workflow_set(preproc, list(lm = log_reg_spec), cross = FALSE)

# Tune hyper-parameters
all_cores <- parallel::detectCores(logical = FALSE) / 2

library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

grid_control <- control_grid(
  allow_par = TRUE,
  save_pred = TRUE,
  event_level = "second",
  parallel_over = "resamples"
)

startTime <- Sys.time()
log_reg_wfs_tune <- log_reg_wfs %>%
  workflow_map(fn = "tune_grid",
               grid = 10,
               seed = 1101,
               verbose = TRUE,
               resamples = train_folds,
               control = grid_control)
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

