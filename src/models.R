library('ProjectTemplate')
library('tidyverse')
library('tidymodels')

load.project()

#[1] "loc"               "v.g."              "ev.g."             "iv.g."
#[5] "n"                 "v"                 "l"                 "d"
#[9] "i"                 "e"                 "b"                 "t"
#[13] "lOCode"            "lOComment"         "lOBlank"           "locCodeAndComment"
#[17] "uniq_Op"           "uniq_Opnd"         "total_Op"          "total_Opnd"
#[21] "branchCount"       "defects"

# Prepare v-fold cross-validation
set.seed(123)
train_folds = vfold_cv(train)

# Try logistic model workflow
basic_rec <- recipe(defects ~ ., data=train) %>%
  step_best_normalize(all_numeric_predictors()) %>%
  step_corr(threshold = 0.90) %>%
  step_normalize() %>%
  step_zv()

interaction_rec <- recipe(defects ~ ., data=train) %>%
  step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
  step_best_normalize(all_numeric_predictors()) %>%
  step_corr(threshold = 0.90) %>%
  step_normalize() %>%
  step_zv()

ns_rec <- recipe(defects ~ ., data=train) %>%
  step_ns(all_numeric_predictors(), deg_free = 5) %>%
  step_normalize() %>%
  step_zv()

preproc <-
  list(
       #basic = basic_rec,
       #interact = interaction_rec,
       ns = ns_rec
  )

log_reg_spec <- logistic_reg(penalty = tune()) %>%
  set_engine("glmnet")

log_reg_wfs <- workflow_set(preproc, list(lm = log_reg_spec), cross = FALSE)

# Tune hyper-parameters
all_cores <- parallel::detectCores(logical = FALSE) / 4

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
               seed = 1100,
               verbose = TRUE,
               resamples = train_folds,
               control = grid_control)
endTime <- Sys.time()
print(endTime - startTime)

collect_metrics(log_reg_wfs_tune)



