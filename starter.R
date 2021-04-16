library(tidyverse)
library(tidymodels)
library(vip)
library(rpart.plot)
library(DataExplorer)

###DATA LOAD
bank<-read.csv("https://raw.githubusercontent.com/PatLittle/CIND119-group-project/main/bank_marketing/bank.csv")



###EDA

introduce(bank)
plot_intro(bank)
plot_missing(bank)
plot_bar(bank, by = "y")
plot_histogram(bank)

plot_correlation(na.omit(bank), type = "d")


str(bank)
colSums(is.na(bank))





###Decision Tree



bank_clean<- bank %>% mutate_if(is.character, factor)

set.seed(888)
bank_split <- initial_split(bank_clean, prop = 0.75, 
                             strata = y)

bank_training <- bank_split %>% training()
bank_test <- bank_split %>% testing()
bank_folds <- vfold_cv(bank_training, v = 10)



bank_recipe <- recipe(y ~ ., data = bank_training) %>% 
  step_YeoJohnson(all_numeric(), -all_outcomes()) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes())


bank_clean_baked<-bank_recipe %>% 
  prep() %>% 
  bake(new_data = bank_training)

tree_model <- decision_tree(cost_complexity = tune(),
                            tree_depth = tune(),
                            min_n = tune()) %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

tree_workflow <- workflow() %>% 
  add_model(tree_model) %>% 
  add_recipe(bank_recipe)

tree_grid <- grid_latin_hypercube(cost_complexity(),
                          tree_depth(),
                          min_n(), 
                          size = 30)

set.seed(888)

tree_tuning <- tree_workflow %>% 
  tune_grid(resamples = bank_folds,
            grid = tree_grid)

tree_tuning %>% show_best('roc_auc')

best_tree <- tree_tuning %>% 
  select_best(metric = 'roc_auc')


final_tree_workflow <- tree_workflow %>% 
  finalize_workflow(best_tree)


tree_wf_fit <- final_tree_workflow %>% 
  fit(data = bank_training)

tree_fit <- tree_wf_fit %>% 
  pull_workflow_fit()

vip(tree_fit)

rpart.plot(tree_fit$fit, roundint = FALSE)

tree_last_fit <- final_tree_workflow %>% 
  last_fit(bank_split)

tree_last_fit %>% collect_metrics()


tree_last_fit %>% collect_predictions() %>% 
  roc_curve(truth  = y, estimate = .pred_no) %>% 
  autoplot()

tree_predictions <- tree_last_fit %>% collect_predictions()

conf_mat(tree_predictions, truth = y, estimate = .pred_class)

predict(tree_last_fit$.workflow[[1]],bank_test[15,])

saveRDS(tree_last_fit$.workflow[[1]],"./saved_model.Rds")

trained_model<-readRDS("saved_model.Rds")

###Naive Bayes

set.seed(888)
nb_split <- initial_split(bank_clean, prop = 0.75, 
                            strata = y)

nb_training <- nb_split %>% training()
nb_test <- nb_split %>% testing()
nb_folds <- vfold_cv(nb_training, v = 10)

nb_recipe <- recipe(y ~ ., data = nb_training)
  


nb_wf <- workflow() %>%
  add_recipe(nb_recipe)

library(discrim)
nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")

nb_spec

nb_fit <- nb_wf %>%
  add_model(nb_spec) %>%
  fit(data = nb_training)

nb_wf_final <-  workflow() %>%
  add_recipe(nb_recipe) %>%
  add_model(nb_spec)

nb_rs <- fit_resamples(
  nb_wf_final,
  nb_folds,
  control = control_resamples(save_pred = TRUE)
)


nb_last_fit <- nb_wf_final %>% 
  last_fit(nb_split)

nb_last_fit %>% collect_metrics()

nb_predictions <- nb_last_fit %>% collect_predictions()
conf_mat(nb_predictions, truth = y, estimate = .pred_class)