
library(parsnip)
library(yardstick)


# Steg 1 ------------------------------------------------------------------

formel <- as.formula(
  ad_tot_price ~
    ad_sqm
)


linear_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(formel, data = finn_train_raw)
  

# Steg 2 ------------------------------------------------------------------

lm_recipe <- recipe(ad_tot_price ~ ., data = finn_train_raw) %>% 
  step_rm(ad_id) %>% 
  step_other(ad_home_type, threshold = 100) %>% 
  prep()

finn_train <- bake(lm_recipe, finn_train_raw)
finn_test  <- bake(lm_recipe, finn_test_raw)

formel <- as.formula(
  ad_tot_price ~
    ad_sqm
  + ad_home_type
)

linear_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(formel, data = finn_train)
  

summary(linear_model$fit)


prediction <- predict(linear_model, finn_test) %>%
  bind_cols(finn_test_raw) %>%
  rename(estimate     = .pred,
         truth        = ad_tot_price)

# Evaluate model
multi_metric <- metric_set(rmse)

prediction %>%
  multi_metric(truth = truth, estimate = estimate)



# OPPGAVER!



# Check out the pdp-plot
# Note how unrealistic this is - it will just keep increasing...
linear_model$fit %>%
  pdp::partial(pred.var = "ad_sqm", train = finn_train) %>%
  autoplot() +
  theme_light()




# XGBoost -----------------------------------------------------------------

xg_recipe <- recipe(ad_tot_price ~ ., data = finn_train_raw) %>% 
  step_rm(ad_id) %>% 
  step_other(ad_home_type, threshold = 100) %>% 
  prep()

finn_train <- bake(xg_recipe, finn_train_raw)
finn_test  <- bake(xg_recipe, finn_test_raw)

formel <- as.formula(
  ad_tot_price ~
    ad_sqm
  + ad_home_type
)

xg_model <- xg_mod <- boost_tree(mode = "regression",
                                 trees = 300
                                 ) %>% 
  set_engine("xgboost", tree_method = "exact") %>% 
  fit(formel, data = finn_train)


prediction <- predict(xg_model, finn_test) %>%
  bind_cols(finn_test_raw) %>%
  rename(estimate     = .pred,
         truth        = ad_tot_price)

# Evaluate model
multi_metric <- metric_set(rmse)

prediction %>%
  multi_metric(truth = truth, estimate = estimate)


xgboost::xgb.importance(model = xg_model$fit) %>% 
  xgboost::xgb.plot.importance()







