library(h2o)
library(readr)
library(tidyverse)

h2o.init(max_mem_size = "32g")

df <- h2o.importFile("../train_data.csv")
test_data <- h2o.importFile("../test_data.csv")

y <- "y"
x <- setdiff(names(df), c(y, "id"))
df$y <- as.factor(df$y)
print(h2o.isfactor(df["y"]))

h2o.impute(df,"yearly_income", method="mean", by=c("home_ownership"))
h2o.impute(df,"bankruptcies", method="mean", by=c("credit_problems"))
h2o.impute(df,"credit_score", method="mode",by=c("credit_problems"))
h2o.impute(df,"years_current_job", method="mean")
h2o.impute(df,"max_open_credit", method="median")

splits <- h2o.splitFrame(df, c(0.6,0.2), seed=101)
train  <- h2o.assign(splits[[1]], "train")
valid  <- h2o.assign(splits[[2]], "valid") 
test   <- h2o.assign(splits[[3]], "test") 


ntrees_opt <- c(20,35,50)
maxdepth_opt <- c(10,15)
learnrate_opt <- c(0.01,0.05)
hyper_parameters <- list(ntrees=ntrees_opt,
                         max_depth=maxdepth_opt, learn_rate=learnrate_opt)

gbm_grid <- h2o.grid(algorithm = "gbm",
                     grid_id = "ktu_grid",
                     x=x,
                     y=y,
                     training_frame = train,
                     validation_frame = valid,
                     stopping_metric = "AUC",
                     hyper_params = hyper_parameters)

grid_results <- h2o.getGrid("ktu_grid", sort_by = "auc", decreasing = TRUE)

#geriausi rezultatai su learn rate=0.05, max_depth=15, ntrees=50

best_model <- h2o.getModel(grid_results@model_ids[[1]])

h2o.performance(best_model,train = TRUE)
h2o.performance(best_model,valid = TRUE)
perf <- h2o.performance(best_model, newdata = test)

#0.8228195 AUC value for test data

plot(perf, type = "roc")

h2o.saveModel(best_model, "../4-model", filename = "my_best_automlmodel")

predictions <- h2o.predict(best_model, test)
predictions %>%
  as_tibble() %>%
  mutate(id = row_number(), y = p0) %>%
  select(id, y) %>%
  write_csv("../5-predictions/predictions1.csv")