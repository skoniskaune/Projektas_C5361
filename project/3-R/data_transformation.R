library(readr)
library(tidyverse)

data <- read_csv("../1-data/1-sample_data.csv")
data_additional <- read_csv("../1-data/3-additional_features.csv")
data_additional2 <- read_csv("../1-data/2-additional_data.csv")

head(data)
head(data_additional2)

appended_data <- rbind(data,data_additional2)

joined_data <- inner_join(appended_data, data_additional, by = "id")
write_csv(joined_data, "../1-data/train_data.csv")