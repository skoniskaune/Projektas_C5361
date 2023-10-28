#install.packages("tidyverse")
library(tidyverse)

data <- read_csv("../../../project/1-data/1-sample_data.csv")
data_additional <- read_csv("../../../project/1-data/3-additional_features.csv")

joined_data <- inner_join(data, data_additional, by = "id")

write_csv(joined_data, "../../../project/1-data/train_data.csv")
