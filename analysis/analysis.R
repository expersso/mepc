library(readr)
library(plyr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(tidyr)
library(feather)
library(caret)
# library(doMC)

# registerDoMC(parallel::detectCores())
set.seed(1)

read_clean <- function(files) {
  files %>%
    map(read_feather) %>%
    set_names(basename(files) %>% str_replace("\\.feather", ""))
}

# IO
train <- list.files("data/train", full.names = TRUE) %>% read_clean()

train_df <- inner_join(train$power, train$ltdata) %>%
  sample_frac(0.5) %>%
  gather(powerline, power, starts_with("NPWD")) %>%
  mutate(turnon = (ut_ms > ymd_hms("2010-12-14 16:00:00")) &
           powerline %in% c("NPWD2472", "NPWD2792"))

train_idx <- createDataPartition(train_df$power, p = .75, list = FALSE)
training <- train_df[train_idx, ]
validation <- train_df[-train_idx, ]

rm(train_idx, train_df, train)

# fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
# m1 <- train(power ~ powerline*sunmars_km + powerline*earthmars_km +
#               powerline*sunmarsearthangle_deg + powerline*solarconstantmars,
#             data = training,
#             method = "lm",
#             trControl = fitControl)

m1 <- lm(power ~ powerline*sunmars_km + powerline*earthmars_km +
           powerline*sunmarsearthangle_deg + powerline*solarconstantmars + turnon,
         training)

RMSE(predict(m1, validation), validation$power)

test <- list.files("data/test", full.names = TRUE) %>% read_clean()
test_df <- inner_join(test$power, test$ltdata) %>%
  gather(powerline, power, starts_with("NPWD")) %>%
  mutate(turnon = (ut_ms > ymd_hms("2010-12-14 16:00:00")) &
           powerline %in% c("NPWD2472", "NPWD2792"))

rm(test, training, validation)

submit_df <- test_df %>%
  mutate(power = predict(m1, test_df)) %>%
  select(ut_ms, powerline, power) %>%
  spread(powerline, power) %>%
  mutate(ut_ms = as.numeric(ut_ms) * 1000)
write_csv(submit_df, "submit_df.csv")
