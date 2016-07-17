library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(feather)

if (!dir.exists("analysis")) dir.create("analysis")
if (!dir.exists("data/train")) dir.create("data/train", recursive = TRUE)
if (!dir.exists("data/test")) dir.create("data/test", recursive = TRUE)

if (!dir.exists("data-raw/")) {
  url <- paste0("https://kelvins.esa.int/media/competitions/",
                "mars-express-power-challenge/mars-express-power-3years.zip")
  download.file(url, basename(url), mode = "wb")
  unzip(basename(url))
  file.rename("mars-express-power-3years/", "data-raw")
}

convert_time <- function(x) {
  (x / 1000) %>% as.POSIXct(tz = "UTC", origin = origin)
}

read_data <- function(file, freq = "hour") {
  df <- read_csv(file)
  df$ut_ms <- df$ut_ms %>% convert_time() %>% floor_date(freq)
  df
}

clean_numerical_df <- function(file, f = mean) {
  read_data(file) %>% group_by(ut_ms) %>% summarise_all(f)
}

clean_dmop <- function(file) {
  read_data(file) %>%
    mutate(
      system = str_sub(subsystem, 1, 4),
      command = str_sub(subsystem, 5) %>% str_replace("\\.", "")
    ) %>%
    group_by(ut_ms, system) %>%
    tally() %>%
    ungroup()
}

save_feather <- function(df, path = "data/train") {
  filename <- deparse(substitute(df))
  path <- file.path(path, filename) %>% paste0(".feather")
  write_feather(df, path)
  message(sprintf("Object \"%s\" saved to %s", filename, path))
}

make_test_power <- function(time_var) {
  matrix(NA, length(time_var), 33) %>%
    as_data_frame() %>%
    set_names(train$power %>% names() %>% keep(str_detect, "NPWD")) %>%
    mutate(ut_ms = time_var) %>%
    select(ut_ms, starts_with("NPWD"))
}

# IO
### Train data
train_raw <- list.files("data-raw/train_set/", full.names = TRUE)
ut_ms_train <- seq(ymd_hms("2008-08-22 00:00:00", tz = "UTC"),
                   ymd_hms("2014-04-13 23:00:00", tz = "UTC"), by = "hour")

power <- train_raw %>% keep(str_detect, "power") %>% map_df(clean_numerical_df)
save_feather(power)

saaf <- train_raw %>% keep(str_detect, "saaf") %>% map_df(clean_numerical_df)
save_feather(saaf)

ltdata <- train_raw %>% keep(str_detect, "ltdata") %>%
  map_df(
  ~read_data(.x) %>%
    filter(!duplicated(ut_ms))
  ) %>%
   complete(ut_ms = ut_ms_train) %>%
   fill(ut_ms:occultationduration_min, .direction = "down")
save_feather(ltdata)

dmop <- train_raw %>% keep(str_detect, "dmop") %>%
  map_df(clean_dmop) %>%
  complete(ut_ms = ut_ms_train, system, fill = list(n = 0))
save_feather(dmop)

### Test data
test_raw <- list.files("data-raw/test_set/", full.names = TRUE)
ut_ms_test <- seq(ymd_hms("2014-04-14 00:00:00", tz = "UTC"),
                  ymd_hms("2016-02-29 23:00:00", tz = "UTC"), by = "hour")

saaf <- test_raw %>% keep(str_detect, "saaf") %>% map_df(clean_numerical_df)
save_feather(saaf, "data/test")

ltdata <- test_raw %>% keep(str_detect, "ltdata") %>%
  map_df(
  ~read_data(.x) %>%
    filter(!duplicated(ut_ms))
  ) %>%
   complete(ut_ms = ut_ms_test) %>%
   fill(ut_ms:occultationduration_min, .direction = "down")
save_feather(ltdata, "data/test")

dmop <- test_raw %>% keep(str_detect, "dmop") %>%
  map_df(clean_dmop) %>%
  complete(ut_ms = ut_ms_test, system, fill = list(n = 0))
save_feather(dmop, "data/test")

power <- make_test_power(ut_ms_test)
save_feather(power, "data/test")
