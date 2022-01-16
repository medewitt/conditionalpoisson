library(httr)
library(rvest)
library(dplyr)
hospitalizations <- read_html("https://conedatascience.github.io/covid-dashboard")

all_src<- hospitalizations %>%
  html_nodes("script") %>%
  .[[74]] %>%
  html_text()

tmp <- tempfile(fileext = ".json")

writeLines(all_src, tmp)

hospitalization_data <- jsonlite::fromJSON(tmp)

raw_dat <- data.table::rbindlist(hospitalization_data[["x"]][["hc_opts"]][["series"]][["data"]])

raw_dat <- within(raw_dat, DateDT <- as.Date(DateDT))

data.table::fwrite(raw_dat, here::here("data-raw", "variant.csv"))
