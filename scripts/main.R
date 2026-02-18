rm(list = ls())

library(dplyr)
library(here)


source(here("R", "utils.R"))
source(here("scripts", "clean.R"))

full_data <- read_data()
data <- clean(full_data)

labs <- data[[1]]
vals <- data[[2]]
ques <- data[[3]]
