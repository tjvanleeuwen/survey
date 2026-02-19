rm(list = ls())

library(dplyr)
library(here)


source(here("R", "utils.R"))
source(here("R", "clean.R"))
source(here("R", "mc_byquestion.R"))

full_data <- read_data()
data <- clean(full_data, cutoff=20)

labs <- data[[1]]
vals <- data[[2]]
ques <- data[[3]]

possible_options <- ques$id[ques$type %in% c("disagree/agree", "never/always")]
# print(possible_options)

question <- "Work_JobSat_1"

categories <- c("Gen_Org", "Gen_Progress", "Gen_Gender", "Gen_FTE")

build <- build_figure(labs, ques, question, categories)
fig <- build[[1]]
n_cols <- build[[2]]
n_rows <- build[[3]]

print(fig)
ggsave("./figures/testfig.pdf", fig, width = 3 * n_cols, 
       height = 3 * n_rows, dpi = 300)