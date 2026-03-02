rm(list = ls())

library(tidyverse)

source(here::here("R", "clean.R"))
source(here::here("R", "mc_byquestion.R"))

full_data <- read_data()
data <- clean(full_data, cutoff=20)

labs <- data[[1]]
vals <- data[[2]]
ques <- data[[3]]

possible_options <- ques$id[ques$type %in% c("disagree/agree", "never/always")]
# print(possible_options)

question <- sample(possible_options, size=1)

categories <- c("Gen_Org", "Gen_Progress", "Gen_Gender", "Gen_FTE")

build <- build_figure(labs, ques, question, categories)

print(build$plot)
ggsave("./figures/testfig.pdf", build$plot, width = build$width, 
       height = build$height, dpi = 300)
   