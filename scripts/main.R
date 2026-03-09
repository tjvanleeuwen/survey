rm(list = ls())

library(tidyverse)

source(here::here("R", "clean.R"))
source(here::here("R", "mc_byquestion.R"))
source(here::here("R", "mc_bybase.R"))

full_data <- read_data(
  filepath_labs="../labels.csv", 
  filepath_vals="../values.csv"
)
data <- clean(full_data, cutoff=20)

labs <- data[[1]]
vals <- data[[2]]
ques <- data[[3]]

possible_questions <- ques$id[ques$type %in% c("disagree/agree", "never/always")]
# print(possible_questions)

question <- sample(possible_questions, size=1)

categories <- c("Gen_Org", "Gen_Progress", "Gen_Gender", "Gen_FTE", "Gen_Studies")

if(T){
  build <- build_figure(labs, ques, question, categories)
  
  print(build$plot)
  ggsave("./figures/testfig1.pdf", build$plot, width = build$width,
         height = build$height, dpi = 300) 
}

possible_bases <- unique(remove_na(
  ques$base[ques$type %in% c("never/always", "disagree/agree")]
))
# print(possible_bases)

base <- sample(possible_bases, size=1)
category <- sample(categories, size=1)

if(T){
  build <- build_bybase(labs, ques, base, category)
  print(build$fig)
  ggsave("./figures/testfig2.pdf", build$fig, width = build$width, 
         height=build$height, dpi = 300)
}






