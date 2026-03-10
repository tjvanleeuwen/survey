rm(list = ls())

library(tidyverse)

source("./R/clean.R")
source("./R/mc_byquestion.R")
source("./R/mc_bybase.R")

unlink("./figures/*")

full_data <- read_data(
  filepath_labs="./data/labels.csv", 
  filepath_vals="./data/values.csv"
)
data <- clean(full_data, cutoff=20)

labs <- data[[1]]
vals <- data[[2]]
ques <- data[[3]]

possible_questions <- ques$id[ques$type %in% c("disagree/agree", "never/always")]
# print(possible_questions)

question <- sample(possible_questions, size=1)

categories <- c("Gen_Org", "Gen_Progress", "Gen_Gender", "Gen_Nationality", "Gen_Studies")

if(T){
  build <- build_figure(labs, ques, question, categories)
  # print(build$plot)
  ggsave("./figures/testfig1.pdf", build$plot, width = build$width,
         height = build$height, dpi = 300) 
}

possible_bases <- unique(remove_na(
  ques$base[ques$type %in% c("never/always", "disagree/agree")]
))
# print(possible_bases)

categories <- c(NULL, categories)

base <- sample(possible_bases, size=1)
category <- sample(categories, size=1)

if(T){
  build <- build_bybase(labs, ques, base, category)
  # print(build$fig)
  ggsave("./figures/testfig2.pdf", build$fig, width = build$width, 
         height=build$height, dpi = 300)
}





source("../fig_types.R")


generate_figures <- function(type_list, k){
  if (k > length(type_list$bases))
    stop("Base index out of bounds!")
  
  base <- type_list$bases[k]
  for (i in seq_along(type_list$categories)){
    category <- type_list$categories[i]
    filename <- paste0("./figures/fig_", k, "_", i, ".pdf")
    build <- build_bybase(labs, ques, base, category)
    ggsave(filename, build$fig, width = build$width, 
           height=build$height, dpi = 300)
  }
}

generate_figures(workload, 1)










