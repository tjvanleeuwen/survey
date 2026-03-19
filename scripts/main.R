rm(list = ls())

library(tidyverse)

source("./R/clean.R")
source("./R/settings.R")
source("./R/mc_bybase.R")
source("./R/mc_byquestion.R")
source("./R/custom.R")
source("./R/statistics.R")


## ----- Clean data -----

full_data <- read_data(
  filepath_labs="./data/labels.csv", 
  filepath_vals="./data/values.csv"
)
data <- clean(full_data, cutoff=20)

labs <- data[[1]]
vals <- data[[2]]
ques <- data[[3]]


## ----- Clear saved figures -----

if (T){
  unlink("./figures/ordered/*")
  unlink("./figures/named/*")
  unlink("./figures/test/*")
}



## ----- Testing -----

if (T) {
  build <- build_bybase(
    data = labs,
    question_data = ques,
    base_data = bases,
    base = "Work_Teach_Say",
    answer_map = my_answer_map,
    category_map = my_category_map,
    category = "Gen_Gender",
    title = TRUE,
    style = my_style,
    fonts = my_fonts
  )
  save_build("test/testfig_base", build)
}


if (T) {
  categories <- c("Gen_Org", "Gen_Progress", "Gen_Gender", 
                  "Gen_Nationality", "Gen_Studies")
  build <- build_average(
    data = labs,
    base_data = bases,
    base = "MH_PSS",
    category_map = my_category_map,
    categories = categories,
    title = TRUE,
    style = my_style,
    fonts = my_fonts
  )
  save_build("test/testfig_average", build)
}




## ----- Statistics -----


if (T) {
  likert_questions <- ques$id[ques$type %in% c(
    "never/always", "disagree/agree", "ubos", "pss"
  )]
  
  pvals <- get_pvals(labs, likert_questions, "Gen_Gender", "Gen_Org")
  overall <- pvals$overall
  bycat <- pvals$bycat
  
  print(paste(
    "Gender:",
    length(overall$question[overall$significant])
  ))
  print(count(bycat[bycat$significant,], Gen_Org))
  
  pvals <- get_pvals(labs, likert_questions, "Gen_Nationality", "Gen_Org")
  overall <- pvals$overall
  bycat <- pvals$bycat
  
  print(paste(
    "Nationality:",
    length(overall$question[overall$significant])
  ))
  print(count(bycat[bycat$significant,], Gen_Org))
}



## ----- Figure generation -----

if (T){
  categories <- c("Gen_Org", "Gen_Progress", "Gen_Gender", 
                  "Gen_Nationality", "Gen_Studies")
  
  question_codes <- unique(ifelse(is.na(ques$base), ques$id, ques$base))
  
  for (k in 1:nrow(bases)){
    base <- bases$base[k]
    num <- which(question_codes == base)
    
    build <- build_average(
      data = labs,
      base_data = bases,
      base = base,
      category_map = my_category_map,
      categories = categories,
      style = my_style,
      fonts = my_fonts
    )
    save_build(paste0("ordered/fig", num, "_av"), build)
    save_build(paste0("named/", base, "_average"), build)
    
    build <- build_bybase(
      data = labs,
      question_data = ques,
      base_data = bases,
      base = base, 
      answer_map = my_answer_map,
      category_map = my_category_map,
      style = my_style,
      fonts = my_fonts
    )
    save_build(paste0("ordered/fig", num, "_0"), build)
    save_build(paste0("named/", base), build)
    
    for (i in seq_along(categories)){
      category <- categories[i]
      build <- build_bybase(
        data = labs,
        question_data = ques,
        base_data = bases,
        base = base, 
        answer_map = my_answer_map,
        category_map = my_category_map,
        category = category,
        style = my_style,
        fonts = my_fonts
      )
      save_build(paste0("ordered/fig", num, "_", i), build)
      save_build(paste0("named/", base, "_", sub("^Gen_", "", category)), build)
    }
  }
}


## ----- Custom figures -----

if (T){
  categories <- c("Gen_Org", "Gen_Progress", "Gen_Gender")
  
  question_codes <- unique(ifelse(is.na(ques$base), ques$id, ques$base))
  
  custom <- labs |> mutate(
    Work_Finish = fct_rev(Work_Finish),
    SS_IB_Exp = fct_rev(SS_IB_Exp)
  )
  
  for (question in c("Work_Finish", "SS_IB_Exp")){
    num <- which(question_codes == question)
    build <- list(
      fig = build_custom(custom, question, categories),
      width = 8,
      height = 5
    )
    save_build(paste0("ordered/fig", num), build)
    save_build(paste0("named/", question), build)
  }
}














