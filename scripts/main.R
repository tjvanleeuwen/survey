rm(list = ls())

library(tidyverse)

source("./R/clean.R")
source("./R/settings.R")
source("./R/mc_bybase.R")
source("./R/mc_byquestion.R")
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

if (F){
  unlink("./figures/ordered/*")
  unlink("./figures/named/*")
  unlink("./figures/test/*")
}



## ----- Variables -----

likert_questions <- ques$id[ques$type %in% c(
  "never/always", "disagree/agree", "ubos", "pss"
)]

twotype_categories <- c("Gen_Gender", "Gen_Nationality")

categories <- c("Gen_Org", "Gen_Progress", "Gen_Gender", 
                "Gen_Nationality", "Gen_Studies")

question_codes <- unique(ifelse(is.na(ques$base), ques$id, ques$base))
question_codes <- question_codes[!grepl("^Gen_", question_codes)]
question_codes <- c(categories, question_codes)


## ----- Testing -----

if (F) {
  build <- build_bybase(
    data = labs,
    question_data = ques,
    base_data = my_bases,
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

if (F) {
  categories <- c("Gen_Org", "Gen_Progress", "Gen_Gender", 
                  "Gen_Nationality", "Gen_Studies")
  build <- build_average(
    data = labs,
    base_data = my_bases,
    base = "MH_PSS",
    category_map = my_category_map,
    categories = categories,
    title = TRUE,
    style = my_style,
    fonts = my_fonts
  )
  save_build("test/testfig_average", build)
}

if (F){
  categories <- c("Gen_Org", "Gen_Progress", "Gen_Gender", 
                  "Gen_Nationality", "Gen_Studies")
  build <- create_patchwork(
    data = labs,
    question = "Work_Finish",
    style = my_style,
    title = paste(c("A", rep("really", 50), "long title"), collapse = " "),
    categories = my_category_map[categories],
    fonts = my_fonts,
    legend_loc = "bottom"
  )
  save_build("test/testfig", build)
}


## ----- Statistics -----

if (F) {
  for (i in seq_along(twotype_categories)){
    print_statistics(
      data = labs,
      questions = likert_questions,
      category_map = my_category_map,
      category = twotype_categories[i],
      disag_category = "Gen_Org", 
      alpha = 0.05,
      minimum = 5
    )
    if (i != length(twotype_categories)) cat("\n")
  }
}


## ----- Figure generation -----

if (F){
  for (k in 1:nrow(my_bases)){
    base <- my_bases$base[k]
    num <- which(question_codes == base)
    
    build <- build_average(
      data = labs,
      base_data = my_bases,
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
      base_data = my_bases,
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
        base_data = my_bases,
        base = base, 
        answer_map = my_answer_map,
        category_map = my_category_map,
        category = category,
        style = my_style,
        fonts = my_fonts
      )
      save_build(paste0("ordered/fig", num, "_", i), build)
      save_build(paste0("named/", base, sub("^Gen", "", category)), build)
    }
  }
}


## ----- Custom figures -----

if (F){
  custom_questions <- ques$id[is.na(ques$base) & !grepl("^Gen_", ques$id)]
  custom_questions <- setdiff(
    custom_questions, 
    c("Work_Teach", "Work_Teach_Type", "Work_OtherTasks")
  )
  flip_questions <- c("Work_Course_Teach", "Work_Finish", "SS_IB_Exp")
  custom <- labs[c(categories, custom_questions)] |> mutate(
    across(all_of(flip_questions), fct_rev)
  )
  
  for (question in colnames(custom)){
    num <- which(question_codes == question)
    title <- ques$short[equals(ques$id, question)]
    build <- create_patchwork(
      data = custom,
      question = question,
      style = my_style,
      fonts = my_fonts,
      legend_loc = "right",
      title = title
    )
    save_build(paste0("ordered/fig", num), build)
    save_build(paste0("named/", question), build)
  }
}














