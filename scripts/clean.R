
rm(list = ls())

library(dplyr)
library(here)
library(janitor)
library(jsonlite)


## ----- Utility functions -----

source(here("R", "utils.R"))

## ----- Read data -----

read_data <- function(filepath_labs="../labels.csv", filepath_vals="../values.csv"){
  labs <- read.csv(filepath_labs, header=TRUE)
  vals <- read.csv(filepath_vals, header=TRUE)
  
  ## first 12 columns are unnecessary (respondent specific metadata)
  labs <- labs[, -c(1:12)]
  vals <- vals[, -c(1:12)]
  
  ## first row contains the questions
  ques <- labs[1,]
  write.csv(t(ques), "./data/questions.csv", row.names = TRUE)
  
  ## rereading converts datatypes nicely
  write.csv(labs[-c(1, 2), ], "./data/labs.csv", row.names = FALSE)
  labs <- read.csv("./data/labs.csv", header=TRUE, 
                   na.strings = c("", "I don't know", "NA"))
  
  write.csv(vals[-c(1, 2), ], "./data/vals.csv", row.names = FALSE)
  vals <- read.csv("./data/vals.csv", header=TRUE)
  vals[is.na(labs)] <- NA
  
  ## typo in column name
  labs <- rename(labs, !!!setNames("Gen_Departement", "Gen_Department"))
  vals <- rename(vals, !!!setNames("Gen_Departement", "Gen_Department"))
  ques <- rename(ques, !!!setNames("Gen_Departement", "Gen_Department"))
  
  return(list(labs, vals, ques))
}


## ----- Clean data -----

fix_other_dept <- function(data){
  ## people who gave an 'other department' are from FI
  labs <- data[[1]]
  vals <- data[[2]]
  ques <- data[[3]]
  
  dept_other <- equals (labs$Gen_Department, "Another department, namely")
  inst_fi_val <- unique (vals$Gen_Institute.[
    equals(labs$Gen_Institute., "Freudenthal Institute")
  ])
  dept_fi_val <- unique (vals$Gen_Department[
    equals(labs$Gen_Institute., "Freudenthal Institute")
  ])
  if (length(inst_fi_val) != 1 | length(dept_fi_val) != 1) 
    stop("values not found")
  
  vals <- vals |> 
    mutate(
      Gen_Department = ifelse (dept_other, dept_fi_val, Gen_Department),
      ## Add 4 to account for the ICS categories
      Gen_Institute. = ifelse (dept_other, inst_fi_val, Gen_Institute.) + 4,
      Gen_Institute = coalesce (Gen_Institute., Gen_Institute_ICS)
    ) |>
    relocate(Gen_Institute, .after=Gen_Department) |>
    select(-Gen_Institute., -Gen_Institute_ICS, -Gen_Departement_5_TEXT)
  
  labs <- labs |> 
    mutate(
      Gen_Department = ifelse (dept_other, "Mathematics", Gen_Department),
      Gen_Institute. = ifelse (dept_other, "Freudenthal Institute", 
                               Gen_Institute.),
      Gen_Institute = coalesce (Gen_Institute., Gen_Institute_ICS)
    ) |>
    relocate(Gen_Institute, .after=Gen_Department) |>
    select(-Gen_Institute., -Gen_Institute_ICS, -Gen_Departement_5_TEXT)
  
  colnames(ques)[colnames(ques) == "Gen_Institute."] <- "Gen_Institute"
  ques <- ques[, !colnames(ques) %in%c("Gen_Institute_ICS", 
                                       "Gen_Departement_5_TEXT")]
  
  return(list(labs, vals, ques))
}


remove_test_question <- function(data){
  labs <- data[[1]]
  vals <- data[[2]]
  ques <- data[[3]]
  
  labs[["Sup_Leadership_10"]] <- NULL
  vals[["Sup_Leadership_10"]] <- NULL
  ques[["Sup_Leadership_10"]] <- NULL
  
  return(list(labs, vals, ques))
}


shorten <- function(data, test=FALSE) {
  labs <- data[[1]]
  vals <- data[[2]]
  ques <- data[[3]]
  
  labs <- labs |> mutate(
    Gen_Department = shorten_answer(
      Gen_Department, c("Phys", "ICS", "Chem", "Maths"), test
    ),
    Gen_Institute = shorten_answer(
      Gen_Institute, 
      c("GRASP", "Interaction", "Data", "Software", "ISCC", "IMAU", "Debye", 
        "ITP", "Freudenthal", "Mathematics", "Algorithms"),
      test
    ),
    Gen_PhDtype = shorten_answer(
      Gen_PhDtype, c("UU", "External", "NWO-I", "None"), test
    ),
    Gen_Studies = shorten_answer(
      Gen_Studies, c("Utrecht", "EU", "Other", "Netherlands"), test
    )
  )
  return(list(labs, vals, ques))
}


create_disag_columns <- function(data){
  labs <- data[[1]]
  vals <- data[[2]]
  ques <- data[[3]]
  
  labs <- labs |>
    mutate(
      Gen_FTE = ifelse(Gen_PhDfulltime. == "Full-time", 
                                1, Gen_PhDparttime._1),
      Gen_Gender = ifelse(Gen_Gender == "Male", "Majority", "Minority"),
      Gen_Nationality = ifelse(startsWith(Gen_Nation, "Dutch"), "Yes", "No"),
      Gen_Progress = cut(Gen_PhDyear_1 / Gen_PhDtrack, 
                         breaks=c(0, 0.25, 0.5, 0.75, 10),
                         labels=c("<25%", "25%-50%", "50%-75%", ">75%"))
    ) |>
    relocate(Gen_FTE, .after = Gen_PhDparttime._1) |>
    relocate(Gen_Nationality, .after = Gen_Nation) |>
    relocate(Gen_Progress, .after = Gen_PhDyear_1) |>
    select(-Gen_PhDfulltime., -Gen_PhDparttime._1, -Gen_Nation, 
           -Gen_PhDyear_1, -Gen_PhDtrack)
  
  vals <- vals |>
    mutate(
      Gen_FTE = labs$Gen_FTE,
      Gen_Gender = ifelse(labs$Gen_Gender == "Minority", 1, 2),
      Gen_Nationality = ifelse(labs$Gen_Nationality == "Yes", 1, 2),
      Gen_Progress = as.integer(labs$Gen_Progress)
    ) |>
    relocate(Gen_FTE, .after = Gen_PhDparttime._1) |>
    relocate(Gen_Nationality, .after = Gen_Nation) |>
    relocate(Gen_Progress, .after = Gen_PhDyear_1) |>
    select(-Gen_PhDfulltime., -Gen_PhDparttime._1, -Gen_Nation, 
           -Gen_PhDyear_1, -Gen_PhDtrack)
  
  ques <- ques |>
    mutate(
      Gen_Nation = "Do you have a Dutch nationality?"
    ) |>
    rename(!!!setNames(c("Gen_PhDparttime._1", "Gen_PhDyear_1", "Gen_Nation"), 
                       c("Gen_FTE", "Gen_Progress", "Gen_Nationality"))) |>
    select(-Gen_PhDfulltime., -Gen_PhDtrack)
  
  return(list(labs, vals, ques))
}


remove_and_write <- function(data){
  labs <- data[[1]]
  vals <- data[[2]]
  ques <- data[[3]]
  
  if (any(colnames(labs) != colnames(vals) | colnames(vals) != colnames(ques)))
    stop("Inconsistent columns")
  
  open_questions <- grepl("^End_|_(TEXT|Other|Open)$", colnames(labs))
  general_questions <- grepl("^Gen_", colnames(labs))
  
  open_answers <- labs[, open_questions | general_questions]
  open_answers <- open_answers[
    rowSums (is.na (labs[, open_questions])) < length(open_questions),
  ]
  open_answers <- remove_empty(open_answers, "cols")
  write.csv(open_answers, "./data/open_questions.csv", row.names = FALSE,)
  
  labs <- labs[, !open_questions]
  vals <- vals[, !open_questions]
  ques <- ques[, !open_questions]
  
  if (any(colnames(labs) != colnames(vals) | colnames(vals) != colnames(ques)))
    stop("Error in column removal")
  
  return(list(labs, vals, ques))
}


rename_ubos_df <- function(df, prefix = "MH_UBOS") {
  ubos_cols <- grep("UBOS", colnames(df))
  if(length(ubos_cols) == 0) return(df)
  
  ubos_U <- c(1, 3, 5, 11, 13)
  ubos_D <- c(2, 7, 8, 14)
  ubos_C <- c(4, 6, 9, 10, 12, 15)
  
  ubos_zero <- ubos_cols[1] - 1
  
  colnames(df)[ubos_zero + ubos_U] <- paste(prefix, "U", seq_along(ubos_U), sep = "_")
  colnames(df)[ubos_zero + ubos_D] <- paste(prefix, "D", seq_along(ubos_D), sep = "_")
  colnames(df)[ubos_zero + ubos_C] <- paste(prefix, "C", seq_along(ubos_C), sep = "_")
  
  return(df)
}
rename_ubos <- function(data){
  return(lapply(data, rename_ubos_df))
}



## ----- Clean questions -----

select_upto_qm <- function(ques, names, test=FALSE){
  if (length(names) == 1) {names <- c(names)}
  for (name in names) {
    target_question <- ques$name == name
    question <- ques$question[target_question]
    result <- sub("^([^?]*\\?).*", "\\1", question)
    if (test) print(result)
    ques$question[target_question] <- result
  }
  return(ques)
}

remove_inbrackets <- function(ques){
  gsub("\\s*\\([^)]*\\)", "", ques)
}

set_type_row <- function(df, row, type_row, pattern, value) {
  cols <- grepl(pattern, row, fixed=TRUE)
  df[type_row, cols] <- value
  return(df)
}

sort_questions <- function(data) {
  labs <- data[[1]]
  vals <- data[[2]]
  ques <- data[[3]]
  
  ques <- ques |> 
    mutate(id = "question") |> 
    relocate(id) |>
    add_row(id = "type") |>
    add_row(id = "preamble")
  
  name_patterns <- c("^Gen_", "UBOS", "PSS")
  name_types <- c("general", "ubos", "pss")
  
  ques_patterns <- c("1 (strongly disagree) to 7 (strongly agree)",
                "1 (never) to 7 (always)")
  ques_types <- c("disagree/agree", "never/always")
  
  type_row <- which(ques$id == "type")
  
  for (i in seq_along(name_patterns))
    ques <- set_type_row(ques, colnames(ques), type_row, 
                         name_patterns[i], name_types[i])
  for (i in seq_along(ques_patterns))
    ques <- set_type_row(ques, ques[ques$id == "question", ], type_row, 
                         ques_patterns[i], ques_types[i])
  ques[type_row, is.na(ques[type_row, ])] <- "other"
  
  return(list(labs, vals, ques))
}


## ----- Prime data -----


select_data <- function(data, cutoff=20){
  labs <- data[[1]]
  vals <- data[[2]]
  ques <- data[[3]]
  
  enough_rows <- rowSums(is.na(labs)) < ncol(labs) - cutoff + 1
  labs <- labs[enough_rows, ]
  vals <- vals[enough_rows, ]
  
  return(list(labs, vals, ques))
}


find_levels <- function(df_labs, df_vals, cols){
  labels <- unique (unlist (df_labs[cols]))
  values <- unique (unlist (df_vals[cols]))
  return (remove_na (labels[order(values)]))
}

factorise <- function(data){
  labs <- data[[1]]
  vals <- data[[2]]
  ques <- data[[3]]
  
  if (! "id" %in% colnames(ques)) 
    stop("No id column in questions dataframe")
  
  type_row = which(ques$id == "type")
  
  if (any(is.na(ques[type_row, ])))
    stop("Missing types")
  
  individual_cols <- colnames(ques)[ques[type_row, ] %in% c("general", "other")]
  likert_cols <- colnames(ques)[ques[type_row, ] %in% 
                              c("disagree/agree", "never/always")]
  
  for (col in individual_cols){
    labs[[col]] <- factor(labs[[col]], find_levels(labs, vals, col))
  }
  for (col in likert_cols){
    labs[[col]] <- factor(labs[[col]], 1:7)
  }
  labs[["Sup_ActiveSup"]] <- 
    factor(labs[["Sup_ActiveSup"]], 
           c("Promotor", "Copromotor", "Postdoc", "Other"))
  
  return(list(labs, vals, ques))
}








## ----- Final -----

clean <- function(data){
  ## clean data
  data <- fix_other_dept(data)
  data <- remove_test_question(data)
  data <- shorten(data)
  data <- create_disag_columns(data)
  data <- remove_and_write(data)
  data <- rename_ubos(data)
  ## clean questions
  data <- sort_questions(data)
  ## prime data
  data <- select_data(data)
  data <- factorise(data)
  return(data)
}







