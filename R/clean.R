
library(tidyverse)
library(janitor)


source("./R/utils.R")

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
      Gen_Org = replace_values(
        Gen_Institute,
        c("Algorithms", "Data", "Software", "Interaction") ~ "ICS",
        c("GRASP", "IMAU", "ITP") ~ "Physics"
      ),
      Gen_FTE = ifelse(Gen_PhDfulltime. == "Full-time", 
                                1, Gen_PhDparttime._1),
      Gen_Gender = ifelse(Gen_Gender == "Male", "Majority", "Minority"),
      Gen_Nationality = ifelse(startsWith(Gen_Nation, "Dutch"), "Yes", "No"),
      Gen_Progress = cut(Gen_PhDyear_1 / Gen_PhDtrack, 
                         breaks=c(0, 0.25, 0.5, 0.75, 10),
                         labels=c("<25%", "25%-50%", "50%-75%", ">75%"))
    ) |>
    relocate(Gen_Org, .after = Gen_Institute) |>
    relocate(Gen_FTE, .after = Gen_PhDparttime._1) |>
    relocate(Gen_Nationality, .after = Gen_Nation) |>
    relocate(Gen_Progress, .after = Gen_PhDyear_1) |>
    select(-Gen_PhDfulltime., -Gen_PhDparttime._1, -Gen_Nation, 
           -Gen_PhDyear_1, -Gen_PhDtrack)
  
  vals <- vals |>
    mutate(
      Gen_Org = replace_values(
        Gen_Institute,
        c(1:4) ~ 1,
        c(7:9) ~ 7
      ),
      Gen_FTE = labs$Gen_FTE,
      Gen_Gender = ifelse(labs$Gen_Gender == "Minority", 1, 2),
      Gen_Nationality = ifelse(labs$Gen_Nationality == "Yes", 1, 2),
      Gen_Progress = as.integer(labs$Gen_Progress)
    ) |>
    relocate(Gen_Org, .after = Gen_Institute) |>
    relocate(Gen_FTE, .after = Gen_PhDparttime._1) |>
    relocate(Gen_Nationality, .after = Gen_Nation) |>
    relocate(Gen_Progress, .after = Gen_PhDyear_1) |>
    select(-Gen_PhDfulltime., -Gen_PhDparttime._1, -Gen_Nation, 
           -Gen_PhDyear_1, -Gen_PhDtrack)
  
  ques <- ques |>
    mutate(
      Gen_Org = "What department / institute do you belong to?",
      Gen_Nation = "Do you have a Dutch nationality?"
    ) |>
    rename(!!!setNames(c("Gen_PhDparttime._1", "Gen_PhDyear_1", "Gen_Nation"), 
                       c("Gen_FTE", "Gen_Progress", "Gen_Nationality"))) |>
    relocate(Gen_Org, .after = Gen_Institute) |>
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


rename_ubos <- function(data){
  prefix = "MH_UBOS"
  for (i in seq_along(data)){
    ubos_cols <- grep("UBOS", colnames(data[[i]]))
    
    ubos_U <- c(1, 3, 5, 11, 13)
    ubos_D <- c(2, 7, 8, 14)
    ubos_C <- c(4, 6, 9, 10, 12, 15)
    
    ubos_zero <- ubos_cols[1] - 1
    
    colnames(data[[i]])[ubos_zero + ubos_U] <- 
      paste(prefix, "U", seq_along(ubos_U), sep = "_")
    colnames(data[[i]])[ubos_zero + ubos_D] <- 
      paste(prefix, "D", seq_along(ubos_D), sep = "_")
    colnames(data[[i]])[ubos_zero + ubos_C] <- 
      paste(prefix, "C", seq_along(ubos_C), sep = "_")
  }
  return(data)
}



## ----- Clean questions -----


reformat_questions <- function(data){
  labs <- data[[1]]
  vals <- data[[2]]
  ques <- data[[3]]
  
  ## transpose dataframe
  ques <- ques |> 
    mutate(id = "question") |> 
    relocate(id)
  rownames(ques) <- NULL
  ques <- ques |>
    column_to_rownames("id") |>
    t() |>
    as.data.frame() |>
    rownames_to_column("id")
  
  ques <- ques |>
    extract(
      id,
      into = c("base", "number"),
      regex = "^(.+)_([0-9]+)$",
      remove = FALSE
    ) |>
    mutate(number = as.integer(number)) |>
    relocate(base, .before = question) |>
    relocate(number, .before = question)
  
  ## assign types
  ques <- ques |> mutate(
    type = ifelse(
      grepl("^Gen_", id), "general",
      ifelse(grepl("UBOS", id), "ubos",
             ifelse(grepl("PSS", id), "pss", NA))),
    type = ifelse(
      grepl("1 (strongly disagree)", question, fixed=TRUE), "disagree/agree",
      ifelse(grepl("1 (never)", question, fixed=TRUE), "never/always",
             ifelse(is.na(type), "other", type))
    )
  ) |>
    relocate(type, .before = question)
  return(list(labs, vals, ques))
}

remove_intro <- function(col){
  markers <- c(
    "(not applicable).", "'Not applicable'.", "'I don't know'. ",
    "(strongly agree). ", "(always). ", "appropriate category. ",
    "a certain way.", "skip this question. "
  )
  for (marker in markers){
    marker_esc <- escape_regex(marker)
    pattern <- paste0("(?s)^.*?", marker_esc, "\\.?\\s*")
    for (i in seq_along(col)){
      col[i] <- sub(pattern, "", col[i], perl = TRUE)
    }
  }
  return(col)
}


clean_questions <- function(data){
  labs <- data[[1]]
  vals <- data[[2]]
  ques <- data[[3]]
  
  ques <- ques |> mutate(
    question = gsub("\u2026", "\\.\\.\\.", question),
    question = gsub("\\s+", " ", question),
    question = trimws(question),
    short = ifelse(
      type %in% c("general", "other"), 
      sub("^([^?]*\\?).*", "\\1", question),
      remove_intro(question)
    ),
    short = gsub("\\s*\\([^)]*\\)", "", short)
  )
  
  pattern <- "\\.\\.\\.\\s-\\s\\.\\.\\."
  
  ques <- ques |> 
    mutate(
      preamble = ifelse(
        grepl(pattern, short), sub("\\s-\\s\\.\\.\\..*", "", short), NA
      ),
      preamble = sub("\\s\\.\\.\\.", "\\.\\.\\.", preamble),
      preamble = sub(" - .*? - ", " ", preamble),
      short = ifelse(
        grepl(pattern, short), sub(".*\\.\\.\\.\\s-\\s", "", short), short
      ),
      short = sub("^\\s*-\\s*", "", short)
    ) |>
    relocate(short, .before=question) |>
    relocate(preamble, .before=question)
  
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
  
  for (col in ques$id[ques$type %in% c("general", "other")]){
    labs[[col]] <- factor(labs[[col]], find_levels(labs, vals, col))
  }
  ## one is off for some reason
  labs[["Sup_ActiveSup"]] <- 
    factor(labs[["Sup_ActiveSup"]], 
           c("Promotor", "Copromotor", "Postdoc", "Other"))
  
  ## some UBOS or PSS questions don't contain all categories
  for (type in c("ubos", "pss")){
    cols <- ques$id[ques$type == type]
    for (col in cols){
      labs[[col]] <- factor(labs[[col]], find_levels(labs, vals, cols))
    }
  }
  
  for (col in ques$id[ques$type %in% c("disagree/agree", "never/always")]){
    labs[[col]] <- factor(labs[[col]], 1:7)
  }
  
  return(list(labs, vals, ques))
}








## ----- Final -----

clean <- function(data, cutoff=20){
  ## clean data
  data <- fix_other_dept(data)
  data <- remove_test_question(data)
  data <- shorten(data)
  data <- create_disag_columns(data)
  data <- remove_and_write(data)
  data <- rename_ubos(data)
  ## clean questions
  data <- reformat_questions(data)
  data <- clean_questions(data)
  ## prime data
  data <- select_data(data, cutoff=cutoff)
  data <- factorise(data)
  return(data)
}







