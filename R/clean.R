library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(rlang)
library(janitor)
library(forcats)
library(scales)
library(patchwork)
library(jsonlite)


## ----- Utility functions -----

equals <- function(a, b) 
  a == b & !is.na(a) & !is.na(b)

remove_na <- function(x) 
  x[!is.na(x)]

is_numeric <- function(x) 
  all (!is.na (suppressWarnings (as.numeric (remove_na(x)))))

common_prefix <- function(str_list) {
  chars <- strsplit(str_list, "")
  i <- 1
  repeat {
    current <- sapply(chars, `[`, i)
    if (length(unique(current)) != 1 || any(is.na(current))) break
    i <- i + 1
  }
  return (paste (chars[[1]][seq_len(i - 1)], collapse = ""))
}

find_levels <- function(df_labs, df_vals, cols){
  labels <- unique (unlist (df_labs[cols]))
  values <- unique (unlist (df_vals[cols]))
  return (remove_na (labels[order(values)]))
}

find_questions <- function(colnames, questions, cols){
  result <- questions[colnames %in% cols]
  result <- sub (paste0("^", common_prefix(result)), "", result)
  return(result)
}

list_levels <- function(df_labs, df_vals, names){
  levels <- list()
  for (name in names) {
    levels[[name]] <- 
      if (is.factor (df_labs[[name]])) {
        levels(df_labs[[name]])
      } else if (is_numeric( df_labs[[name]])) {
        sort (unique (df_labs[[name]]))
      } else {
        find_levels(df_labs, df_vals, name)
      }
  }
  return(levels)
}


## ----- Read data -----

labs <- read.csv("../labels.csv", header=TRUE)
vals <- read.csv("../values.csv", header=TRUE)

write.csv(t (labs[, -c(1:12)][1,]), "./data/questions.csv", row.names = TRUE)
questions <- read.csv("./data/questions.csv", header=TRUE)
colnames(questions) <- c("name", "question")

write.csv(labs[-c(1, 2), ][, -c(1:12)], "./data/labs.csv", row.names = FALSE)
labs <- read.csv("./data/labs.csv", header=TRUE, 
                 na.strings = c("", "I don't know", "NA"))
write.csv(vals[-c(1, 2), ][, -c(1:12)], "./data/vals.csv", row.names = FALSE)
vals <- read.csv("./data/vals.csv", header=TRUE)
vals[is.na(labs)] <- NA


## ----- Visual inspection -----

## typo in column name
colnames(labs)[colnames(labs) == "Gen_Departement"] <- "Gen_Department"
colnames(vals)[colnames(vals) == "Gen_Departement"] <- "Gen_Department"
questions$name[questions$name == "Gen_Departement"] <- "Gen_Department"

## people who gave an 'other department' are from FI
## SupLeadership_10 is unnecessary

dept_other <- equals (labs$Gen_Department, "Another department, namely")
inst_fi_val <- unique (vals$Gen_Institute.[
  equals(labs$Gen_Institute., "Freudenthal Institute")
])
dept_fi_val <- unique (vals$Gen_Department[
  equals(labs$Gen_Institute., "Freudenthal Institute")
])
if (length(inst_fi_val) != 1 | length(dept_fi_val) != 1) print("Error!")

vals <- vals |> 
  mutate(
    Gen_Department = ifelse (dept_other, dept_fi_val, Gen_Department),
    ## Add 4 to account for the ICS categories
    Gen_Institute. = ifelse (dept_other, inst_fi_val, Gen_Institute.) + 4,
    Gen_Institute = coalesce (Gen_Institute., Gen_Institute_ICS)
  ) |>
  relocate(Gen_Institute, .after=Gen_Department) |>
  select(-Gen_Institute., -Gen_Institute_ICS, 
         -Gen_Departement_5_TEXT, -Sup_Leadership_10)

labs <- labs |> 
  mutate(
    Gen_Department = ifelse (dept_other, "Mathematics", Gen_Department),
    Gen_Institute. = ifelse (dept_other, "Freudenthal Institute", Gen_Institute.),
    Gen_Institute = coalesce (Gen_Institute., Gen_Institute_ICS)
  ) |>
  relocate(Gen_Institute, .after=Gen_Department) |>
  select(-Gen_Institute., -Gen_Institute_ICS, 
         -Gen_Departement_5_TEXT, -Sup_Leadership_10)

questions$name[questions$name == "Gen_Institute."] <- "Gen_Institute"
questions <- questions [!questions$name %in% c(
  "Gen_Institute_ICS", "Gen_Departement_5_TEXT", "Sup_Leadership_10"
), ]


## ----- Legibility -----

test <- FALSE

## note: requires manual input of shortened names

long <- remove_na( unique(labs$Gen_Department) )
short <- c("Phys", "ICS", "Chem", "Maths")
if(test) print( tibble(long, short) )
labs$Gen_Department <- setNames(short, long)[labs$Gen_Department]

long <- remove_na( unique(labs$Gen_Institute) )
short <- c("GRASP", "Interaction", "Data", "Software", "ISCC", "IMAU",
           "Debye", "ITP", "Freudenthal", "Mathematics", "Algorithms")
if(test) print( tibble(long, short) )
labs$Gen_Institute <- setNames(short, long)[labs$Gen_Institute]

long <- remove_na( unique(labs$Gen_PhDtype) )
short <- c("UU", "External", "NWO-I", "None")
if(test) print( tibble(long, short) )
labs$Gen_PhDtype <- setNames(short, long)[labs$Gen_PhDtype]

long <- remove_na( unique(labs$Gen_Studies) )
short <- c("Utrecht", "EU", "Other", "Netherlands")
if(test) print( tibble(long, short) )
labs$Gen_Studies <- setNames(short, long)[labs$Gen_Studies]


## ----- Disaggregation -----

labs <- labs |>
  mutate(
    Gen_FTE = ifelse(Gen_PhDfulltime. == "Full-time", 1, Gen_PhDparttime._1),
    .before = Gen_PhDfulltime.
  ) |>
  select(-Gen_PhDfulltime., -Gen_PhDparttime._1)

labs <- labs |> 
  mutate(
    Gen_Gender_Dis = ifelse(Gen_Gender == "Male", "Majority", "Minority"),
    Gen_Gender_Dis = factor(Gen_Gender_Dis, levels=c("Minority", "Majority")),
    .after = Gen_Gender
  )

labs <- labs |>
  mutate(
    Gen_Nation_Dis = ifelse(grepl("Dutch,", Gen_Nation, fixed=TRUE),
                            "Dutch", Gen_Nation),
    Gen_Nation_Dis = ifelse(Gen_Nation_Dis == "Dutch",
                            "Dutch", "Non-Dutch"),
    Gen_Nation_Dis = factor(Gen_Nation_Dis, levels=c("Dutch", "Non-Dutch")),
    .after = Gen_Nation
  )

labs <- labs |>
  mutate(
    Gen_Progress = cut(Gen_PhDyear_1 / Gen_PhDtrack, 
                       breaks=c(0, 0.25, 0.5, 0.75, 1, 10),
                       labels=c("<25%", "25%-50%", "50%-75%", "75%-100%", ">100%")),
    Gen_Progress_Dis = cut(Gen_PhDyear_1 / Gen_PhDtrack, 
                           breaks=c(0, 0.25, 0.5, 0.75, 10),
                           labels=c("<25%", "25%-50%", "50%-75%", ">75%")),
    .before = Gen_PhDtrack
  )


## ----- Open questions -----

disaggregation_q <- colnames(labs) %in% c("Gen_Institute", "Gen_Progress", 
                                          "Gen_Gender_Dis", "Gen_Nation_Dis")
open_q_labs <- grepl("_(TEXT|Other|Open)$", colnames(labs)) | 
  grepl("^End_", colnames(labs))
open_q_vals <- grepl("_(TEXT|Other|Open)$", colnames(vals)) | 
  grepl("^End_", colnames(vals))

## select only respondents who answered an open question
open_q <- labs[, disaggregation_q | open_q_labs]
open_q <- open_q[
  rowSums( is.na( labs[, open_q_labs] ) ) < sum(open_q_labs), 
]
open_q <- remove_empty(open_q, "cols")
write.csv(open_q, "./data/open_questions.csv", row.names = FALSE,)


## ----- Data selection -----

## select respondents that have given at least some answers
questions <- questions[ !questions$name %in% colnames(labs)[open_q_labs], ]
labs <- labs[, !open_q_labs]
vals <- vals[, !open_q_vals]
total_respondents <- nrow(labs)
print(paste("Total respondents:", total_respondents))
cutoff <- 15
print( paste("Select respondents with at least", cutoff, "answers") )
enough_rows <- rowSums(is.na(labs)) < ncol(labs) - cutoff + 1
labs <- labs[enough_rows, ]
vals <- vals[enough_rows, ]
print( paste("Remaining respondents:", nrow(labs)) )


## ----- Question types -----

general <- colnames(labs)[grepl("^Gen_", colnames(labs))]
agree_disagree <- questions$name[
  grepl("1 (strongly disagree) to 7 (strongly agree)", questions$question, fixed=TRUE)
]
always_never <- questions$name[
  grepl("1 (never) to 7 (always)", questions$question, fixed=TRUE)
]
ubos <- questions$name [grepl("UBOS", questions$name, fixed=TRUE)]
pss <- questions$name [grepl("PSS", questions$name, fixed=TRUE)]
other <- setdiff (colnames(labs), c(general, agree_disagree, always_never, ubos, pss))

question_types <- list(
  general = general,
  agree_disagree = agree_disagree,
  always_never = always_never,
  ubos = ubos,
  pss = pss,
  other = other
)


## ----- UBOS and PSS -----

ubos_levels <- find_levels(labs, vals, ubos)
pss_levels <- find_levels(labs, vals, pss)

labs[c(ubos, pss)] <- vals[c(ubos, pss)]

ubos_questions <- find_questions(questions$name, questions$question, ubos)
pss_questions <- find_questions(questions$name, questions$question, pss)

ubos_U <- c(1, 3, 5, 11, 13)
ubos_D <- c(2, 7, 8, 14)
ubos_C <- c(4, 6, 9, 10, 12, 15)
ubos_zero <- which(colnames(labs) == ubos[1]) - 1
colnames(labs)[ubos_zero + ubos_U] <- paste("MH_UBOS_U", 1:length(ubos_U), sep="_")
colnames(labs)[ubos_zero + ubos_D] <- paste("MH_UBOS_D", 1:length(ubos_D), sep="_")
colnames(labs)[ubos_zero + ubos_C] <- paste("MH_UBOS_C", 1:length(ubos_C), sep="_")

positive_pss <- c(4, 5, 7, 8)
pss_zero <- which(colnames(labs) == pss[1]) - 1
x <- labs[pss_zero + 4]
labs[pss_zero + positive_pss] <- 4 - labs[pss_zero + positive_pss]


## ----- Factors -----

factors <- list(
  general = list_levels(labs, vals, general),
  agree_disagree = c(1:7),
  always_never = c(1:7),
  ubos = ubos_levels,
  pss = pss_levels,
  other = list_levels(labs, vals, other)
)

metadata <- list(
  question_types = question_types,
  factors = factors
)

write_json(metadata, "./data/metadata.json", pretty=TRUE)
write.csv(labs, "./data/cleaned_data.csv")





