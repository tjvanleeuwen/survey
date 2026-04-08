library(tidyverse)

## Contains three functions for obtaining and printing statistics based on the
## two-type Wilcox test

## - safe_wilcox
## - get_pvals
## - print_statistics


## --- Functions ---

## run wilcox test if and only if there are exactly two types
## and at least the minimum number of observations of each type in the group
safe_wilcox <- function(x, group, minimum = 5) {
  if (is.factor(x)) x <- as.numeric(as.character(x))
  df_tmp <- data.frame(x = x, group = group) |>
    filter(!is.na(x), !is.na(group))
  
  if(n_distinct(df_tmp$group) != 2) return(NA)
  if(any(table(df_tmp$group) < minimum)) return(NA)
  
  test <- wilcox.test(x ~ group, data = df_tmp, exact = FALSE)
  return(test$p.value)
}


## find pvalues and conclude significance 
## overall and by disaggregation category
get_pvals <- function(
    data, 
    questions, 
    category, 
    disag_category = NULL, 
    alpha = 0.05,
    minimum = 5
  ) {
  stopifnot(
    is.data.frame(data),
    is.character(questions),
    is_string(category),
    is_number(alpha),
    is_number(minimum),
    c(questions, category, disag_category) %in% colnames(data)
  )
  
  overall <- tibble(question = questions) %>%
    mutate(
      p_value = map_dbl(
        question, 
        ~ safe_wilcox(data[[.x]], group = data[[category]], minimum = minimum)),
      adj_p_value = p.adjust(p_value, method = "BH"),
      significant = p_value < alpha,
      adj_significant = adj_p_value < alpha
    )
  
  bycat <- NULL
  
  if (!is.null(disag_category)) {
    bycat <- data %>%
      pivot_longer(
        cols = all_of(questions),
        names_to = "question",
        values_to = "response"
      ) %>%
      group_by(across(all_of(c(disag_category, "question")))) %>%
      summarise(
        p_value = safe_wilcox(response, .data[[category]]),
        .groups = "drop"
      ) %>%
      group_by(across(all_of(disag_category))) %>%   # adjust within subgroup
      mutate(
        adj_p_value = p.adjust(p_value, method = "BH"),
        significant = p_value < alpha,
        adj_significant = adj_p_value < alpha
      ) %>%
      ungroup()
  }
  
  list(
    overall = overall,
    bycat = bycat
  )
}


## print statistics obtained in a legible way
print_statistics <- function(
    data,
    questions,
    category_map,
    category,
    disag_category,
    alpha = 0.05,
    minimum = 5
  ){
  stopifnot(
    is.data.frame(data),
    is.character(questions),
    is_string(category),
    is_number(alpha),
    is_number(minimum),
    c(category, disag_category) %in% names(category_map),
    c(questions, category, disag_category) %in% colnames(data)
  )
  if (n_distinct(data[[category]], na.rm = T) != 2)
    stop("Not a category of two")
  
  pvals <- get_pvals(
    data = data,
    questions = questions,
    category = category,
    disag_category = disag_category,
    alpha = alpha,
    minimum = minimum
  )
  overall <- pvals$overall
  bycat <- pvals$bycat
  
  cat(paste(
    "Statistics on", 
    tolower(category_map[[category]]),
    "disaggregated by",
    tolower(category_map[[disag_category]]),
    "\n"
  ))
  
  cat(paste(
    "Number of questions:",
    length(questions),
    "\n"
  ))
  
  cat(paste(
    "Overall number of significant differences:",
    sum(overall$significant),
    "\n"
  ))
  
  summary_tbl <- bycat %>%
    group_by(.data[[disag_category]]) %>%
    summarise(
      n_significant = sum(significant, na.rm = TRUE),
      n_na = sum(is.na(significant)),
      .groups = "drop"
    )%>%
    rename("Group" := 1)
  summary_df <- as.data.frame(summary_tbl)
  print(summary_df, row.names = FALSE)
}












