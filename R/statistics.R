# rm(list = ls())

library(tidyverse)

source("./R/clean.R")

safe_wilcox <- function(x, group) {
  if (is.factor(x)) x <- as.numeric(as.character(x))
  df_tmp <- data.frame(x = x, group = group) |>
    filter(!is.na(x), !is.na(group))
  
  if(length(unique(df_tmp$group)) < 2) return(NA)
  if(any(table(df_tmp$group) < 2)) return(NA)
  
  test <- wilcox.test(x ~ group, data = df_tmp, exact = FALSE)
  return(test$p.value)
}


get_pvals <- function(data, questions, category, disag_category=NULL, alpha = 0.05){
  csym = sym(category)
  dsym = sym(disag_category)
  
  overall_pvals <- sapply(questions, function(q) {
    safe_wilcox(data[[q]], data[[category]])
  })
  overall_pvals <- data.frame(
    question = questions,
    p_value = overall_pvals,
    adj_p_value = p.adjust(overall_pvals, method = "BH"),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  overall_pvals <- overall_pvals |>
    mutate(
      significant = p_value < alpha,
      adj_significant = adj_p_value < alpha
    )
  
  if (!is.null(disag_category)){
    by_cat_pvals_long <- data |>
      pivot_longer(
        cols = all_of(questions), 
        names_to = "question", 
        values_to = "response"
      ) |>
      group_by(.data[[disag_category]], question) |>
      summarise(
        p_value = safe_wilcox(response, .data[[category]]),
        .groups = "drop"
      ) |>
      mutate(
        adj_p_value = p.adjust(p_value, method = "BH"),
        significant = p_value < alpha,
        adj_significant = adj_p_value < alpha
      )
  } else {
    by_cat_pvals_long <- NULL
  }
  return(list(overall = overall_pvals, bycat = by_cat_pvals_long))
}
















