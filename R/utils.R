
rm(list = ls())

library(docstring)


## ----- Utility functions -----


equals <- function(a, b) {
  #' @title Compare two vectors for non-missing equality
  #' @description This function performs element-wise comparison of two vectors
  #' and returns TRUE only where both elements are equal and neither value is NA.
  #' Any comparison involving NA returns FALSE.
  #' @param a The first vector.
  #' @param b The second vector.
  #' @return A logical vector indicating where a and b are equal and non-missing.
  a == b & !is.na(a) & !is.na(b)
}


remove_na <- function(x) {
  #' @title Remove missing values from a vector
  #' @description This function removes all NA values from a vector and
  #' returns only the non-missing elements.
  #' @param x A vector that may contain NA values.
  #' @return A vector containing only the non-missing elements of x.
  x[!is.na(x)]
}


is_numeric <- function(x) {
  #' @title Check whether a vector is numeric-coercible
  #' @description This function checks whether all non-missing elements of a
  #' vector can be coerced to numeric without introducing NA values.
  #' It suppresses warnings from as.numeric() and returns TRUE only if
  #' coercion succeeds for every non-NA element.
  #' @param x A vector to test for numeric coercibility.
  #' @return TRUE if all non-missing elements of x can be converted to numeric;
  #' FALSE otherwise.
  all (!is.na (suppressWarnings (as.numeric (remove_na(x)))))
}


common_prefix <- function(str_list) {
  #' @title Find the Longest Common Prefix
  #' @description This function takes a character vector of strings and returns
  #' the longest prefix that is common to all strings. If there is no common 
  #' prefix, it returns an empty string.
  #' @param str_list A character vector of strings to analyze
  #' @return A single string representing the longest common prefix, or an 
  #' empty string ("") if no common prefix exists
  if (length(str_list) == 0) return("")
  chars <- strsplit(str_list, "")
  i <- 1
  while (TRUE) {
    current <- sapply(chars, `[`, i)
    # Stop if characters differ or any string is too short
    if (length(unique(current)) != 1 || any(is.na(current))) break
    i <- i + 1
  }
  if (i == 1) return("")
  return(paste(chars[[1]][seq_len(i - 1)], collapse = ""))
}


coerce_factor <- function(col) {
  #' @title Coerce a column to a factor
  #' @description This function checks if a given column is already a factor.
  #' If it is not, it converts the column to a factor and returns it. If the
  #' column is already a factor, it is returned unchanged.
  #' @param col A vector (e.g., numeric, character, or factor) to be coerced
  #' @return A factor version of the input column. If the input was already
  #' a factor, it is returned unchanged.
  if (!is.factor(col)) return (factor(col))
  return (col)
}


number_of_cols <- function(n) {
  #' @title Determine best number of columns for layout
  #' @description Chooses the optimal number of columns to arrange `n` items 
  #'   in a visually balanced grid. Prefers layouts divisible by 4, then 5, then 3. 
  #'   If `n` is not divisible by any, returns the number (4, 5, or 3) that maximizes 
  #'   the remainder (`n %% k`) to minimize leftover items in the last row.
  #' @param n A single numeric value specifying the total number of items.
  #' @return An integer indicating the recommended number of columns.
  
  stopifnot(length(n) == 1, is_numeric(n))
  
  n <- as.numeric(n)
  if(n < 3) return(n)
  
  candidates <- c(4, 5, 3)
  divisible <- candidates[n %% candidates == 0]
  if(length(divisible) > 0) return(divisible[1])
  
  remainders <- n %% candidates
  best <- candidates[which.max(remainders)]
  return(best)
}


