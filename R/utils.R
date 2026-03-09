library(docstring)


## ----- Utility functions -----


equals <- function(a, b) {
  #' @title Compare two vectors for non-missing equality
  #' @description This function performs element-wise comparison of two vectors
  #' and returns TRUE only where both elements are equal and neither value is 
  #' NA. Any comparison involving NA returns FALSE.
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


dimensions <- function(n) {
  #' @title Determine grid dimensions from a single number
  #' @description Computes a pair of dimensions (columns and rows) for arranging
  #' items in a grid based on a single numeric input. The function prioritizes
  #' column counts of 4, 5, or 3 (in that order) when evenly divisible. If no
  #' exact division is possible, it selects the column count that maximizes the
  #' remainder and rounds up the number of rows.
  #' @param n A single numeric value indicating the total number of items.
  #' @return A list of length two containing the number of columns and rows.
  stopifnot(length(n) == 1, is_numeric(n))
  
  n <- as.numeric(n)
  if(n < 3) return(list(col = n, row = 1))
  
  candidates <- c(4, 5, 3)
  divisible <- candidates[n %% candidates == 0]
  if(length(divisible) > 0) {
    n_cols <- divisible[1]
    return(list(col = n_cols, row = n / n_cols))
  }
  
  remainders <- n %% candidates
  n_cols <- candidates[which.max(remainders)]
  return(list(col = n_cols, row = ceiling(n / n_cols)))
}


grob_dimensions <- function(grob_width, grob_height){
  x <- grid::convertWidth(grob_width, unitTo = "in", valueOnly = TRUE)
  y <- grid::convertHeight(grob_height, unitTo = "in", valueOnly = TRUE)
  return(list(width = as.numeric(x), height = as.numeric(y)))
}


show_cols <- function(lst){
  #' @title Display list elements as equal-length tibble columns
  #' @description This function takes a list of vectors and ensures that all
  #' elements have the same length by extending shorter vectors with NA values.
  #' If the list elements are unnamed, default column names are generated.
  #' The resulting list is printed as a tibble with one column per list element.
  #' The function is used for display purposes and does not return the tibble.
  #' @param lst A list of vectors to be displayed as columns.
  #' @return NULL. The formatted tibble is printed to the console as a
  #' side effect.
  len <- max(lengths(lst))
  lst <- lapply(lst, `length<-`, len)
  if (is.null(names(lst))) {
    names(lst) <- paste0("V", seq_along(lst))
  }
  print(as_tibble(lst), n=len)
  return()
}


shorten_answer <- function(column, short, test=FALSE){
  #' @title Recode values in a column to shorter labels
  #' @description This function replaces the non-missing values of a column
  #' with corresponding shorter labels. Unique non-missing values are extracted
  #' and matched in order to a provided vector of short labels. Optionally,
  #' the detected unique values can be printed for inspection.
  #' @param column A vector whose values are to be recoded.
  #' @param short A character vector of replacement (short) labels. Its length
  #' must match the number of unique non-missing values in `column`.
  #' @param test Logical; if TRUE, the unique non-missing values of `column`
  #' are printed before recoding. Default is FALSE.
  #' @return A vector of the same length as `column`, where non-missing values
  #' have been replaced by their corresponding short labels.
  long <- unique (remove_na (column))
  if (test) print(long)
  column <- setNames(short, long)[column]
  return(column)
}


escape_regex <- function(x){
  #' @title Escape special characters for regular expressions
  #' @description Escapes regular expression metacharacters in a character
  #' vector so the values can be safely used as literal patterns in regex
  #' functions such as `grep()`, `grepl()`, or `sub()`.
  #' @param x A character vector that may contain regular expression
  #' metacharacters.
  #' @return A character vector of the same length as `x` with all regex
  #' metacharacters escaped by a leading backslash.
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
}


get_patchwork_title_width <- function(pw) {
  pw_grob <- patchwork::patchworkGrob(pw)
  
  title_idx <- which(pw_grob$layout$name == "title")
  if(length(title_idx) == 0) {
    stop("No title found in this patchwork object")
  }
  
  title_grob <- pw_grob$grobs[[title_idx]]
  
  # ensure a graphics device is open
  if (grDevices::dev.cur() == 1) grid::grid.newpage()
  
  title_width <- grid::convertWidth(grid::grobWidth(title_grob), "in", 
                                    valueOnly = TRUE)
  return(title_width)
}


find_title <- function(ques, question){
  if (! question %in% ques$id)
    stop("Invalid question")
  
  row <- ques$id == question
  
  if (is.na (ques$preamble[row]))
    return(ques$short[row])
  
  left <- sub("\\.\\.\\.$", "", ques$preamble[row])
  right <- sub("^\\.\\.\\.\\s", "", ques$short[row])
  return(paste(left, right))
}


add_nums <- function(fig, data, category, fontsize = 8) {
  totals <- data |>
    count({{ category }}, name = "n_total")
  
  fig +
    coord_cartesian(clip = "off") +
    geom_text(
      data = totals,
      aes(
        y = fct_rev({{ category }}),
        x = Inf,
        label = paste0(" (", n_total, ")")
      ),
      inherit.aes = FALSE,
      hjust = -0.1,
      size = fontsize * 0.3528
    )
}


create_inline_legend <- function(plot, answers, fontsize=12) {
  legend <- cowplot::get_legend(
    plot + theme(
      legend.position = "bottom",
      legend.box.margin = margin(0, 0, 0, 0),
      legend.margin = margin(0, 0, 0, 0)
    )
  )
  
  left <- paste0(answers[1], "  ")
  right <- answers[length(answers)]
  
  left_text  <- grid::textGrob(left,  x = 1, hjust = 1, 
                               gp = grid::gpar(fontsize = fontsize))
  right_text <- grid::textGrob(right, x = 0, hjust = 0, 
                               gp = grid::gpar(fontsize = fontsize))
  
  left_width  <- grid::grobWidth(left_text)
  right_width <- grid::grobWidth(right_text)
  legend_width <- grid::grobWidth(legend)
  
  gt <- gtable::gtable(
    widths = grid::unit.c(left_width, legend_width, right_width),
    heights = grid::unit(1, "grobheight", legend)
  )
  
  gt <- gtable::gtable_add_grob(gt, left_text, 1, 1)
  gt <- gtable::gtable_add_grob(gt, legend, 1, 2)
  gt <- gtable::gtable_add_grob(gt, right_text, 1, 3)
  
  return(gt)
}



measure_text <- function(txt, fontsize){
  if (is.null(txt)) return(list(width = 0, height = 0))
  txt_grob <- grid::textGrob(txt, gp = grid::gpar(fontsize = fontsize))
  width <- grid::convertWidth(grid::grobWidth(txt_grob), "in", valueOnly = TRUE)
  height <- grid::convertHeight(grid::grobHeight(txt_grob), "in", valueOnly = TRUE)
  return(list(width = width, height = height))
}


width_to_spaces <- function(width, fontsize) {
  space_grob <- grid::textGrob(" ", gp = grid::gpar(fontsize = fontsize))
  space_width <- grid::convertWidth(grid::grobWidth(space_grob), "in", valueOnly = TRUE)
  n_spaces <- ceiling(width / space_width)
  return(n_spaces)
}


splitstring <- function(string, max_char = 50, coll = "\n"){
  paste(strwrap(string, max_char), collapse = coll)
}

add_padding <- function(string, padding, fontsize = 11, right = TRUE) {
  if (padding == 0) return(string)
  
  n_spaces <- width_to_spaces(padding, fontsize = fontsize)
  
  named_string <- vapply(string, function(s) {
    lines <- strsplit(s, "\n")[[1]]
    
    if (right) {
      lines <- paste0(lines, strrep(" ", n_spaces))
    } else {
      lines <- paste0(strrep(" ", n_spaces), lines)
    }
    
    paste(lines, collapse = "\n")
  }, character(1))
  
  return(unname(named_string))
}





