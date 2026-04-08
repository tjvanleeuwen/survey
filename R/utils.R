
library(tidyverse)

## ----- utility functions -----

equals <- function(a, b) {
  a == b & !is.na(a) & !is.na(b) | is.na(a) & is.na(b)
}


remove_na <- function(x) {
  x[!is.na(x)]
}


is_number <- function(x){
  length(x) == 1 & is.numeric(x)
}


shorten_answer <- function(column, short, test=FALSE){
  long <- unique (remove_na (column))
  if (test) print(long)
  column <- setNames(short, long)[column]
  return(column)
}


escape_regex <- function(x){
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
}



## --- figure building ---
# scale_fill_brewer(
#   palette = style$palette,
#   labels = rep("", n_answers),
#   drop = FALSE,
#   guide = guide_legend (nrow = 1),
#   direction = -1
# )
# scale_fill_brewer(
#   palette = style$palette, 
#   labels = rep("", n_answers),
#   guide = guide_legend(nrow = 1),
#   direction = -1
# )
# scale_fill_brewer(
#   palette = style$palette, 
#   labels = rep("", n_answers), 
#   drop = FALSE, 
#   guide = guide_legend (nrow = 1)
# )


scale_fill <- function(
    labels,
    palette = "Spectral",
    guide = guide_legend(nrow = 1),
    drop = FALSE,
    direction = 1
  ){
  stopifnot(
    is.character(labels),
    is_string(palette),
    direction %in% c(1, -1)
  )
  
  if (length(labels) == 2){
    pal <- RColorBrewer::brewer.pal(11, palette)[c(2, 10)]
    if (direction == -1) pal <- rev(pal)
    scale_fill_manual(
      values = pal,
      labels = labels,
      drop = drop,
      guide = guide
    )
  } else if (length(labels) == 3){
    pal <- RColorBrewer::brewer.pal(11, palette)[c(3, 6, 9)]
    if (direction == -1) pal <- rev(pal)
    scale_fill_manual(
      values = pal,
      labels = labels,
      drop = drop,
      guide = guide
    )
  } else {
    scale_fill_brewer(
      palette = palette,
      labels = labels,
      drop = drop,
      guide = guide,
      direction = direction
    )
  }
}


dimensions <- function(n) {
  stopifnot(is_number(n))
  
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


save_build <- function(filename, build, filetype="pdf"){
  stopifnot(
    is_string(filename),
    c("plot", "width", "height") %in% names(build),
    is_string(filetype)
  )
  
  ggsave(
    filename = paste0("./figures/", filename, ".", filetype),
    plot = build$plot,
    width = build$width,
    height = build$height,
    dpi = 300
  )
}



## --- measuring functions ---

grob_dimensions <- function(grob_width, grob_height){
  x <- grid::convertWidth(grob_width, unitTo = "in", valueOnly = TRUE)
  y <- grid::convertHeight(grob_height, unitTo = "in", valueOnly = TRUE)
  return(list(width = as.numeric(x), height = as.numeric(y)))
}


measure_text <- function(txt, fontsize=12){
  if (is.null(txt)) return(list(width = 0, height = 0))
  stopifnot(is.character(txt))

  widths <- numeric(length(txt))
  heights <- numeric(length(txt))

  for (i in seq_along(txt)) {
    grob <- grid::textGrob(txt[i], gp = grid::gpar(fontsize = fontsize))
    widths[i] <- grid::convertWidth(
      grid::grobWidth(grob), 
      "in", 
      valueOnly = TRUE
    )
    heights[i] <- grid::convertHeight(
      grid::grobHeight(grob), 
      "in", 
      valueOnly = TRUE
    )
  }

  return(list(width = widths, height = heights))
}


find_width <- function(txt, fontsize = 12){
  if (is.null(txt)) return(0)
  stopifnot(is.character(txt))
  measure_text(txt, fontsize = fontsize)$width
}


width_to_spaces <- function(width, fontsize = 12) {
  stopifnot(
    is_number(width),
    is_number(fontsize)
  )
  space_grob <- grid::textGrob(" ", gp = grid::gpar(fontsize = fontsize))
  space_width <- grid::convertWidth(
    grid::grobWidth(space_grob), 
    "in", 
    valueOnly = TRUE
  )
  n_spaces <- ceiling(width / space_width)
  return(n_spaces)
}


splitstring_width <- function(
    string, 
    max_width, 
    fontsize = 12, 
    collapse = "\n"
  ){
  stopifnot(
    is_string(string),
    is_number(max_width),
    is_number(fontsize),
    is_string(collapse)
  )
  
  if (find_width(string, fontsize = fontsize) < max_width) 
    return(string)
  
  words <- strsplit(string, " ")[[1]]
  
  if (max(find_width(words, fontsize = fontsize)) >= max_width) 
    stop("Word(s) too long")
  
  lines <- character(0)
  current_line <- words[1]
  
  for (word in words[-1]) {
    test_line <- paste(current_line, word, sep = " ")
    
    if (find_width(test_line, fontsize = fontsize) <= max_width) {
      current_line <- test_line
    } else {
      lines <- c(lines, current_line)
      current_line <- word
    }
  }
  
  lines <- c(lines, current_line)
  paste(lines, collapse = collapse)
}



## --- unused ---

show_cols <- function(lst){
  len <- max(lengths(lst))
  lst <- lapply(lst, `length<-`, len)
  if (is.null(names(lst))) {
    names(lst) <- paste0("V", seq_along(lst))
  }
  print(as_tibble(lst), n=len)
  return()
}

common_prefix <- function(str_list) {
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
  if (!is.factor(col)) return (factor(col))
  return (col)
}

int_to_string <- function(x, k=3) {
  fmt <- paste0("%0", k, "d")
  sprintf(fmt, x)
}

