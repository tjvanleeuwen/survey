
library(tidyverse)
library(rlang)
library(scales)

source("./R/utils.R")
source("./R/settings.R")


## ----- create individual plots -----

create_fig_totals <- function(
    data, 
    question, 
    n_answers = 7, 
    title = NULL,
    style = list(
      thematics = theme_minimal(),
      palette = "Spectral"
    ),
    fonts = list(
      title = 12,
      subtitle = 11,
      axis = 10,
      nums = 5
    )
  ) {
  if (!question %in% colnames(data))
    stop("Invalid question name")

  qsym <- sym(question)
  filtered_data <- data |>
    filter(!is.na(!!qsym))
  counted_data <- filtered_data |>
    count(!!qsym, name = "n") |>
    complete(!!qsym, fill = list(n = 0))

  fig <- counted_data |>
    ggplot() +
    aes(x=!!qsym, y=n, fill=!!qsym) +
    geom_col() +

    # scale_fill_brewer(
    #   palette = style$palette, 
    #   labels = rep("", n_answers), 
    #   # drop = FALSE, 
    #   guide = guide_legend (nrow = 1)
    # ) +
    scale_fill(
      labels = rep("", n_answers),
      palette = style$palette,
      guide = guide_legend(nrow = 1),
      direction = 1
    ) +
    scale_x_discrete(labels = NULL) + 
    labs(title = title) +
    style$thematics +
    theme(
      legend.position = "none",
      plot.title = element_text(size = fonts$subtitle, hjust = 0.5),
      axis.text = element_text(size = fonts$axis)
    )
  
  return(fig)
}

create_fig_bycategory <- function(
    data, 
    question, 
    category,
    n_answers = 7,
    title = NULL,
    style = list(
      thematics = theme_minimal(),
      palette = "Spectral"
    ),
    fonts = list(
      title = 12,
      subtitle = 11,
      axis = 10,
      nums = 5
    )
  ) {
  if (!question %in% colnames(data)) 
    stop("Invalid question name")
  if (!category %in% colnames(data))
    stop("Invalid category")
  
  qsym <- sym(question)
  csym <- sym(category)
  filtered_data <- data |>
    filter(!is.na(!!qsym)) |>
    filter(!is.na(!!csym))
  
  counted_data <- filtered_data |>
    count( !!csym, !!qsym ) |>
    complete( !!csym, !!qsym, fill = list(n = 0) ) |>
    group_by(!!csym) |>
    filter(sum(n) > 0) |>
    ungroup()
  
  fig <- counted_data |>
    ggplot() +
    aes( y = fct_rev(!!csym), x=n, fill=fct_rev(!!qsym) ) +
    geom_col(position = "fill") +
    
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    # scale_fill_brewer(
    #   palette = style$palette, 
    #   labels = rep("", n_answers),
    #   guide = guide_legend(nrow = 1),
    #   direction = -1
    # ) +
    scale_fill(
      labels = rep("", n_answers),
      palette = style$palette,
      guide = guide_legend(nrow = 1),
      direction = -1
    ) +
    labs(title = title) +
    style$thematics + 
    theme(
      legend.position = "none",
      plot.title = element_text(size = fonts$subtitle, hjust = 0.5),
      axis.text = element_text(size = fonts$axis)
    )
  
  fig <- add_nums(
    fig = fig,
    yaxis = fct_rev(data[[category]]),
    fontsize = fonts$nums
  )
  
  return (fig)
}


## ----- create patchwork figure -----

list_figures <- function(
    data,
    question,
    categories = list(),
    n_answers = 7,
    style = list(
      thematics = theme_minimal(),
      palette = "Spectral"
    ),
    fonts = list(
      title = 12,
      subtitle = 11,
      axis = 10,
      nums = 5
    )
  ){
  
  figures <- list()
  
  total_title <- NULL
  if (length(categories) > 0){
    total_title <- "Total"
  }
  
  figures$Total <- create_fig_totals(
    data = data,
    question = question,
    n_answers = n_answers,
    title = total_title,
    style = style,
    fonts = fonts
  )
  
  for (name in names(categories)) {
    figures[[name]] <- create_fig_bycategory(
      data = data,
      question = question,
      n_answers = n_answers,
      category = name,
      title = categories[[name]],
      style = style,
      fonts = fonts
    )
  }
  
  return(figures)
}


create_patchwork <- function(
    data, 
    question, 
    answers = NULL, 
    categories = list(), 
    title = NULL,
    legend_loc = "bottom",
    style = list(
      thematics = theme_minimal(),
      palette = "Spectral",
      panel_size = 3
    ),
    fonts = list(
      title = 12,
      subtitle = 11,
      axis = 10,
      nums = 5
    )
) {
  stopifnot(
    is.data.frame(data), 
    is_string(question),
    question %in% colnames(data),
    legend_loc %in% c("bottom", "right")
  )
  
  if (is.null(answers)){
    answers <- levels(data[[question]])
  }
  n_answers <- length(answers)
  
  figures <- list_figures(
    data = data, 
    question = question, 
    n_answers = n_answers, 
    categories = categories, 
    style = list(
      thematics = style$thematics,
      palette = style$palette
    ),
    fonts = fonts
  )
  
  grid_dims <- dimensions(length(figures))
  
  plot <- patchwork::wrap_plots(figures, ncol = grid_dims$col)
  width <- style$panel_size * grid_dims$col
  height <- style$panel_size * grid_dims$row
  
  build <- list(plot = plot, width = width, height = height)
  
  build <- place_legend(
    build = build,
    answers = answers,
    palette = style$palette,
    fontsize = fonts$subtitle,
    position = legend_loc
  )
  
  if (!is.null(title)){
    build <- place_title(
      build = build,
      title = title,
      fontsize = fonts$title,
      minimum_width = 2 * style$panel_size
    )
  }
  
  return(build)
}


build_average <- function(
    data, 
    base_data,
    base,
    category_map,
    categories, 
    title = TRUE,
    style = list(
      thematics = theme_minimal(),
      palette = "Spectral",
      panel_size = 3,
      spacer = 0.1,
      max_char = 50
    ),
    fonts = list(
      title = 12,
      subtitle = 11,
      axis = 10,
      nums = 5
    )
  ) {
  stopifnot(
    is.data.frame(data),
    is.data.frame(base_data),
    is_string(base),
    is.list(category_map),
    is.character(categories),
    is_bool(title),
    sapply(fonts, is_number)
  )
  stopifnot(
    c("base", "title", "left", "right", "invert") %in% colnames(base_data),
    categories %in% names(category_map)
  )
  
  filter <- equals(base_data$base, base)
  stopifnot(sum(filter) == 1)
  
  invert <- base_data$invert[filter]
  
  base_columns <- data[grepl(base, colnames(data))] 
  range <- unlist(lapply(base_columns, levels))
  range <- as.numeric(unique(range))
  
  base_columns <- base_columns |>
    mutate(across(everything(), ~as.numeric(as.character(.x))))
  
  if (!is.na(invert)){
    invert <- str_split(invert, pattern = ", ")[[1]]
    invert_questions <- paste0(base, "_", invert)
    
    base_columns <- base_columns |>
      mutate(across(all_of(invert_questions), ~max(range) + 1 - .x))
  }
  
  breaks <- seq(
    from = min(range), 
    to = max(range), 
    length.out = length(range)-1
  )
  breaks[1] <- breaks[1] - 1
  
  data <- data |> mutate(
    mean = cut(
      rowMeans(base_columns), 
      breaks = breaks,
      labels = c(1:(length(range) - 2))
    )
  )
  
  fig_title <- NULL
  if (title) {
    fig_title <- paste(base_data$title[filter], "average")
  }
  
  left <- base_data$left[filter]
  right <- base_data$right[filter]
  answers <- c(left, rep("", length(breaks)-3), right)
  
  categories <- category_map[categories]
  
  return(create_patchwork(
    data = data,
    question = "mean",
    answers = answers,
    categories = categories,
    title = fig_title,
    style = style,
    fonts = fonts
  ))
}








