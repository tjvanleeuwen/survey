
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

    scale_fill_brewer(
      palette = style$palette, 
      labels = rep("", n_answers), 
      drop = FALSE, 
      guide = guide_legend (nrow = 1)
    ) +
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
    scale_fill_brewer(
      palette = style$palette, 
      labels = rep("", n_answers),
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
    categories,
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
  
  figures$Total <- create_fig_totals(
    data = data,
    question = question,
    n_answers = n_answers,
    title = "Total",
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
    answers, 
    categories, 
    title = NULL,
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
  
  stopifnot(is.data.frame(data), length(question) == 1, length(answers) > 0)
  
  n_answers = length(answers)
  
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
  figures$Total <- figures$Total + scale_x_discrete(labels = answers)
  
  dims <- dimensions(length(figures))
  
  title <- splitstring(title, max_char = 3 * style$max_char)
  title_dims <- measure_text(title, fontsize = fonts$title)
  
  dummy_plot <- create_dummy_plot(
    n_answers = n_answers,
    style = list(
      thematics = style$thematics,
      palette = style$palette
    )
  )
  
  legend_grob <- create_legend(
    plot = dummy_plot, 
    left = answers[1],
    right = answers[length(answers)],
    fontsize = fonts$subtitle
  )
  legend_dims <- grob_dimensions(
    grob_width = sum(legend_grob$widths),
    grob_height = sum(legend_grob$heights)
  )
  
  
  patchwork_width <- style$panel_size * dims$col
  patchwork_height <- style$panel_size * dims$row
  
  text_width <- 2 * style$spacer + max(title_dims$width, legend_dims$width)
  text_height <- title_dims$height + legend_dims$height + style$spacer
  
  total_width <- max(patchwork_width, text_width)
  total_height <- patchwork_height + text_height
  
  
  patchwork_plot <- patchwork::wrap_plots(figures, ncol = dims$col) +
    patchwork::plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(size = fonts$title, hjust = 0.5))
    )
  
  horizontal_padding <- max(0, (total_width - patchwork_width) / 2)
  padded_patchwork <- cowplot::plot_grid(
    grid::nullGrob(),
    patchwork_plot,
    grid::nullGrob(),
    ncol = 3,
    rel_widths = c(
      horizontal_padding, 
      patchwork_width, 
      horizontal_padding
    )
  )
  
  final_plot <- cowplot::plot_grid(
    padded_patchwork,
    legend_grob,
    grid::nullGrob(),
    ncol = 1,
    rel_heights = c(
      title_dims$height + patchwork_height, 
      legend_dims$height, 
      style$spacer
    )
  )
  
  return(list(
    fig = final_plot,
    width = total_width,
    height = total_height
  ))
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
  if (! all(c("base", "title", "left", "right", "invert") 
            %in% colnames(base_data))) 
    stop("Incomplete bases dataframe")
  
  if (! base %in% base_data$base)
    stop("Base not found")
  
  filter <- equals(base_data$base, base)
  if (sum(filter) == 0)
    stop("Base not found")
  if (sum(filter) > 1)
    stop("Non-unique base")
  
  invert <- base_data$invert[filter]
  
  base_columns <- data[grepl(base, colnames(data))] 
  range <- as.numeric(unique(unlist(lapply(base_columns, levels))))
  
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
  
  fig_title <- if (title){
    paste(base_data$title[filter], "average")
  } else {
    NULL
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








