
library(tidyverse)
library(rlang)
library(scales)

source("./R/utils.R")
source("./R/settings.R")


## ----- create individual plots -----

create_fig_totals <- function(
    data, 
    question, 
    answers, 
    title = NULL,
    style = list(
      thematics = theme_minimal(), 
      palette = "Spectral"
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

    scale_x_discrete(drop = FALSE, labels=answers) +
    scale_fill_brewer(
      palette = style$palette, 
      labels = rep("", length(answers)), 
      drop = FALSE, 
      guide = guide_legend (nrow = 1)
    ) +
    labs(title = title) +
    style$thematics +
    theme(legend.position = "bottom")
  
  return(fig)
}

create_fig_bycategory <- function(
    data, 
    question, 
    answers, 
    category,
    num_font = NULL, 
    title = NULL,
    style = list(
      thematics = theme_minimal(), 
      palette = "Spectral"
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
      labels = rep("", length(answers)),
      guide = guide_legend(nrow = 1),
      direction = -1
    ) +
    labs(title = title) +
    style$thematics + 
    theme(legend.position = "bottom")
  
  if(!is.null(num_font)){ 
    fig <- add_nums(
      fig = fig, 
      data = filtered_data, 
      category = !!sym(category), 
      fontsize = num_font
    ) 
  }
  return (fig)
}


## ----- create patchwork figure -----

list_figures <- function(
    categories = c(), 
    num_font = NULL,
    ...
  ){
  
  figures <- list()
  
  figures[[1]] <- create_fig_totals(
    title = "Totals",
    ...
  )
  
  for (k in seq_along(categories)) {
    category <- categories[k]
    subtitle <- find_subtitle(category)
    figures[[1 + k]] <- create_fig_bycategory(
      category = category, 
      num_font = num_font, 
      title = subtitle,
      ...
    )
  }
  
  return(figures)
}


create_patchwork <- function(
    data, 
    question, 
    answers, 
    categories = c(), 
    num_font = NULL, 
    title = NULL,
    panel_size = 3,
    spacer = 0.1,
    marg_spacer = 0.1,
    style = list(
      thematics = theme_minimal(), 
      palette = "Spectral"
    )
  ) {
  
  stopifnot(is.data.frame(data), length(question) == 1, length(answers) > 0)
  
  if (is.null(title))
    title <- question
  
  figures <- list_figures(
    data = data, 
    question = question, 
    answers = answers, 
    categories = categories, 
    num_font = num_font, 
    style = style
  )
  
  base_plot <- figures[[1]] 
  figures <- map(figures, ~ .x + theme(legend.position = "none"))
  
  dims <- dimensions(1 + length(categories))
  
  top_space <- marg_spacer
  right_space <- 2 * marg_spacer
  bottom_space <- marg_spacer
  left_space <- marg_spacer
  
  patchwork_width <- (panel_size + right_space + left_space) * dims$col
  patchwork_height <- (panel_size + top_space + bottom_space) * dims$row
  
  title_dims <- measure_text(title, fontsize = title_fontsize)
  
  legend_grob <- create_inline_legend(base_plot, answers, fontsize = subtitle_fontsize)
  legend_dims <- grob_dimensions(
    grob_width = sum(legend_grob$widths),
    grob_height = sum(legend_grob$heights)
  )
  
  text_width <- max(title_dims$width, legend_dims$width) + 2 * spacer
  total_width <- max(patchwork_width, text_width)
  total_height <- patchwork_height + title_dims$height + legend_dims$height + spacer
  
  patchwork_plot <- patchwork::wrap_plots(figures, ncol = dims$col) +
    patchwork::plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(size = title_fontsize, hjust = 0.5))
    ) &
    theme(plot.margin = margin(top_space, right_space, 
                               bottom_space, left_space, unit="in"))
  
  horizontal_padding <- (total_width - patchwork_width) / 2
  padded_patchwork <- cowplot::plot_grid(
    grid::nullGrob(),
    patchwork_plot,
    grid::nullGrob(),
    ncol = 3,
    rel_widths = c(horizontal_padding, patchwork_width, horizontal_padding)
  )
  
  final_plot <- cowplot::plot_grid(
    padded_patchwork,
    legend_grob,
    grid::nullGrob(),
    ncol = 1,
    rel_heights = c(title_dims$height + patchwork_height, legend_dims$height, spacer)
  )
  
  return(list(
    plot = final_plot,
    width = total_width,
    height = total_height
  ))
}



build_figure <- function(
    data, 
    question_data, 
    question, 
    categories=c(), 
    num_font = num_fontsize,
    style = list(
      thematics = my_thematics, 
      palette = my_palette
    )
    ){
  if (! question %in% question_data$id)
    stop("Invalid question name")
  
  type <- question_data$type[question_data$id == question]
  if (! type %in% names(likert_answer_map))
    stop("Invalid question type")
  
  answers <- likert_answer_map[[type]]
  
  if (! all(categories %in% question_data$id[question_data$type == "general"]))
    stop("Invalid categories")
  
  title <- find_title(ques, question)
  return(create_patchwork(
    data = data, 
    question = question, 
    answers = answers, 
    categories = categories, 
    num_font = num_font, 
    title = title,
    style = style
  ))
}



