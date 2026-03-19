
library(tidyverse)
library(rlang)
library(scales)

source("./R/utils.R")
source("./R/functions.R")



create_fig_bybase <- function(
    data_long,
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
  ){
  fig <- data_long |>
    ggplot() +
    aes(y = fct_rev(column), fill = fct_rev(value)) +
    geom_bar(stat = "count", position = "fill")
  
  fig <- fig +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_brewer(
      palette = style$palette,
      labels = rep("", n_answers),
      drop = FALSE,
      guide = guide_legend (nrow = 1),
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
    yaxis = fct_rev(data_long$column), 
    fontsize = fonts$nums
  )
  
  return(fig)
}




disaggregate_figures <- function(
    data_long,
    n_answers = 7,
    preamble = NULL,
    yaxis = NULL,
    category = NULL,
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
  
  if (is.null(category)) {
    return(list(Total = create_fig_bybase(
      data_long = data_long,
      n_answers = n_answers,
      # title = "Total",
      style = style,
      fonts = fonts
    )))
  }

  split_data <- split(data_long, data_long[[category]])
  figures <- lapply(names(split_data), function(name) {
    fig <- create_fig_bybase(
      data_long = split_data[[name]],
      n_answers = n_answers,
      title = name,
      style = style,
      fonts = fonts
    )
  })
  names(figures) <- names(split_data)

  return(figures)
}


create_patchwork_bybase <- function(
    data,
    questions,
    answers,
    yaxis = "",
    preamble = NULL,
    category = NULL,
    title = NULL,
    style = list(
      thematics = theme_minimal(),
      palette = "Spectral",
      panel_size = 3,
      spacer = 0.1,
      max_char = 50
    ),
    fonts = list(
      title = 16,
      subtitle = 11,
      axis = 10,
      nums = 5
    )
    ){

  n_answers = length(answers)
  
  padded <- split_and_pad(
    preamble = preamble,
    yaxis = yaxis,
    fontsize = fonts$axis,
    max_char = style$max_char
  )
  preamble <- padded$preamble
  yaxis <- padded$yaxis
  yaxis_width <- padded$width

  data_long <- data |>
    pivot_longer(
      cols = all_of(questions),
      names_to = "column",
      values_to = "value"
    ) |>
    filter(!is.na(value))
  
  figures <- disaggregate_figures(
    data_long = data_long,
    n_answers = n_answers,
    preamble = preamble,
    yaxis = yaxis,
    category = category,
    style = list(
      thematics = style$thematics,
      palette = style$palette
    ),
    fonts = fonts
  )
  
  
  plot_dims <- dimensions(ifelse(
    is.null(category), 1,
    length(unique(remove_na(data[[category]])))
  ))
  
  first_column <- 1 + plot_dims$col * (0:(plot_dims$row - 1))
  for (k in seq_along(figures)){
    if (k %in% first_column){
      figures[[k]] <- figures[[k]] + scale_y_discrete(labels = rev(yaxis))
      figures[[k]] <- add_preamble(
        fig = figures[[k]],
        preamble = preamble,
        fontsize = fonts$axis
      )
    } else {
      figures[[k]] <- figures[[k]] + scale_y_discrete(labels = NULL)
    }
  }
  
  
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
  
  
  text_width <- 2 * style$spacer + max(title_dims$width, legend_dims$width)
  patchwork_width <- plot_dims$col * style$panel_size
  plot_width <- yaxis_width + patchwork_width
  final_width <- max(plot_width, text_width)
  
  text_height <- title_dims$height + legend_dims$height + style$spacer
  subtitle_height <- 1/9 + measure_text("Ty", fontsize = fonts$subtitle)$height
  n_questions <- length(yaxis)
  panel_height <- subtitle_height + style$panel_size * max(n_questions, 2) / 5
  patchwork_height <- plot_dims$row * panel_height
  final_height <- text_height + patchwork_height

  
  plot <- patchwork::wrap_plots(figures, ncol = plot_dims$col) + 
    patchwork::plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(size = fonts$title))
    )
  
  if (is.null(category)){
    legend_grid <- legend_grob
  } else {
    legend_grid <- cowplot::plot_grid(
      grid::nullGrob(),
      legend_grob,
      nrow = 1,
      rel_widths = c(yaxis_width, patchwork_width)
    )
  }

  final_plot <- cowplot::plot_grid(
    plot,
    legend_grid,
    grid::nullGrob(),
    ncol = 1,
    rel_heights = c(
      title_dims$height + patchwork_height, 
      legend_dims$height, 
      style$spacer
    )
  )

  return(list(fig = final_plot, width = final_width, height = final_height))
}


build_bybase <- function(
    data,
    question_data,
    base_data,
    base,
    answer_map,
    category_map,
    category = NULL,
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
  
  if (!all(c("id", "base", "type", "short", "preamble") 
           %in% colnames(question_data)))
    stop("Incomplete questions dataframe")
  if (!all(c("base", "title") %in% colnames(base_data)))
    stop("Incomplete bases dataframe")

  filter <- equals(question_data$base, base)
  if (sum(filter) == 0)
    stop("Base not found")

  questions <- question_data$id[filter]
  if (length(questions) == 0)
    stop("Questions not found")

  type <- unique(question_data$type[filter])
  if (length(type) == 0)
    stop("Type not found")
  if (length(type) > 1)
    stop("Non-unique types")
  
  if (!type %in% names(answer_map))
    stop("Answers not found")
  answers <- answer_map[[type]]

  yaxis <- question_data$short[filter]
  
  preamble <- unique(question_data$preamble[filter])
  if (length(preamble) == 0)
    stop("Preamble not found")
  if (length(preamble) > 1)
    stop("Non-unique preambles")
  if (is.na(preamble)) { preamble <- NULL }
  
  if (title) {
    fig_title = base_data$title[equals(base_data$base, base)]
    
    if (!is.null(category)){
      if (!category %in% names(category_map))
        stop("Category title not found")
      cat_title <- tolower(category_map[[category]])
      if (cat_title == "dutch nationality"){ cat_title <- "Dutch nationality" }
      fig_title = paste(fig_title, "by", cat_title)
    }
  } else {
    fig_title = NULL
  }

  return(create_patchwork_bybase(
    data = data,
    questions = questions,
    answers = answers,
    yaxis = yaxis,
    preamble = preamble,
    category = category,
    title = fig_title,
    style = style,
    fonts = fonts
  ))
}











