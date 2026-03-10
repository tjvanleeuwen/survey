
library(tidyverse)
library(rlang)
library(scales)

source("./R/utils.R")
source("./R/settings.R")


split_and_pad <- function(preamble, yaxis, max_char = 50, fontsize = 11){
  yaxis <- unname(vapply(yaxis, splitstring, character(1), max_char = max_char))
  yaxis_width <- max(sapply(
    yaxis, function(x) measure_text(x, fontsize = fontsize)$width
  ))
  
  if (is.null(preamble)){
    return(list(
      preamble = NULL, 
      yaxis = yaxis,
      width = yaxis_width
    ))
  }
  
  preamble <- splitstring(preamble, max_char = max_char, coll = "  \n")
  preamble <- paste0(preamble, "  ")
  preamble_width <- measure_text(preamble, fontsize = fontsize)$width
  
  right_padding <- max(yaxis_width - preamble_width, 0)
  left_padding <- max(preamble_width - yaxis_width, 0)
  
  if (right_padding > 0) {
    preamble <- add_padding(
      string = preamble, 
      padding = right_padding, 
      fontsize = fontsize, 
      right = TRUE
    )
    preamble_width <- measure_text(preamble, fontsize = fontsize)$width
  }
  
  if (left_padding > 0){
    yaxis <- unname(vapply(
      yaxis, 
      add_padding,
      character(1),
      padding = left_padding, 
      fontsize = fontsize, 
      right = FALSE
    ))
    yaxis_width <- max(sapply(
      yaxis, function(x) measure_text(x, fontsize = fontsize)$width
    ))
  }
  
  max_width <- max(preamble_width, yaxis_width)
  
  return(list(
    preamble = preamble, 
    yaxis = yaxis,
    width = max_width
  ))
}


add_preamble <- function(fig, preamble){
  if (is.null(preamble)) return(fig)
  
  fig <- fig + 
    annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = preamble,
      hjust = 1,
      vjust = 0,
      size = axistext_fontsize * 0.3528,
      color = "grey20"
    ) + 
    coord_cartesian(clip = "off")
  
  return(fig)
}



create_fig_bybase <- function(
    data_long,
    n_answers = 7,
    title = NULL,
    thematics = theme_minimal(),
    palette = "Spectral"
  ){
  fig <- data_long |>
    ggplot() +
    aes(y = column, fill = fct_rev(value)) +
    geom_bar(stat = "count", position = "fill")
  
  fig <- fig +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_brewer(
      palette = palette,
      labels = rep("", n_answers),
      drop = FALSE,
      guide = guide_legend (nrow = 1),
      direction = -1
    ) +
    labs(title = title) +
    thematics + 
    theme(legend.position = "none")
  return(fig)
}




disaggregate_figures <- function(
    data_long,
    n_answers = 7,
    preamble = NULL,
    yaxis = NULL,
    category = NULL,
    thematics = theme_minimal(),
    palette = "Spectral"
  ) {
  
  if (is.null(category)) {
    return(list(Total = create_fig_bybase(
      data_long = data_long,
      n_answers = n_answers,
      title = "Total",
      thematics = thematics,
      palette = palette
    )))
  }

  split_data <- split(data_long, data_long[[category]])
  figures <- lapply(names(split_data), function(name) {
    fig <- create_fig_bybase(
      data_long = split_data[[name]],
      n_answers = n_answers,
      title = name,
      thematics = thematics,
      palette = palette
    )
  })
  names(figures) <- names(split_data)

  return(figures)
}


create_dummy_plot <- function(
    data,
    value,
    n_answers = 7,
    thematics = theme_minimal(),
    palette = "Spectral"
  ){
  
  value_sym <- sym(value)
  data |>
    ggplot() +
    aes(x = !!value_sym, fill = !!value_sym) +
    geom_bar(stat = "count") +
    scale_fill_brewer(
      palette = palette,
      labels = rep("", n_answers),
      drop = FALSE,
      guide = guide_legend (nrow = 1)
    ) +
    thematics +
    theme(legend.position = "bottom")
}



create_patchwork_bybase <- function(
    data,
    questions,
    answers,
    yaxis = NULL,
    preamble = NULL,
    category = NULL,
    title = NULL,
    thematics = theme_minimal(),
    palette = "Spectral",
    panel_size = 3,
    spacer = 0.2
    ){

  if (!all (questions %in% colnames(data)))
    stop("Invalid question names")
  
  n_answers = length(answers)
  
  padded <- split_and_pad(
    preamble = preamble,
    yaxis = yaxis,
    fontsize = axistext_fontsize,
    max_char = max_characters
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

  base_plot <- create_dummy_plot(
    data = data_long,
    value = "value",
    n_answers = n_answers,
    thematics = thematics,
    palette = palette
  )
  
  figures <- disaggregate_figures(
    data_long = data_long,
    n_answers = n_answers,
    preamble = preamble,
    yaxis = yaxis,
    category = category,
    thematics = thematics,
    palette = palette
  )
  
  plot_dims <- dimensions(ifelse(
    is.null(category), 1,
    length(unique(remove_na(data[[category]])))
  ))
  
  title_dims <- measure_text(title, fontsize = title_fontsize)

  legend_grob <- create_inline_legend(base_plot, answers, fontsize = subtitle_fontsize)
  legend_dims <- grob_dimensions(
    grob_width = sum(legend_grob$widths),
    grob_height = sum(legend_grob$heights)
  )
  
  first_column <- 1 + plot_dims$col * (0:(plot_dims$row - 1))
  for (k in seq_along(figures)){
    if (k %in% first_column){
      figures[[k]] <- figures[[k]] + scale_y_discrete(labels = yaxis)
      figures[[k]] <- add_preamble(
        fig = figures[[k]],
        preamble = preamble
      )
    } else {
      figures[[k]] <- figures[[k]] + scale_y_discrete(labels = NULL)
    }
  }
  
  
  patchwork_width <- plot_dims$col * panel_size + yaxis_width
  final_width <- max(patchwork_width, legend_dims$width, title_dims$width)
  
  subtitle_height <- 1/9 + measure_text("Ty", subtitle_fontsize)$height
  n_questions <- length(yaxis)
  panel_height <- subtitle_height + panel_size * max(n_questions, 2) / 5
  patchwork_height <- plot_dims$row * panel_height
  final_height <- title_dims$height + patchwork_height + legend_dims$height

  
  fig <- patchwork::wrap_plots(figures, ncol = plot_dims$col) + 
    patchwork::plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(size = title_fontsize, hjust = 0.5))
    )

  final_plot <- cowplot::plot_grid(
    fig,
    legend_grob,
    grid::nullGrob(),
    ncol = 1,
    rel_heights = c(title_dims$height + patchwork_height, legend_dims$height, spacer)
  )

  return(list(fig = final_plot, width = final_width, height = final_height))
}


build_bybase <- function(
    data,
    question_data,
    base,
    category = "Gen_Gender",
    thematics = my_thematics,
    palette = my_palette
  ) {

  filter <- equals(question_data$base, base)

  questions <- question_data$id[filter]

  types <- question_data$type[filter]
  if (! length(unique(types)) == 1)
    stop("Non-unique types")
  type = unique(types)
  answers <- likert_answer_map[[type]]

  yaxis <- question_data$short[filter]
  preambles <- question_data$preamble[filter]
  if (! length(unique(preambles)) == 1)
    stop("Non-unique preambles")
  preamble = unique(preambles)
  if (is.na(preamble)) { preamble <- NULL }
  
  title = base_title_map[[base]]
  
  if (!is.null(category)){
    cat_title <- tolower(find_subtitle(category))
    if (cat_title == "dutch nationality")
      cat_title <- "Dutch nationality"
    title = paste(title, "by", cat_title)
  }

  return(create_patchwork_bybase(
    data = data,
    questions = questions,
    answers = answers,
    yaxis = yaxis,
    preamble = preamble,
    category = category,
    title = title,
    thematics = thematics,
    palette = palette
  ))
}











