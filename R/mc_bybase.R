
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
      palette = "Spectral",
      panel_size = 3
    ),
    fonts = list(
      title = 12,
      subtitle = 11,
      axis = 10,
      nums = 5
    )
  ){
  stopifnot(
    is.data.frame(data_long),
    c("column", "value") %in% colnames(data_long),
    is_number(n_answers),
    is.null(title) | is_string(title),
    sapply(fonts, is_number)
  )
  
  fig <- data_long |>
    ggplot() +
    aes(y = fct_rev(column), fill = fct_rev(value)) +
    geom_bar(stat = "count", position = "fill")
  
  fig <- fig +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    # scale_fill_brewer(
    #   palette = style$palette,
    #   labels = rep("", n_answers),
    #   drop = FALSE,
    #   guide = guide_legend (nrow = 1),
    #   direction = -1
    # ) +
    scale_fill(
      labels = rep("", n_answers),
      palette = style$palette,
      guide = guide_legend(nrow = 1),
      direction = -1
    ) +
    style$thematics + 
    theme(
      legend.position = "none",
      axis.text = element_text(size = fonts$axis)
    )
  
  if (!is.null(title)){
    fig <- fig + labs(title = title) + theme(
      plot.title = element_text(size = fonts$subtitle, hjust = 0.5)
    )
  }
  
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
  stopifnot(
    is.data.frame(data_long),
    is_number(n_answers),
    is.null(preamble) | is_string(preamble),
    is.null(category) | is_string(category),
    sapply(fonts, is_number)
  )
  
  if (is.null(category)) {
    return(list(Total = create_fig_bybase(
      data_long = data_long,
      n_answers = n_answers,
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
    yaxis,
    preamble = NULL,
    category = NULL,
    title = NULL,
    style = list(
      thematics = theme_minimal(),
      palette = "Spectral",
      panel_size = 3
    ),
    fonts = list(
      title = 16,
      subtitle = 11,
      axis = 10,
      nums = 5
    )
){
  stopifnot(
    questions %in% colnames(data),
    is.character(answers),
    is.character(yaxis),
    sapply(fonts, is_number)
  )
  
  n_answers = length(answers)
  
  padded <- split_and_pad(
    preamble = preamble,
    yaxis = yaxis,
    fontsize = fonts$axis,
    max_width = style$panel_size
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
    category = category,
    style = list(
      thematics = style$thematics,
      palette = style$palette
    ),
    fonts = fonts
  )
  
  grid_dims <- dimensions(length(figures))
  
  for (k in seq_along(figures)){
    if ((k-1) %% grid_dims$col == 0){
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
  
  plot <- patchwork::wrap_plots(figures, ncol = grid_dims$col)
  width <- yaxis_width + style$panel_size * grid_dims$col
  height <- style$panel_size * grid_dims$row
  
  build <- list(plot = plot, width = width, height = height)
  
  build <- place_legend(
    build = build,
    answers = answers,
    palette = style$palette,
    fontsize = fonts$subtitle,
    position = "bottom"
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
    is.data.frame(question_data),
    is.data.frame(base_data),
    is_string(base),
    is.list(answer_map),
    is.list(category_map),
    is.null(category) | is_string(category),
    is_bool(title),
    sapply(fonts, is_number)
  )
  stopifnot(
    c("id", "base", "type", "short", "preamble") %in% colnames(question_data),
    c("base", "title") %in% colnames(base_data)
  )

  filter <- equals(question_data$base, base)
  stopifnot(sum(filter) > 0)
  
  questions <- question_data$id[filter]
  yaxis <- question_data$short[filter]
  preamble <- unique(question_data$preamble[filter])
  type <- unique(question_data$type[filter])
  
  stopifnot(
    length(questions) > 0,
    length(yaxis) > 0,
    length(preamble) == 1,
    length(type) == 1,
    type %in% names(answer_map)
  )
  
  if (is.na(preamble)) { preamble <- NULL }
  answers <- answer_map[[type]]
  
  fig_title <- NULL
  if (title) {
    fig_title <- base_data$title[equals(base_data$base, base)]
    
    if (!is.null(category)){
      stopifnot(category %in% names(category_map))
      cat_title <- tolower(category_map[[category]])
      if (cat_title == "dutch nationality"){ 
        cat_title <- "Dutch nationality" 
      }
      fig_title = paste(fig_title, "by", cat_title)
    }
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











