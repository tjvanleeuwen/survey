
library(tidyverse)

source("./R/clean.R")
source("./R/settings.R")
source("./R/mc_byquestion.R")





base_fig <- function(
    data, 
    question,
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
  qsym <- sym(question)
  data |>
    filter(!is.na(!!qsym)) |>
    ggplot() +
    aes(x = !!qsym, fill = !!qsym) +
    geom_bar(stat = "count") +
    style$thematics + 
    labs(title = "Totals") +
    theme(
      plot.title = element_text(size = fonts$subtitle, hjust = 0.5),
      axis.text = element_text(size = fonts$axis)
    ) +
    scale_x_discrete(labels = rep("", 4)) +
    scale_fill_brewer(palette = style$palette)
}


disag_fig <- function(
    data, 
    question, 
    category,
    category_map,
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
  qsym <- sym(question)
  csym <- sym(category)
  data |>
    filter(!is.na(!!qsym)) |>
    filter(!is.na(!!csym)) |>
    ggplot() +
    aes(y = fct_rev(!!csym), fill = fct_rev(!!qsym)) +
    geom_bar(stat = "count", position = "fill") +
    style$thematics + 
    labs(title = category_map[[category]]) +
    theme(
      plot.title = element_text(size = fonts$subtitle, hjust = 0.5),
      axis.text = element_text(size = fonts$axis)
    ) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_brewer(palette = my_style$palette, direction=-1) +
    theme(legend.position = "none")
}


build_custom <- function(
    data, 
    question, 
    categories,
    category_map,
    style = list(
      thematics = theme_minimal(),
      palette = "Spectral",
      max_char = 50
    ),
    fonts = list(
      title = 12,
      subtitle = 11,
      axis = 10,
      nums = 5
    )
  ){
  figures <- list()
  figures$total <- base_fig(
    data = data, 
    question = question,
    style = list(
      thematics = style$thematics,
      palette = style$palette
    ),
    fonts = fonts
    )
  figures[categories] <- lapply(
    categories, 
    function(x) disag_fig(
      data = data, 
      question = question,
      category = x,
      style = list(
        thematics = style$thematics,
        palette = style$palette
      ),
      fonts = fonts)
  )
  patch <- patchwork::wrap_plots(figures) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = splitstring(
        find_title(ques, question), 
        max_char = 3 * style$max_char
      ),
      theme = theme(plot.title = element_text(size = fonts$title))
    )
  return(patch)
}









