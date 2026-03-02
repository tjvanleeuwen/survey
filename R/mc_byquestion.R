
library(tidyverse)
library(rlang)
library(scales)

source(here::here("R", "utils.R"))


## ----- Thematics -----

my_thematics <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  plot.title = element_text(hjust = 0.5)
)

my_palette <- "RdYlGn"
mc_answers <- c("Never", "", "", "", "", "", "Always")


## ----- Functions -----


create_fig_totals <- function(
    data, question, answers, title = NA,
    style = list(thematics = my_thematics, palette = my_palette)
  ) {
  if (!question %in% colnames(data))
    stop("question column not found")

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

    style$thematics +
    scale_x_discrete(drop = FALSE, labels=answers) +
    scale_fill_brewer(
      palette = style$palette, labels = rep("", 7), drop=FALSE,
      guide = guide_legend (nrow = 1)
    ) +
    labs( x="", y="") +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )

  if (!is.na(title)) { 
    fig <- fig + labs(title=title) +
      theme(
        plot.title = element_text(size = 10)
      )
  }
  return(fig)
}


add_nums <- function(fig, data, category){
  csym <- sym(category)
  totals <- data |>
    count(!!csym, name = "n_total") |>
    mutate(
      !!csym := fct_rev(!!csym),
      ypos = as.numeric(!!csym)
    )
  fig <- fig + coord_cartesian(clip = "off") +
    geom_text(
      data = totals,
      aes(
        x = Inf,
        y = ypos,
        label = paste0(" (", n_total, ")")
      ),
      inherit.aes = FALSE,
      vjust = 0.5,
      size = 2
    )
  return(fig)
}


chat_add_nums <- function(fig, data, category) {
  
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
      size = 2
    )
}



create_fig_bycategory <- function(data, question, answers, category,
                                    nums = FALSE, title = NA,
                                    style = list(thematics = my_thematics, 
                                                 palette = my_palette)){
  if (!question %in% colnames(data)) 
    stop("question column not found")
  if (!category %in% colnames(data))
    stop("category column not found")
  
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
    style$thematics + 
    scale_fill_brewer( palette = style$palette, labels = rep("", 7),
                       guide = guide_legend(nrow = 1), direction=-1) +
    labs( x="", y="") +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
  if (!is.na(title)) { 
    fig <- fig + labs(title=title) +
      theme(
        plot.title = element_text(size = 10)
      )
  }
  if(nums){ fig <- chat_add_nums(fig, filtered_data, !!sym(category)) }
  return (fig)
}


create_inline_legend <- function(plot, answers) {
  legend <- cowplot::get_legend(
    plot + theme(
      legend.position = "bottom",
      legend.box.margin = margin(0, 0, 0, 0),
      legend.margin = margin(0, 0, 0, 0)
    )
  )
  
  left <- paste0(answers[1], "  ")
  right <- answers[length(answers)]
  
  left_text  <- grid::textGrob(left,  x = 1, hjust = 1)
  right_text <- grid::textGrob(right, x = 0, hjust = 0)
  
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



create_patchwork <- function(
    data, 
    question, 
    answers, 
    categories = c(), 
    nums = FALSE, 
    title = NA,
    style = list(
      thematics = my_thematics, 
      palette = my_palette
    ),
    panel_size = 3,
    spacer = 0.2
) {
  stopifnot(is.data.frame(data), length(question) == 1, length(answers) > 0)
  
  if (is.null(title) || is.na(title))
    title <- question
  
  figures <- list()
  dims <- dimensions(1 + length(categories))
  
  base_plot <- create_fig_totals(
    data = data, 
    question = question, 
    answers = answers, 
    style = style, 
    title = "Totals"
  )
  figures[[1]] <- base_plot + theme(legend.position = "none")
  
  category_map <- list(
    "Gen_Org" = "Department / Institute",
    "Gen_PhDtype" = "Employer",
    "Gen_Studies" = "Previous studies"
  )
  
  for (k in seq_along(categories)) {
    category <- categories[k]
    subtitle <- ifelse(
      category %in% names(category_map),
      category_map[[category]],
      sub("^Gen_", "", category)
    )
    figures[[1 + k]] <- create_fig_bycategory(
      data = data, 
      question = question, 
      answers = answers,
      category = category, 
      style = style, 
      nums = nums, 
      title = subtitle
    ) + theme(legend.position = "none")
  }
  
  patchwork_plot <- patchwork::wrap_plots(
    figures,
    ncol = dims$col
  ) +
    patchwork::plot_layout(
      widths  = rep(panel_size, dims$col),
      heights = rep(panel_size, dims$row)
    ) +
    patchwork::plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(hjust = 0.5))
    )
  
  patchwork_width <- panel_size * dims$col
  patchwork_height <- panel_size * dims$row
  
  title_grob <- grid::textGrob(title, gp = grid::gpar(fontsize = 14))
  title_dims <- grob_dimensions(title_grob)
  
  legend_grob <- create_inline_legend(base_plot, answers)
  legend_dims <- grob_dimensions(legend_grob)
  
  total_width <- max(patchwork_width, 
                     max(title_dims$width, legend_dims$width) + spacer)
  horizontal_padding <- max((total_width - patchwork_width) / 2, 0)
  
  panels_centered <- cowplot::plot_grid(
    grid::nullGrob(),
    patchwork_plot,
    grid::nullGrob(),
    ncol = 3,
    rel_widths = c(horizontal_padding, patchwork_width, horizontal_padding + spacer)
  )
  
  patchwork_with_title <- patchwork_plot + 
    patchwork::plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(hjust = 0.5))
    )
  
  final_plot <- cowplot::plot_grid(
    panels_centered,
    legend_grob,
    grid::nullGrob(),
    ncol = 1,
    rel_heights = c(patchwork_height + title_dims$height, legend_dims$height, spacer)
  )
  
  total_height <- patchwork_height + title_dims$height + legend_dims$height + spacer
  
  return(list(
    plot = final_plot,
    width = total_width,
    height = total_height
  ))
}


build_figure <- function(labs, ques, question, categories=c(), nums=F){
  if (! question %in% ques$id)
    stop("Invalid question")
  
  answer_map <- list(
    "disagree/agree" = c("Strongly disagree", rep("", 5), "Strongly agree"),
    "never/always" = c("Never", rep("", 5), "Always")
  )
  
  type <- ques$type[ques$id == question]
  if (! type %in% names(answer_map))
    stop("Invalid question type")
  
  answers <- answer_map[[type]]
  
  if (! all(categories %in% ques$id[ques$type == "general"]))
    stop("Invalid categories")
  
  title <- find_title(ques, question)
  return(create_patchwork(labs, question, answers, categories, 
                          nums=nums, title=title))
}



