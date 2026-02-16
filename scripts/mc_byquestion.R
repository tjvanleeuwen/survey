
rm(list = ls())

library(cowplot)
library(dplyr)
library(forcats)
library(ggplot2)
library(jsonlite)
library(rlang)
library(scales)
library(tidyr)

source(here("R", "utils.R"))

## ----- Load data -----

data <- read.csv("./data/cleaned_data.csv")

metadata <- read_json("./data/metadata.json")

question_types <- metadata$question_types
factors <- metadata$factors


assign_factors <- function(data, question_types, factors){
  for (column_name in colnames(data)) {
    matches <- sapply(question_types, function(v) column_name %in% v)
    if (!any(matches)) {
      stop(sprintf("'%s' not found in question_types", column_name))
    }
    
    matched_name <- names(matches)[which(matches)[1]]
    factor_result <- factors[[matched_name]]
    
    levels <- 
    if (!is.null(names(factor_result))){
      unlist(factor_result[[column_name]])
    } else {
      unlist(factor_result)
    }
    if (is_numeric(levels)) { levels <- as.numeric(levels) }
    data[[column_name]] <- factor(data[[column_name]], levels)
  }
  return(data)
}

data <- assign_factors(data, question_types, factors)


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


create_mcfig_totals <- function(
    data, question, answers, title = question,
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

  if (!is.na(title)) { fig <- fig + labs(title=title) }
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


create_mcfig_bycategory <- function(data, question, answers, category,
                                    nums = FALSE, title = question,
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
  
  if (!is.na(title)) { fig <- fig + labs(title=title) }
  if(nums){ fig <- add_nums(fig, filtered_data, csym) }
  return (fig)
}


create_inline_legend <- function(plot) {
  legend <- cowplot::get_legend(
    plot + theme(
      legend.position = "bottom",
      legend.box.margin = margin(0, 0, 0, 0),
      legend.margin = margin(0, 0, 0, 0)
    )
  )
  
  left_text  <- grid::textGrob("Never  ",  x = 1, hjust = 1)
  right_text <- grid::textGrob("Always", x = 0, hjust = 0)
  
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


create_mc_patchwork <- function(
    data, question, answers, 
    disag_categories = c(), nums = FALSE, title = NA,
    style = list(thematics = my_thematics, palette = my_palette)
  ) {
  stopifnot(is.data.frame(data), length(question) == 1, length(answers) > 0)
  
  figures <- list()
  n_cols <- number_of_cols(1+length(disag_categories))
  
  base_plot <- create_mcfig_totals(
    data=data, question=question, answers=answers, title=title, style=style
  )
  figures[[1]] <- base_plot + theme(legend.position = "none")
    
  for (k in seq_along(disag_categories)){
    figures[[1+k]] <- create_mcfig_bycategory(
      data=data, question=question, answers=answers, title=title,
      category=disag_categories[k], style=style, nums=nums
    ) + theme(legend.position = "none")
  }
  patchwork <- wrap_plots(figures, ncol=n_cols, guides="collect") + 
    plot_layout(guides = "collect")
  legend_row <- create_inline_legend(base_plot)
  spacer <- grid::nullGrob()
  final_plot <- cowplot::plot_grid(
    patchwork,
    legend_row,
    spacer,
    ncol = 1,
    rel_heights = c(1, 0.08, 0.03)
  )
  
  return(final_plot)
}


disag_cat <- question_types$general
select <- c(2, 7, 11)
disag_cat <- unlist(disag_cat)[select]

patch <- create_mc_patchwork(
  data = data, question = "SS_PsychSafetyTeam_1", answers = mc_answers,
  disag_categories = disag_cat, nums = TRUE
)

print(patch)
ggsave("./figures/testfig.pdf", patch, width = 3 * 4, 
       height = 3 * 1, dpi = 300)



