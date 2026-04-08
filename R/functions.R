library(tidyverse)

source("./R/utils.R")



add_nums <- function(fig, yaxis, fontsize = 11){
  ## result of number of entries per y-value saved in `Freq`
  totals <- as.data.frame(table(yaxis))
  
  fig <- fig +
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(5.5, 20, 5.5, 5.5)) +
    geom_text(
      data = totals,
      aes(
        y = yaxis,
        x = Inf,
        label = paste0(" (", Freq, ")")
      ),
      inherit.aes = FALSE,
      hjust = 0.3,
      size = fontsize * 0.3528
    )
  
  return(fig)
}


add_preamble <- function(fig, preamble, fontsize = 11){
  if (is.null(preamble)) return(fig)
  
  fig <- fig + 
    annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = preamble,
      hjust = 1,
      vjust = 0,
      size = fontsize * 0.3528,
      color = "grey20"
    )
  
  return(fig)
}


add_padding <- function(string, padding, fontsize = 11, right = TRUE) {
  stopifnot(
    is.character(string),
    is_number(padding),
    is_number(fontsize),
    is_bool(right)
  )
  
  if (padding == 0) return(string)
  
  n_spaces <- width_to_spaces(padding, fontsize = fontsize)
  
  string <- unname(vapply(string, function(s) {
    lines <- strsplit(s, "\n")[[1]]
    
    if (right) {
      lines <- paste0(lines, strrep(" ", n_spaces + 1))
    } else {
      lines <- paste0(strrep(" ", n_spaces), lines)
    }
    
    paste(lines, collapse = "\n")
  }, character(1)))
  
  return(string)
}


split_and_pad <- function(preamble, yaxis, fontsize = 11, max_width = 3){
  yaxis <- unname(sapply(
    yaxis, 
    splitstring_width, 
    max_width = max_width,
    fontsize = fontsize
  ))
  yaxis_width <- max(find_width(yaxis, fontsize = fontsize))
  
  if (is.null(preamble)){
    return(list(
      preamble = NULL, 
      yaxis = yaxis,
      width = yaxis_width
    ))
  }
  
  preamble <- splitstring_width(
    preamble, 
    max_width = max_width, 
    fontsize = fontsize,
    collapse = "  \n"
  )
  preamble <- paste0(preamble, "  ")
  preamble_width <- find_width(preamble, fontsize = fontsize)
  
  right_padding <- max(yaxis_width - preamble_width, 0)
  left_padding <- max(preamble_width - yaxis_width, 0)
  
  if (right_padding > 0) {
    preamble <- add_padding(
      string = preamble, 
      padding = right_padding, 
      fontsize = fontsize, 
      right = TRUE
    )
    preamble_width <- find_width(preamble, fontsize = fontsize)
  }
  
  if (left_padding > 0){
    yaxis <- unname(sapply(
      yaxis, 
      add_padding,
      padding = left_padding, 
      fontsize = fontsize, 
      right = FALSE
    ))
    yaxis_width <- max(find_width(yaxis, fontsize = fontsize))
  }
  
  max_width <- max(preamble_width, yaxis_width)
  
  return(list(
    preamble = preamble, 
    yaxis = yaxis,
    width = max_width
  ))
}


create_dummy_plot <- function(
    answers,
    palette = "Spectral",
    n_row = 1
){
  dummy_data <- data.frame(value = c(1:length(answers)))
  dummy_data |>
    ggplot() +
    aes(x = factor(value), fill = factor(value)) +
    geom_bar(stat = "count") +
    # scale_fill_brewer(
    #   palette = palette,
    #   labels = answers,
    #   drop = FALSE,
    #   guide = guide_legend(nrow = n_row)
    # )
    scale_fill(
      labels = answers,
      palette = palette,
      direction = 1,
      guide = guide_legend(nrow = n_row)
    )
}


create_full_legend <- function(
    answers,
    palette = "Spectral",
    fontsize = 12
  ){
  n_answers <- length(answers)
  plot <- create_dummy_plot(
    answers = answers,
    palette = palette,
    n_row = n_answers
  ) + theme(
    legend.title = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = fontsize)
  )
  legend <- cowplot::get_legend(plot)
  return(legend)
}


create_inline_legend <- function(
    answers, 
    palette = "Spectral",
    fontsize = 12
  ) {
  stopifnot(length(answers) > 1)
  
  n_answers <- length(answers)
  plot <- create_dummy_plot(
    answers = rep("", n_answers),
    palette = palette,
    n_row = 1
  ) + theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0)
  )
  legend <- cowplot::get_legend(plot)
  
  left <- paste0(" ", answers[1], "  ")
  right <- paste0(answers[n_answers], " ")
  
  left_width <- find_width(left, fontsize = fontsize)
  right_width <- find_width(right, fontsize = fontsize)
  
  right_longer <- right_width - left_width
  
  if (right_longer > 0){
    left <- add_padding(
      string = left,
      padding = right_longer,
      fontsize = fontsize,
      right = FALSE
    )
  } else if (right_longer < 0){
    right <- add_padding(
      string = right,
      padding = -right_longer,
      fontsize = fontsize,
      right = TRUE
    )
  }
  
  left_text  <- grid::textGrob(left,  x = 1, hjust = 1, 
                               gp = grid::gpar(fontsize = fontsize))
  right_text <- grid::textGrob(right, x = 0, hjust = 0, 
                               gp = grid::gpar(fontsize = fontsize))
  
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



place_legend_below <- function(
    build,
    answers,
    palette = "Spectral",
    fontsize = 12
  ){
  stopifnot(
    all(c("plot", "width", "height") %in% names(build)),
    is_string(palette),
    is_number(fontsize)
  )
  
  plot <- build$plot
  width <- build$width
  height <- build$height
  
  legend_grob <- create_inline_legend(
    answers = answers,
    palette = palette,
    fontsize = fontsize
  )
  legend_dims <- grob_dimensions(
    grob_width = sum(legend_grob$widths),
    grob_height = sum(legend_grob$heights)
  )
  
  plot <- cowplot::plot_grid(
    plot,
    legend_grob,
    grid::nullGrob(),
    ncol = 1,
    rel_heights = c(
      height,
      legend_dims$height,
      5.5/72
    )
  )
  width <- max(width, legend_dims$width)
  height <- height + legend_dims$height + 5.5/72
  
  return(list(plot = plot, width = width, height = height))
}


place_legend_beside <- function(
    build,
    answers,
    palette = "Spectral",
    fontsize = 12
  ){
  stopifnot(
    all(c("plot", "width", "height") %in% names(build)),
    is_string(palette),
    is_number(fontsize)
  )
  
  plot <- build$plot
  width <- build$width
  height <- build$height
  
  legend_grob <- create_full_legend(
    answers = answers,
    palette = palette,
    fontsize = fontsize
  )
  legend_dims <- grob_dimensions(
    grob_width = sum(legend_grob$widths),
    grob_height = sum(legend_grob$heights)
  )
  
  plot <- cowplot::plot_grid(
    plot,
    legend_grob,
    grid::nullGrob(),
    nrow = 1,
    rel_widths = c(
      width,
      legend_dims$width,
      5.5/72
    )
  )
  width <- width + legend_dims$width
  height <- max(height, legend_dims$height)
  
  return(list(plot = plot, width = width, height = height))
}


place_legend <- function(
    build,
    answers,
    palette = "Spectral",
    fontsize = 12,
    position = "bottom"
){
  if (position == "bottom")
    return(place_legend_below(
      build = build,
      answers = answers,
      palette = palette,
      fontsize = fontsize
    ))
  if (position == "right")
    return(place_legend_beside(
      build = build,
      answers = answers,
      palette = palette,
      fontsize = fontsize
    ))
  stop("Wrong position")
}




place_title <- function(
    build,
    title,
    fontsize = 14,
    minimum_width = 6
  ){
  stopifnot(
    all(c("plot", "width", "height") %in% names(build)),
    is_string(title),
    is_number(fontsize)
  )
  
  plot <- build$plot
  width <- build$width
  height <- build$height
  
  pt_per_inch <- 72
  width <- max(width, minimum_width)
  
  margin <- 5.5 / pt_per_inch
  cut_title <- splitstring_width(title, max_width = width - 2 * margin, 
                                 fontsize = fontsize)
  
  title_dims <- measure_text(cut_title, fontsize = fontsize)
  title_grob <- grid::textGrob(
    cut_title, 
    gp = grid::gpar(fontsize = fontsize),
    just = "left",
    x = 0
  )
  
  title_grob <- cowplot::plot_grid(
    grid::nullGrob(),
    title_grob,
    grid::nullGrob(),
    nrow = 1,
    rel_widths = c(
      margin, 
      title_dims$width, 
      width - margin - title_dims$width
    )
  )
  
  plot <- cowplot::plot_grid(
    grid::nullGrob(),
    title_grob,
    grid::nullGrob(),
    plot,
    ncol = 1,
    rel_heights = c(
      margin,
      title_dims$height,
      margin,
      height
    )
  )
  height <- 2 * margin + title_dims$height + height
  
  return(list(plot = plot, width = width, height = height))
}












