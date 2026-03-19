
source("./R/utils.R")


add_nums <- function(fig, yaxis, fontsize = 11){
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
  if (padding == 0) return(string)
  
  n_spaces <- width_to_spaces(padding, fontsize = fontsize)
  
  named_string <- vapply(string, function(s) {
    lines <- strsplit(s, "\n")[[1]]
    
    if (right) {
      lines <- paste0(lines, strrep(" ", n_spaces))
    } else {
      lines <- paste0(strrep(" ", n_spaces), lines)
    }
    
    paste(lines, collapse = "\n")
  }, character(1))
  
  return(unname(named_string))
}



split_and_pad <- function(preamble, yaxis, max_char = 50, fontsize = 11){
  yaxis <- unname(sapply(yaxis, splitstring, max_char = max_char))
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
    yaxis <- unname(sapply(
      yaxis, 
      add_padding,
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



create_dummy_plot <- function(
    n_answers = 7,
    style = list(
      thematics = theme_minimal(),
      palette = "Spectral"
    )
){
  dummy_data <- data.frame(value = c(1:n_answers))
  dummy_data |>
    ggplot() +
    aes(x = factor(value), fill = factor(value)) +
    geom_bar(stat = "count") +
    scale_fill_brewer(
      palette = style$palette,
      labels = rep("", n_answers),
      drop = FALSE,
      guide = guide_legend (nrow = 1)
    ) +
    style$thematics +
    theme(legend.position = "bottom")
}



create_legend <- function(plot, left, right, fontsize=12) {
  legend <- cowplot::get_legend(
    plot + theme(
      legend.position = "bottom",
      legend.box.margin = margin(0, 0, 0, 0),
      legend.margin = margin(0, 0, 0, 0)
    )
  )
  left <- paste0(left, "  ")
  
  left_width <- measure_text(left, fontsize = fontsize)$width
  right_width <- measure_text(right, fontsize = fontsize)$width
  
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
  
  right <- paste0(right, " ")
  
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













