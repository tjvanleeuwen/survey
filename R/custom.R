
library(tidyverse)

source("./R/clean.R")
source("./R/settings.R")
source("./R/mc_byquestion.R")

# unlink("./figures/test/*")

# full_data <- read_data(
#   filepath_labs="./data/labels.csv", 
#   filepath_vals="./data/values.csv"
# )
# data <- clean(full_data, cutoff=20)
# 
# labs <- data[[1]]
# vals <- data[[2]]
# ques <- data[[3]]





base_fig <- function(data, question){
  qsym <- sym(question)
  data |>
    filter(!is.na(!!qsym)) |>
    ggplot() +
    aes(x = !!qsym, fill = !!qsym) +
    geom_bar(stat = "count") +
    my_style$thematics + 
    labs(title = "Totals") +
    scale_x_discrete(labels = rep("", 4)) +
    scale_fill_brewer(palette = my_style$palette) #+
    # guides(fill = guide_legend(reverse = TRUE))
}


disag_fig <- function(data, question, category){
  qsym <- sym(question)
  csym <- sym(category)
  data |>
    filter(!is.na(!!qsym)) |>
    filter(!is.na(!!csym)) |>
    ggplot() +
    aes(y = fct_rev(!!csym), fill = fct_rev(!!qsym)) +
    geom_bar(stat = "count", position = "fill") +
    my_style$thematics + 
    labs(title = my_category_map[[category]]) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_brewer(palette = my_style$palette, direction=-1) +
    # guides(fill = guide_legend(reverse = TRUE))
    theme(legend.position = "none")
}


build_custom <- function(data, question, categories){
  figures <- list()
  figures$total <- base_fig(data, question)
  figures[categories] <- lapply(
    categories, 
    function(x) disag_fig(data, question, x)
  )
  patch <- patchwork::wrap_plots(figures) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = splitstring(find_title(ques, question), max_char = 100),
      theme = theme(plot.title = element_text(size = my_fonts$title))
    )
  return(patch)
}


# labs$Work_Finish <- fct_rev(labs$Work_Finish)
# 
# finish_patch <- build_custom(labs, "Work_Finish", 
#                              c("Gen_Gender", "Gen_Org", "Gen_Progress"))
# 
# ggsave("./figures/test/Work_Finish.pdf", finish_patch, width=8, height=5, dpi=300)
# 
# 
# 
# labs$SS_IB_Exp <- fct_rev(labs$SS_IB_Exp)
# ibexp_patch <- build_custom(labs, "SS_IB_Exp", 
#                             c("Gen_Gender", "Gen_Org", "Gen_Progress"))
# ggsave("./figures/test/SS_IB_Exp.pdf", ibexp_patch, width=8, height=5, dpi=300)
# 


# fig <- labs |>
#   filter(!is.na(Sup_Team)) |>
#   filter(!is.na(Sup_ActiveTeam)) |>
#   ggplot() + 
#   aes(x = Sup_Team, fill = Sup_ActiveTeam) +
#   geom_bar(stat = "count") + 
#   my_style$thematics +
#   scale_fill_brewer(palette = my_style$palette)
# 
# ggsave("./figures/test/Sup_Team.pdf", fig, width = 4, height = 3, dpi = 300)










# build <- create_patchwork_bybase(
#   data = labs,
#   questions = c("MH_PSS_total"),
#   answers = c("high", "", "", "", "low"),
#   title = "Perceived stress",
#   category = "Gen_Org",
#   style = my_style,
#   fonts = my_fonts
# )
# 
# ggsave("./figures/test/PSS.pdf", build$fig, width = build$width, 
#        height = build$height, dpi = 300)

# title <- my_title_map[["MH_PSS"]]
# 
# build <- build_byquestion(
#   data = labs,
#   question_data = ques,
#   question = "MH_PSS_total",
#   maps = my_maps,
#   categories = c("Gen_Gender", "Gen_Org"),
#   title = title,
#   style = my_style,
#   fonts = my_fonts
# )

# print(build$fig)

# ggsave("./figures/test/PSS.pdf", build$fig, width = build$width,
#        height = build$height, dpi = 300)








