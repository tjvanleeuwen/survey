rm(list = ls())

library(tidyverse)

source("./R/clean.R")
source("./R/settings.R")

unlink("./figures/*")

full_data <- read_data(
  filepath_labs="./data/labels.csv", 
  filepath_vals="./data/values.csv"
)
data <- clean(full_data, cutoff=20)

labs <- data[[1]]
vals <- data[[2]]
ques <- data[[3]]


base_fig <- function(data, question){
  qsym <- sym(question)
  labs |>
    filter(!is.na(!!qsym)) |>
    ggplot() +
    aes(x = !!qsym, fill = !!qsym) +
    geom_bar(stat = "count") +
    my_thematics + 
    labs(title = "Totals") +
    scale_x_discrete(labels = rep("", 4)) +
    scale_fill_brewer(palette = my_palette) #+
    # guides(fill = guide_legend(reverse = TRUE))
}


disag_fig <- function(data, question, category){
  qsym <- sym(question)
  csym <- sym(category)
  labs |>
    filter(!is.na(!!qsym)) |>
    filter(!is.na(!!csym)) |>
    ggplot() +
    aes(y = fct_rev(!!csym), fill = fct_rev(!!qsym)) +
    geom_bar(stat = "count", position = "fill") +
    my_thematics + 
    labs(title = find_subtitle(category)) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_brewer(palette = my_palette, direction=-1) +
    # guides(fill = guide_legend(reverse = TRUE))
    theme(legend.position = "none")
}


labs$Work_Finish <- fct_rev(labs$Work_Finish)

finish_totals <- base_fig(labs, "Work_Finish")
finish_bygender <- disag_fig(labs, "Work_Finish", "Gen_Gender")
finish_byorg <- disag_fig(labs, "Work_Finish", "Gen_Org")
finish_byprogress <- disag_fig(labs, "Work_Finish", "Gen_Progress")

finish_patch <- patchwork::wrap_plots(
  list(finish_totals, finish_bygender, finish_byorg, finish_byprogress)
) + 
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = find_title(ques, "Work_Finish"),
    theme = theme(plot.title = element_text(size = title_fontsize, hjust = 0.5))
  )
ggsave("./figures/Work_Finish.pdf", finish_patch, width=8, height=5, dpi=300)


labs$SS_IB_Exp <- factor(
  labs$SS_IB_Exp, 
  levels = c("Yes myself & others", "Yes myself", "Yes others", "No")
)

ibexp_totals <- base_fig(labs, "SS_IB_Exp")
ibexp_bygender <- disag_fig(labs, "SS_IB_Exp", "Gen_Gender")
ibexp_byorg <- disag_fig(labs, "SS_IB_Exp", "Gen_Org")
ibexp_byprogress <- disag_fig(labs, "SS_IB_Exp", "Gen_Progress")

ibexp_patch <- patchwork::wrap_plots(
  list(ibexp_totals, ibexp_bygender, ibexp_byorg, ibexp_byprogress)
) + 
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = splitstring(find_title(ques, "SS_IB_Exp"), max_char=100),
    theme = theme(plot.title = element_text(size = title_fontsize, hjust = 0.5))
  )
ggsave("./figures/SS_IB_Exp.pdf", ibexp_patch, width=8, height=5, dpi=300)



labs$Sup_ActiveTeam <- factor(labs$Sup_ActiveTeam, levels = 0:4)
fig <- labs |>
  filter(!is.na(Sup_Team)) |>
  filter(!is.na(Sup_ActiveTeam)) |>
  ggplot() + 
  aes(x = Sup_Team, fill = Sup_ActiveTeam) +
  geom_bar(stat = "count") + 
  my_thematics +
  scale_fill_brewer(palette = my_palette)

# fig <- base_fig(labs, "Sup_Team")

ggsave("./figures/testfig.pdf", fig, width = 4, height = 3, dpi = 300)




















