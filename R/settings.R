
title_fontsize <- 12
subtitle_fontsize <- 10
axistext_fontsize <- 8
num_fontsize <- 6
max_characters <- 60

my_thematics <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  legend.title = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text = element_text(size = axistext_fontsize),
  plot.title = element_text(size = subtitle_fontsize, hjust = 0.5)
)

no_y_axis <- theme(axis.text.y  = element_blank())

my_palette <- "RdYlGn"

likert_answer_map <- list(
  "disagree/agree" = c("Strongly disagree", rep("", 5), "Strongly agree"),
  "never/always" = c("Never", rep("", 5), "Always")
)

category_map <- list(
  "Gen_Org" = "Department / Institute",
  "Gen_PhDtype" = "Employer",
  "Gen_Studies" = "Previous studies"
)

base_title_map <- list(
  "Work_JobSat" = "Job satisfaction",
  "Work_Teach_Say" = "Say in teaching",
  "Work_Teach_Distr" = "Teaching distribution",
  "Work_Manageable" = "Workload manageability",
  "Sup_Leadership" = "Supervisor leadership",
  "Sup_Comparison" = "Supervisor comparison",
  "Sup_Encourage" = "Supervisor encouragement",
  "Sup_Safe" = "Safety supervision",
  "Sup_Plan" = "Supervision planning",
  "SS_PsychSafetyTeam" = "Team safety",
  "SS_TeamBehaviour" = "Team behaviour",
  "SS_PsychSafetyOrg" = "Senior management",
  "SS_Rights" = "Awareness of rights",
  "SS_RightsUse" = "Use of rights",
  "SS_Power" = "Power differences",
  "SS_IB_Report" = "Inappropriate behaviour",
  "SS_EDI" = "EDI",
  "SS_DEI_Ndiv" = "Neurodivergence",
  "MH_Belonging" = "Belonging"
)










