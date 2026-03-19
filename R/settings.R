library(tidyverse)

my_answer_map <- list(
  "disagree/agree" = c("Strongly disagree", rep("", 5), "Strongly agree"),
  "never/always" = c("Never", rep("", 5), "Always"),
  "ubos" = c("Never", rep("", 5), "Every day"),
  "pss" = c("Never", rep("", 3), "Very often")
)

my_category_map <- list(
  "Gen_Department" = "Department",
  "Gen_Institute" = "Institute",
  "Gen_Org" = "Department / institute",
  "Gen_PhDtype" = "Employer",
  "Gen_Studies" = "Previous studies",
  "Gen_FTE" = "Full-time equivalent",
  "Gen_Progress" = "Progress",
  "Gen_Gender" = "Gender",
  "Gen_Age" = "Age",
  "Gen_Nationality" = "Dutch nationality"
)

bases_map <- list(
  "Work_JobSat" = c("Job satisfaction", "low", "high"),
  "Work_Teach_Say" = c("Say in teaching", "low", "high"),
  "Work_Teach_Distr" = c("Teaching distribution", "bad", "good"),
  "Work_Manageable" = c("Workload manageability", "bad", "good"),
  "Sup_Leadership" = c("Supervisor leadership", "bad", "good"),
  "Sup_Comparison" = c("Supervisor bias", "high", "low", "1, 2, 3, 4, 5, 6"),
  "Sup_Encourage" = c("Supervisor encouragement", "low", "high"),
  "Sup_Safe" = c("Safety supervision", "low", "high"),
  "Sup_Plan" = c("Supervision planning", "bad", "good"),
  "SS_PsychSafetyTeam" = c("Team safety", "bad", "good", "1, 2, 5, 6"),
  "SS_TeamBehaviour" = c("Team behaviour", "bad", "good"),
  "SS_PsychSafetyOrg" = c("Senior management", "bad", "good"),
  "SS_Rights" = c("Awareness of rights", "low", "high"),
  "SS_RightsUse" = c("Use of rights", "low", "high"),
  "SS_Power" = c("Power differences intimidation", "high", "low", "1"),
  "SS_IB_Report" = c("Reporting inappropriate behaviour", "bad", "good"),
  "SS_EDI" = c("EDI", "bad", "good"),
  "SS_DEI_Ndiv" = c("Neurodivergence contentment", "low", "high"),
  "MH_UBOS_U" = c("Burn-out: emotional exhaustion", "high", "low", "1, 2, 3, 4, 5"),
  "MH_UBOS_D" = c("Burn-out: mental distance", "high", "low", "1, 2, 3, 4"),
  "MH_UBOS_C" = c("Burn-out: competence", "low", "high"),
  "MH_PSS" = c("Perceived stress", "high", "low", "1, 2, 3, 6, 9, 10"),
  "MH_Belonging" = c("Belonging", "low", "high")
)

bases <- imap_dfr(bases_map, ~tibble(
  base = .y,
  title = .x[1],
  left = .x[2],
  right = .x[3],
  invert = .x[4]
))

my_thematics <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  legend.title = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)

my_style = list(
  thematics = my_thematics,
  palette = "RdYlGn",
  panel_size = 3,
  spacer = 0.1,
  max_char = 60
)

my_fonts = list(
  title = 12,
  subtitle = 10,
  axis = 8,
  nums = 6
)










