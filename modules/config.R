# config.R
config <- list(
  packages = c(
    "tidyverse",
    "haven",
    "psych"
  )
)

colsToKeep <- c(
  "occupation", "topic", "model", "label",
  "techphi_enth", "techphi_dep", "techphi_techrep", "techphi_eou",
  "exp_g_phi", "exp_g_sci", "exp_g_pol", "exp_g_inf", 
  "exp_o_phi", "exp_o_sci", "exp_o_pol", "exp_o_inf", 
  "exp_t_phi", "exp_t_sci", "exp_t_pol", "exp_t_inf",
  "exp_h_phi", "exp_h_sci", "exp_h_pol", "exp_h_inf",
  "violation",
  "emo_angry", "emo_outraged", "emo_disgusted", "emo_anxious", "emo_afraid", "emo_nervous",
  "emo_enthusiastic", "emo_hopeful", "emo_proud", "emo_relaxed", "emo_peaceful", "emo_calm",
  "engage_like", "engage_share", "engage_petition", "engage_attend",
  "accept_phi", "accept_sci", "accept_pol", "accept_inf", 
  "regulate_phi", "regulate_sci", "regulate_pol", "regulate_inf", 
  "familiarity", "experience", "confidence",
  "science_degree", "science_course", "science_course_num"
)

scaleLabels <- c()