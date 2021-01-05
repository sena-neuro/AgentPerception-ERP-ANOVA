library(tidyverse)
library(rstatix)
library(ggpubr)
library(R.matlab)
library(future)
library(furrr)

anova_test <- function(data_tibble) {
  res.aov <-
    anova_test(
      data = data_tibble,
      dv = voltage,
      wid = subject,
      between = experiment_type,
      within = c(presentation_mode, agent)
    )
  anova_df <- res.aov$ANOVA
  return(anova_df)
}
normalityTest <- function(data_tibble) {
  result <-
    data_tibble %>% group_by(agent, presentation_mode, experiment_type) %>%
    shapiro_test(voltage)
  nonNormal <- result[which(result$p < 0.05),]
  return(nonNormal)
}
variance_test <- function(data_tibble, alpha = 0.05) {
  variance_test_results <-
    data_tibble %>% group_by(agent, presentation_mode) %>%
    levene_test(voltage ~ experiment_type)
  nonHomog <-
    variance_test_results[which(variance_test_results$p < alpha),]
  return (nonHomog)
}
outlierTest <- function(data_tibble) {
  outliers <- data_tibble %>%
    group_by(agent, presentation_mode, experiment_type) %>%
    identify_outliers(voltage)
  extreme_outliers <- outliers[which(outliers$is.extreme),]
  return(extreme_outliers)
}

# For Sena's windows 
# setwd("C:\\Users\\lenovo\\Desktop\\r")

prior_data <- readMat("PriorERPs.mat")
naive_data <- readMat("NaiveERPs.mat")
nd <- naive_data$naive.data
pd <- prior_data$prior.data

n_el <- 62 # number of electrodes
n_t <- 400 # number of time points
n_exp <- 2 # number of experiments
n_pr_mode <- 2 # number of presentation modes
n_a <- 3 # number of agents
n_ns <- dim(nd)[4] # naive exp. total subject number
n_ps <- dim(pd)[4] # prior exp. total subject number


# Small data for test purposes
nd_test <- nd[1:n_el, 1:n_t, 1:6, 1:n_ns]
pd_test <- pd[1:n_el, 1:n_t, 1:6, 1:n_ps]


erp_tibble <- tibble(
  subject = factor(),
  electrode = character(),
  time = integer(),
  experiment_type = factor(),
  presentation_mode = factor(),
  agent = factor(),
  voltage = numeric()
)

total_n <- n_el * n_t * n_pr_mode * n_a * n_ns
electrodes <- c('Fp1', 'Fz', 'F3', 'F7', 'FT9', 'FC5' ,'FC1', 'C3', 'T7', 'CP5',
                'CP1', 'Pz', 'P3', 'P7' ,'O1' ,'Oz' ,'O2' ,'P4', 'P8' ,'CP6' ,'CP2' ,
                'Cz' ,'C4', 'T8', 'FT10', 'FC6', 'FC2', 'F4', 'F8', 'Fp2', 'AF7', 'AF3',
                'AFz', 'F1', 'F5', 'FT7', 'FC3', 'FCz', 'C1', 'C5', 'TP7', 'CP3', 'P1',
                'P5', 'PO7', 'PO3', 'POz', 'PO4', 'PO8', 'P6', 'P2', 'CPz', 'CP4', 
                'TP8', 'C6', 'C2', 'FC4', 'FT8', 'F6', 'F2', 'AF4' ,'AF8')

# Just naive
experiment_type_vec <- rep("Naive", each = total_n)

rep1 <- 1
rep2 <- total_n / (rep1 * n_el)
electrode_vec <- rep(electrodes, times = rep2)

rep1 <- rep1 * n_el
rep2 <- total_n / (rep1 * n_t)
time_vec <- rep(1:n_t, each = rep1, times = rep2)

rep1 <- rep1 * n_t
rep2 <- total_n / (rep1 * n_a)
agent_vec <-
  rep(c("Robot", "Android", "Human"),
      each = rep1,
      times = rep2)

rep1 <- rep1 * n_a
rep2 <- total_n / (rep1 * n_pr_mode)
presentation_mode_vec <-
  rep(c("Still", "Video"), each = rep1, times = rep2)


rep1 <- rep1 * n_pr_mode
rep2 <- total_n / (rep1 * n_ns)
subject_vec <- rep(1:n_ns, each = rep1)

# normal data
voltage_vec <- as.vector(nd)

# small data for testing
#voltage_vec <- as.vector(nd_test)

erp_tibble <- erp_tibble %>% add_row(
  subject = factor(subject_vec),
  electrode = electrode_vec,
  time = time_vec,
  experiment_type = factor(experiment_type_vec),
  presentation_mode = factor(presentation_mode_vec),
  agent = factor(agent_vec),
  voltage = voltage_vec
)


# Just prior

total_n <- n_el * n_t * n_pr_mode * n_a * n_ps

# Just naive
experiment_type_vec <- rep("Prior", each = total_n)

rep1 <- 1
rep2 <- total_n / (rep1 * n_el)
electrode_vec <- rep(electrodes, times = rep2)

rep1 <- rep1 * n_el
rep2 <- total_n / (rep1 * n_t)
time_vec <- rep(1:n_t, each = rep1, times = rep2)

rep1 <- rep1 * n_t
rep2 <- total_n / (rep1 * n_a)
agent_vec <-
  rep(c("Robot", "Android", "Human"),
      each = rep1,
      times = rep2)

rep1 <- rep1 * n_a
rep2 <- total_n / (rep1 * n_pr_mode)
presentation_mode_vec <-
  rep(c("Still", "Video"), each = rep1, times = rep2)

rep1 <- rep1 * n_pr_mode
rep2 <- total_n / (rep1 * n_ps)
subject_vec <- rep((n_ns + 1):(n_ns + n_ps), each = rep1)

# normal data
voltage_vec <- as.vector(pd)

# small data for testing
#voltage_vec <- as.vector(pd_test)

erp_tibble <- erp_tibble %>% add_row(
  subject = factor(subject_vec),
  electrode = electrode_vec,
  time = time_vec,
  experiment_type = factor(experiment_type_vec),
  presentation_mode = factor(presentation_mode_vec),
  agent = factor(agent_vec),
  voltage = voltage_vec
)

by_electrode_time  <- erp_tibble %>% group_by(electrode, time)
tbb_nest <- by_electrode_time %>% nest()

no_cores <- availableCores()
plan(multicore, workers = no_cores)

anova_results_tibble <- tbb_nest %>%
  mutate(anova_res = future_map(data, anova_test)) %>%
  unnest(cols = anova_res)
normality_results_tibble <- tbb_nest %>%
  mutate(anova_res = future_map(data, normalityTest)) %>%
  unnest(cols = anova_res)
homogenity_results_tibble <- tbb_nest %>%
  mutate(anova_res = future_map(data, variance_test)) %>%
  unnest(cols = anova_res)
outlier_results_tibble <- tbb_nest %>%
  mutate(anova_res = future_map(data, outlierTest)) %>%
  unnest(cols = anova_res)


# Save an object to a file
# save.image(file = "anova-environment.RData")

# Divide anova Results to different effects
agent_effect <-
  anova_results_tibble[anova_results_tibble$Effect == "agent", ]
experiment_type_effect <-
  anova_results_tibble[anova_results_tibble$Effect == "experiment_type", ]
experiment_type_agent_effect <-
  anova_results_tibble[anova_results_tibble$Effect == "experiment_type:agent", ]
presentation_mode_effect <-
  anova_results_tibble[anova_results_tibble$Effect == "presentation_mode", ]
experiment_typePresentation_mode_effect <-
  anova_results_tibble[anova_results_tibble$Effect == "experiment_type:presentation_mode", ]
presentation_modeAgent_effect <-
  anova_results_tibble[anova_results_tibble$Effect == "presentation_mode:agent", ]
experiment_typePresentation_modeAgent_effect <-
  anova_results_tibble[anova_results_tibble$Effect == "experiment_type:presentation_mode:agent", ]


############### Multiple Corrections ##################
# With FDR and Bonferroni
# Multiple comparisons corrections for each effect type
# TODO functionize this and maybe move to a different script

correctionMethod = "fdr"
n_comparisons = 62 * 400
agent_effect$FDRp <-
  p.adjust(agent_effect$p, method = correctionMethod)
experiment_type_effect$FDRp <-
  p.adjust(experiment_type_effect$p, method = correctionMethod, n = n_comparisons)
experiment_type_agent_effect$FDRp <-
  p.adjust(experiment_typeAgent_effect$p,
           method = correctionMethod,
           n = n_comparisons)
presentation_mode_effect$FDRp <-
  p.adjust(presentation_mode_effect$p, method = correctionMethod, n = n_comparisons)
experiment_typePresentation_mode_effect$FDRp <-
  p.adjust(experiment_typePresentation_mode_effect$p,
           method = correctionMethod,
           n = n_comparisons)
presentation_modeAgent_effect$FDRp <-
  p.adjust(presentation_modeAgent_effect$p,
           method = correctionMethod,
           n = n_comparisons)
experiment_typePresentation_modeAgent_effect$FDRp <-
  p.adjust(experiment_typePresentation_modeAgent_effect$p,
           method = correctionMethod,
           n = n_comparisons)

correctionMethod = "bonferroni"
agent_effect$bonferroniP <-
  p.adjust(agent_effect$p, method = correctionMethod, n = n_comparisons)
experiment_type_effect$bonferronip <-
  p.adjust(experiment_type_effect$p, method = correctionMethod, n = n_comparisons)
experiment_typeAgent_effect$bonferronip <-
  p.adjust(experiment_typeAgent_effect$p,
           method = correctionMethod,
           n = n_comparisons)
presentation_mode_effect$bonferronip <-
  p.adjust(presentation_mode_effect$p, method = correctionMethod, n = n_comparisons)
experiment_typePresentation_mode_effect$bonferronip <-
  p.adjust(experiment_typePresentation_mode_effect$p,
           method = correctionMethod,
           n = n_comparisons)
presentation_modeAgent_effect$bonferronip <-
  p.adjust(presentation_modeAgent_effect$p,
           method = correctionMethod,
           n = n_comparisons)
experiment_typePresentation_modeAgent_effect$bonferronip <-
  p.adjust(experiment_typePresentation_modeAgent_effect$p,
           method = correctionMethod,
           n = n_comparisons)
