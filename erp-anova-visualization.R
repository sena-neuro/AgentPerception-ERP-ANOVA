library(ggplot2)
library(tidyverse)
load("data/corrected-anova_results.RData")
load("data/homogenity_results.RData")
load("data/normality_results.RData")
load("data/outlier_results.Rdata")

create_heat_map <- function(result_tibble, p_column, plot_name){
  heat_map <- ggplot(result_tibble, aes(time, electrode, fill= .data[[p_column]])) +
    geom_tile() + 
    scale_fill_gradient(low = "red", high = "green", limits=c(0,0.05), name="Corrected p-value") +
    # Find a better plot name
    labs(title = paste(unique(result_tibble$Effect)[1],' ',plot_name))
         return(heat_map)
}

# Teemporary solution and WRONG, Delete when anova is rerrun. Converts time points (1-400) to ms (-200 - 600)  
anova_results$time <- (anova_results$time-101 ) * 2
homogenity_results$time <-  (homogenity_results$time-101 ) * 2
normality_results$time <-  (normality_results$time-101 ) * 2
outlier_results$time <-  (outlier_results$time-101 ) * 2

anova_plots_bonferroni_p <- anova_results %>% do(plot = create_heat_map(.,"bonferroni_p","Bonferroni Corrected ANOVA"))
anova_plots_fdr <- anova_results %>% do(plot = create_heat_map(.,"fdr_p","FDR Corrected ANOVA"))

normality_plot <- create_heat_map(ungroup(normality_results),"p","Normality Results")
homogenity_plot <- create_heat_map(ungroup(homogenity_results),"p","Homogenity Results")