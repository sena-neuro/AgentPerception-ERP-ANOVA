library(ggplot2)
library(tidyverse)
load("data/corrected-anova_results.RData")
load("data/homogenity_results.RData")
load("data/normality_results.RData")
load("data/outlier_results.Rdata")

create_heat_map <- function(result_tibble, p_column, plot_name){
  heat_map <- ggplot(result_tibble, aes(time, electrode, fill= result_tibble[[p_column]])) +
    geom_tile() + 
    scale_fill_gradient(low = "red", high = "green", limits=c(0,0.05), name="Corrected p-value") +
    # Find a better plot name
    labs(title = plot_name)
  return(heat_map)
}

anova_plots <- anova_results %>% do(plot = create_heat_map(.,"bonferroni_p","bonferroni_corrected_anova_results"))
normality_plot <- create_heat_map(ungroup(normality_results),"p","Normality Results")
homogenity_plot <- create_heat_map(ungroup(homogenity_results),"p","Homogenity Results")