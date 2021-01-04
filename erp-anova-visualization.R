library(ggplot2)

# save(agent_effect, file="data/corrected-agent_effect.RData")
load("data/corrected-agent_effect.RData")

createHeatMap <- function(resultTibble, p_column){
  heatMap <- ggplot(resultTibble, aes(time, electrode, fill= resultTibble[[p_column]])) +
    geom_tile() + 
    scale_fill_gradient(low = "red", high = "green", limits=c(0,0.05), name="Corrected p-value") +
    # Find a better plot name
    labs(title = p_column)
  return(heatMap)
}

createHeatMap(agent_effect, "bonferroniP")
createHeatMap(agent_effect, "FDRp")
