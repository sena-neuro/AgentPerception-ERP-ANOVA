# Multiple Correction

load("data/anova-results.RData")

agent_effect <-
  anovaResults[anovaResults$Effect == "agent", ]

n_comparisons = 62 * 400
correctionMethod = "bonferroni"

agent_effect$bonferroniP <-
  p.adjust(agent_effect$p, method = correctionMethod, n = n_comparisons)

correctionMethod = "fdr"
agent_effect$FDRp <-
  p.adjust(agent_effect$p, method = correctionMethod)

save(agent_effect, file="data/corrected-agent_effect.RData")