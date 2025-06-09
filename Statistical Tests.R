#### Multicollinearity Test ####
tab_corr(DATA_FINAL[c("TobinQ", "EQ", "SIZE", "INVOP", "EXTFIN", "CAPEXRATIO", "PPERATIO")], 
         triangle = "lower",
         corr.method = c("pearson"),
         digits = 3,
         remove.spaces = TRUE,
         show.p = TRUE,
         p.numeric = FALSE,
         fade.ns = FALSE,
         file = "Correlation Matrix.html")

source("Descriptives and Sample Composition.R")
