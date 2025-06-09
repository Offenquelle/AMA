#### Cross-sectional Regression ####
Formula_EQ <- TobinQ ~ EQ + SIZE + INVOP + EXTFIN + CAPEXRATIO + PPERATIO | Country + Industry
Model_EQ <- feols(Formula_EQ, data = DATA_FINAL)


#### Output of Regression Results ####
modelsummary(Model_EQ,
             digits = 3,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_omit = "^(?!R2|Num)",
             output = "Regression Results.html")

source("Statistical Tests.R")
