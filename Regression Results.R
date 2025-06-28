#### Cross-sectional Regression (Gaio & Raposo) ####
FORMULA_EQ_1 <- TobinQ ~ EQ + SIZE + INVOP + EXTFIN + CAPEX_Ratio + PPE_Ratio + Cash_Ratio + Leverage + SalesM | Country + Industry
MODEL_EQ_1 <- feols(FORMULA_EQ_1, data = DATA_FINAL)

#### Output of Regression Results (Gaio & Raposo) ####
modelsummary(MODEL_EQ_1,
             digits = 3,
             vcov = ~Country + Industry,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_omit = "^(?!R2|Num)",
             output = "Regression Results (Gaio Raposo).html")

#### Cross-sectional Regression (Individual) ####
FORMULA_EQ_2 <- TobinQ ~ AQ_rank + PERS_rank + PRED_rank + RELEV_rank + SMOOTH_rank + TIMEL_rank + CONSER_rank + SIZE + INVOP + EXTFIN + CAPEX_Ratio + PPE_Ratio + Cash_Ratio + Leverage + SalesM | Country + Industry
MODEL_EQ_2 <- feols(FORMULA_EQ_2, data = DATA_FINAL)

#### Output of Regression Results (Individual) ####
modelsummary(MODEL_EQ_2,
             digits = 3,
             vcov = ~Country + Industry,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_omit = "^(?!R2|Num)",
             output = "Regression Results (Individual).html")

source("Statistical Tests.R")
