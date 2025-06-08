#### Load Packages & Data Set ####
#Load Packages
library(readxl)
library(stargazer)
library(tidyr)
library(reshape2)
library(dplyr)
library(collapse)
library(purrr)
library(broom)
library(DescTools)
library(plm)

#Import Data
DATA_RAW <- read_excel("Sample_V2.xlsx")

#Transform to Long Format
DATA_LONG = reshape(data = DATA_RAW,
                    idvar = "Identifier (RIC)",
                    varying = 6:165,
                    sep = "-",
                    timevar = "Year",
                    times = c(2023,2022,2021,2020,2019,2018,2017,2016,2015,2014),
                    new.row.names = 1:1000000,
                    direction = "long")

#Renaming and Transformation to Panel Frame
colnames(DATA_LONG)[1] <- "Identifier"
DATA_PANEL <- pdata.frame(DATA_LONG, index = c("Identifier", "Year"))

#### Variable Construction ####
attach(DATA_PANEL)

#Tobin's Q
DATA_PANEL$MVE <- SharesOutstanding * Price
DATA_PANEL$TobinQ <- (TotalAssets + DATA_PANEL$MVE - BookEquity) / TotalAssets

#1. Accruals Quality
DATA_PANEL$WCA <- DeltaCurrentAssets - DeltaCurrentLiabilities - DeltaCash + DeltaShortTermDebt
DATA_PANEL$CFO <- NIBEX - (DeltaCurrentAssets - DeltaCurrentLiabilities - DeltaCash + DeltaShortTermDebt - Depreciation)
DATA_PANEL$CFO_LAG <- L(DATA_PANEL$CFO, n = 1, shift = "time")
DATA_PANEL$CFO_LEAD <- L(DATA_PANEL$CFO, n = -1, shift = "time")

#2. Earnings Persistence
DATA_PANEL$E <- NIBEX / SharesOutstanding
DATA_PANEL$E_LAG <- L(DATA_PANEL$E, n = 1, shift = "time")
DATA_PANEL[is.na(DATA_PANEL) | DATA_PANEL == "-Inf" | DATA_PANEL == "Inf"] = NA

#5. Value Relevance
DATA_PANEL$EARN <- NIBEX/(SharesOutstanding * Price)
DATA_PANEL$DELTA_EARN <- NIBEX/(SharesOutstanding * Price)

#6. Earnings Timeliness
DATA_PANEL$NEG <- ifelse(X15monthReturn < 0, 1, 0)

#### Clean the Panel Data ####
DATA_CLEAN <- DATA_PANEL[complete.cases(DATA_PANEL), ]

#### Regressions ####

#1. Accruals Quality
Formula_AQ <- WCA ~ CFO/TotalAssets + CFO_LAG/TotalAssets + CFO_LEAD/TotalAssets
RESULTS_AQ <- DATA_CLEAN %>%
  group_by(Identifier) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(Formula_AQ, data = .x)),
         Model_AQ = map(model, tidy),
         AQ = map_dbl(model, ~ sd(residuals(.x))))

#2. Earnings Persistence
Formula_PERS <- E ~ E_LAG
DATA_CLEAN[is.na(DATA_CLEAN) | DATA_CLEAN == "-Inf" | DATA_CLEAN == "Inf"] = NA
RESULTS_PERS <- DATA_CLEAN %>%
  group_by(Identifier) %>%
  group_split() %>%
  map_df(function(DATA_CLEAN) {
    model <- lm(Formula_PERS, data = DATA_CLEAN)
    tidy_model <- broom::tidy(model)
    x1_coef <- tidy_model %>% filter(term == "E_LAG")
    tibble(
      Identifier = unique(DATA_CLEAN$Identifier),
      E_LAG_estimate = x1_coef$estimate,
      sd_res = sd(resid(model))
    )
  })
RESULTS_PERS$PERS <- RESULTS_PERS$E_LAG_estimate *-1

#3. Earnings Predictability
RESULTS_PERS$PRED <- sqrt(RESULTS_PERS$sd_res)

#4. Earnings Smoothness
RESULTS_SMOOTH <- DATA_CLEAN %>%
  group_by(Identifier) %>%
  summarise(
    SMOOTH = sd(NIBEX/TotalAssets)/sd(CFO/TotalAssets)
  )

#5. Value Relevance
Formula_RELEV <- X15monthReturn ~ EARN + DELTA_EARN
RESULTS_RELEV <- DATA_CLEAN %>%
  group_by(Identifier) %>%
  do(
    glance(lm(Formula_RELEV, data = .))
  ) %>%
  select(Identifier, r.squared) %>%
  ungroup()
RESULTS_RELEV$RELEV <- RESULTS_RELEV$r.squared*-1

#6. Earnings Timeliness
Formula_TIMEL <- EARN ~ NEG + X15monthReturn + NEG*X15monthReturn
RESULTS_TIMEL <- DATA_CLEAN %>%
  group_by(Identifier) %>%
  do({
    model <- lm(Formula_TIMEL, data = .)
    tidy_model <- tidy(model)
    glance_model <- glance(model)
    tibble(
      r_squared = glance_model$r.squared,
      coef_X15 = tidy_model$estimate[tidy_model$term == "X15monthReturn"],
      coef_interaction = tidy_model$estimate[tidy_model$term == "NEG:X15monthReturn"]
    )
  }) %>%
  ungroup()
RESULTS_TIMEL$TIMEL <- RESULTS_TIMEL$r_squared*-1

#7. Earnings Conservatism
RESULTS_TIMEL$CONSER <- -(RESULTS_TIMEL$coef_X15+RESULTS_TIMEL$coef_interaction)/RESULTS_TIMEL$coef_X15

#### Aggregate Results ####
DATA_CROSS <- RESULTS_AQ %>%
  left_join(RESULTS_PERS, by = "Identifier") %>%
  left_join(RESULTS_RELEV, by = "Identifier") %>%
  left_join(RESULTS_SMOOTH, by = "Identifier") %>%
  left_join(RESULTS_TIMEL, by = "Identifier")
DATA_CROSS <- DATA_CROSS %>%
  select(Identifier, AQ, PERS, PRED, RELEV, SMOOTH, TIMEL, CONSER)
DATA_CROSS <- DATA_CROSS %>%
  na.omit()
DATA_CROSS$AQ <- Winsorize(DATA_CROSS$AQ, 
                           val = quantile(DATA_CROSS$AQ,
                                          probs = c(0.01, 0.99)))
DATA_CROSS$PERS <- Winsorize(DATA_CROSS$PERS, 
                           val = quantile(DATA_CROSS$PERS,
                                          probs = c(0.01, 0.99)))
DATA_CROSS$PRED <- Winsorize(DATA_CROSS$PRED, 
                           val = quantile(DATA_CROSS$PRED,
                                          probs = c(0.01, 0.99)))
DATA_CROSS$RELEV <- Winsorize(DATA_CROSS$RELEV, 
                           val = quantile(DATA_CROSS$RELEV,
                                          probs = c(0.01, 0.99)))
DATA_CROSS$SMOOTH <- Winsorize(DATA_CROSS$SMOOTH, 
                           val = quantile(DATA_CROSS$SMOOTH,
                                          probs = c(0.01, 0.99)))
DATA_CROSS$TIMEL <- Winsorize(DATA_CROSS$TIMEL, 
                           val = quantile(DATA_CROSS$TIMEL,
                                          probs = c(0.01, 0.99)))
DATA_CROSS$CONSER <- Winsorize(DATA_CROSS$CONSER, 
                           val = quantile(DATA_CROSS$CONSER,
                                          probs = c(0.01, 0.99)))

#### Compute EQ ####
attach(DATA_CROSS)
DATA_CROSS$AQ_rank <- (AQ - min(AQ)) / (max(AQ) - min(AQ)) * 100
DATA_CROSS$PERS_rank <- (PERS - min(PERS)) / (max(PERS) - min(PERS)) * 100
DATA_CROSS$PRED_rank <- (PRED - min(PRED)) / (max(PRED) - min(PRED)) * 100
DATA_CROSS$RELEV_rank <- (RELEV - min(RELEV)) / (max(RELEV) - min(RELEV)) * 100
DATA_CROSS$SMOOTH_rank <- (SMOOTH - min(SMOOTH)) / (max(SMOOTH) - min(SMOOTH)) * 100
DATA_CROSS$TIMEL_rank <- (TIMEL - min(TIMEL)) / (max(TIMEL) - min(TIMEL)) * 100
DATA_CROSS$CONSER_rank <- (CONSER - min(CONSER)) / (max(CONSER) - min(CONSER)) * 100
DATA_CROSS$EQ <- apply(DATA_CROSS[, 9:15], 1, mean, na.rm = TRUE)

#### Compute Control Variables ####
attach(DATA_CLEAN)
DATA_CLEAN$SIZE <- log(TotalAssets)
DATA_CLEAN$INVOP <- DeltaRevenue/Revenue
DATA_CLEAN$EXTFIN <- (CAPEX - CFO)/CAPEX
DATA_CLEAN$CAPEXRATIO <- CAPEX/TotalAssets
DATA_CLEAN$PPERATIO <- PPE/TotalAssets

DATA_CONTROL <- DATA_CLEAN %>%
  group_by(Identifier) %>%
  summarize(
    SIZE = mean(SIZE, na.rm = TRUE),
    INVOP = mean(INVOP, na.rm = TRUE),
    EXTFIN = mean(EXTFIN),
    CAPEXRATIO = mean(CAPEXRATIO, na.rm = TRUE),
    PPERATIO = mean(PPERATIO, na.rm = TRUE),
  )

DATA_CONTROL$SIZE <- Winsorize(DATA_CONTROL$SIZE, 
                               val = quantile(DATA_CONTROL$SIZE,
                                              probs = c(0.01, 0.99)))
DATA_CONTROL$INVOP <- Winsorize(DATA_CONTROL$INVOP, 
                               val = quantile(DATA_CONTROL$INVOP,
                                              probs = c(0.01, 0.99)))
DATA_CONTROL$EXTFIN <- Winsorize(DATA_CONTROL$EXTFIN, 
                               val = quantile(DATA_CONTROL$EXTFIN,
                                              probs = c(0.01, 0.99),
                                              na.rm = TRUE))
DATA_CONTROL$CAPEXRATIO <- Winsorize(DATA_CONTROL$CAPEXRATIO, 
                               val = quantile(DATA_CONTROL$CAPEXRATIO,
                                              probs = c(0.01, 0.99)))
DATA_CONTROL$PPERATIO <- Winsorize(DATA_CONTROL$PPERATIO, 
                               val = quantile(DATA_CONTROL$PPERATIO,
                                              probs = c(0.01, 0.99)))
#### Merge all Data ####
colnames(DATA_RAW)[1] <- "Identifier"
DATA_COUNTRIES_INDUSTRY <- DATA_RAW[, c("Identifier", "Country of Incorporation", "TRBC Industry Group Name")]
DATA_FINAL <- merge(DATA_CROSS, DATA_CONTROL, by = "Identifier")
DATA_FINAL <- merge(DATA_FINAL, DATA_COUNTRIES_INDUSTRY, by = "Identifier", all.x = TRUE)

source("Regression Results.R")


