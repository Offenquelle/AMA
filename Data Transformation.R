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
library(fixest)
library(modelsummary)
library(sjPlot)
library(kableExtra)
library(ggplot2)
library(lmtest)
library(corrplot)

#Import Data Set
DATA_RAW <- read_excel("Sample_V3.xlsx")

#### Transformation to Panel Frame ####
#Transform to Long Format
DATA_LONG = reshape(data = DATA_RAW,
                    idvar = "Identifier",
                    varying = 6:405,
                    sep = "-",
                    timevar = "Year",
                    times = c(2024,2023,2022,2021,2020,2019,2018,2017,2016,2015,2014,2013,2012,2011,2010,2009,2008,2007,2006,2005),
                    new.row.names = 1:2000000,
                    direction = "long")

#Transform to Panel Frame
DATA_PANEL <- pdata.frame(DATA_LONG, index = c("Identifier", "Year"))

#### Drop Firms with less than 10 consecutive Years of Data ####

#Define Key Variables
key_vars <- c("TotalAssets", "Price", "SharesOutstanding", "BookEquity", "DeltaCurrentAssets", "DeltaCurrentLiabilities", "DeltaCash",
              "DeltaShortTermDebt", "NIBEX", "DeltaNIBEX", "Depreciation", "Revenue", "DeltaRevenue", "CAPEX", "PPE", "Cash", "Debt", "SalesMargin",
              "NumberAnalysts", "X15MonthReturn")

DATA_PANEL <- as.data.frame(DATA_PANEL)

DATA_PANEL <- DATA_PANEL %>%
  mutate(complete_row = if_all(all_of(key_vars), ~ !is.na(.)))

longest_streak <- function(x) {
  r <- rle(x)
  if (any(r$values)) max(r$lengths[r$values == TRUE]) else 0
}

firms_with_streak <- DATA_PANEL %>%
  arrange(Identifier, Year) %>%
  group_by(Identifier) %>%
  summarise(max_streak = longest_streak(complete_row), .groups = "drop") %>%
  filter(max_streak >= 10)

# Keep only firms with >=7 consecutive complete years
DATA_PANEL <- DATA_PANEL %>%
  filter(complete_row) %>%                     # Keep only complete rows
  semi_join(firms_with_streak, by = "Identifier")  # Keep only qualified firms

# Convert back to pdata.frame if needed
DATA_PANEL <- pdata.frame(DATA_PANEL, index = c("Identifier", "Year"))

#### Construct Tobin's Q and Variables for EQ Measures ####
attach(DATA_PANEL)

#Tobin's Q
DATA_PANEL$MVE <- SharesOutstanding * Price
DATA_PANEL$TobinQ <- (TotalAssets + DATA_PANEL$MVE - BookEquity) / TotalAssets
RESULTS_Q <- aggregate(TobinQ ~ Identifier, data = DATA_PANEL, FUN = mean)

#1. Accruals Quality
DATA_PANEL$WCA <- DeltaCurrentAssets - DeltaCurrentLiabilities - DeltaCash + DeltaShortTermDebt
DATA_PANEL$CFO <- NIBEX - (DeltaCurrentAssets - DeltaCurrentLiabilities - DeltaCash + DeltaShortTermDebt - Depreciation)
DATA_PANEL$CFO_LAG <- L(DATA_PANEL$CFO, n = 1, shift = "time")
DATA_PANEL$CFO_LEAD <- L(DATA_PANEL$CFO, n = -1, shift = "time")
DATA_PANEL$TotalAssets_Lag <- L(DATA_PANEL$TotalAssets, n = 1, shift = "time")
DATA_PANEL$TotalAssets_Lead <- L(DATA_PANEL$TotalAssets, n = -1, shift = "time")

#2. Earnings Persistence
DATA_PANEL$E <- NIBEX / SharesOutstanding
DATA_PANEL$E_LAG <- L(DATA_PANEL$E, n = 1, shift = "time")
DATA_PANEL[is.na(DATA_PANEL) | DATA_PANEL == "-Inf" | DATA_PANEL == "Inf"] = NA

#5. Value Relevance
DATA_PANEL$EARN <- NIBEX/DATA_PANEL$MVE
DATA_PANEL$DELTA_EARN <- NIBEX/DATA_PANEL$MVE

#6. Earnings Timeliness
DATA_PANEL$NEG <- ifelse(X15MonthReturn < 0, 1, 0)

#### Clean the Panel Data ####
DATA_CLEAN <- DATA_PANEL[complete.cases(DATA_PANEL), ]

#### EQ Measure Regressions ####

#1. Accruals Quality
FORMULA_AQ <- WCA/TotalAssets ~ (CFO/TotalAssets) + (CFO_LAG/TotalAssets_Lag) + (CFO_LEAD/TotalAssets_Lag)
RESULTS_AQ <- DATA_CLEAN %>%
  group_by(Identifier) %>%
  do({
    Model_AQ <- lm(FORMULA_AQ, data = .)
    data.frame(AQ = sd(residuals(Model_AQ)))
  })

#2. Earnings Persistence
FORMULA_PERS <- E ~ E_LAG
DATA_CLEAN[is.na(DATA_CLEAN) | DATA_CLEAN == "-Inf" | DATA_CLEAN == "Inf"] = NA
RESULTS_PERS <- DATA_CLEAN %>%
  group_by(Identifier) %>%
  group_split() %>%
  map_df(function(DATA_CLEAN) {
    model <- lm(FORMULA_PERS, data = DATA_CLEAN)
    tidy_model <- broom::tidy(model)
    x1_coef <- tidy_model %>% filter(term == "E_LAG")
    tibble(
      Identifier = unique(DATA_CLEAN$Identifier),
      E_LAG_estimate = x1_coef$estimate,
      var_res = var(resid(model))
    )
  })
RESULTS_PERS$PERS <- RESULTS_PERS$E_LAG_estimate *-1

#3. Earnings Predictability
RESULTS_PERS$PRED <- sqrt(RESULTS_PERS$var_res)

#4. Earnings Smoothness
RESULTS_SMOOTH <- DATA_CLEAN %>%
  group_by(Identifier) %>%
  summarise(
    SMOOTH = sd(NIBEX/TotalAssets)/sd(CFO/TotalAssets)
  )

#5. Value Relevance
FORMULA_RELEV <- X15MonthReturn ~ EARN/MVE + DELTA_EARN/MVE
RESULTS_RELEV <- DATA_CLEAN %>%
  group_by(Identifier) %>%
  do(
    glance(lm(FORMULA_RELEV, data = .))
  ) %>%
  select(Identifier, r.squared) %>%
  ungroup()
RESULTS_RELEV$RELEV <- RESULTS_RELEV$r.squared*-1

#6. Earnings Timeliness
FORMULA_TIMEL <- EARN ~ NEG + X15MonthReturn + NEG*X15MonthReturn
RESULTS_TIMEL <- DATA_CLEAN %>%
  group_by(Identifier) %>%
  do({
    model <- lm(FORMULA_TIMEL, data = .)
    tidy_model <- tidy(model)
    glance_model <- glance(model)
    tibble(
      r_squared = glance_model$r.squared,
      coef_X15 = tidy_model$estimate[tidy_model$term == "X15MonthReturn"],
      coef_interaction = tidy_model$estimate[tidy_model$term == "NEG:X15MonthReturn"]
    )
  }) %>%
  ungroup()
RESULTS_TIMEL$TIMEL <- RESULTS_TIMEL$r_squared*-1

#7. Earnings Conservatism
RESULTS_TIMEL$CONSER <- (-1*(RESULTS_TIMEL$coef_X15+RESULTS_TIMEL$coef_interaction))/RESULTS_TIMEL$coef_X15

#### Aggregate Results ####
RESULTS_AQ$Identifier <- as.character(RESULTS_AQ$Identifier)
RESULTS_PERS$Identifier <- as.character(RESULTS_PERS$Identifier)
RESULTS_RELEV$Identifier <- as.character(RESULTS_RELEV$Identifier)
RESULTS_SMOOTH$Identifier <- as.character(RESULTS_SMOOTH$Identifier)
RESULTS_TIMEL$Identifier <- as.character(RESULTS_TIMEL$Identifier)
RESULTS_Q$Identifier <- as.character(RESULTS_Q$Identifier)

DATA_CROSS <- RESULTS_AQ %>%
  left_join(RESULTS_PERS, by = "Identifier") %>%
  left_join(RESULTS_RELEV, by = "Identifier") %>%
  left_join(RESULTS_SMOOTH, by = "Identifier") %>%
  left_join(RESULTS_TIMEL, by = "Identifier") %>%
  left_join(RESULTS_Q, by = "Identifier")

DATA_CROSS <- DATA_CROSS %>%
  select(Identifier, AQ, PERS, PRED, RELEV, SMOOTH, TIMEL, CONSER, TobinQ)

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
DATA_CROSS$TobinQ <- Winsorize(DATA_CROSS$TobinQ, 
                               val = quantile(DATA_CROSS$TobinQ,
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
DATA_CLEAN$INVOP <- DeltaRevenue
DATA_CLEAN$EXTFIN <- (CAPEX - CFO)/CAPEX
DATA_CLEAN$CAPEX_Ratio <- CAPEX/TotalAssets
DATA_CLEAN$PPE_Ratio <- PPE/TotalAssets
DATA_CLEAN$Cash_Ratio <- Cash/TotalAssets
DATA_CLEAN$Leverage <- Debt/TotalAssets
DATA_CLEAN$SalesM <- SalesMargin

DATA_CONTROL <- DATA_CLEAN %>%
  group_by(Identifier) %>%
  summarize(
    SIZE = mean(SIZE),
    INVOP = mean(INVOP),
    EXTFIN = mean(EXTFIN),
    CAPEX_Ratio = mean(CAPEX_Ratio),
    PPE_Ratio = mean(PPE_Ratio),
    Cash_Ratio = mean(Cash_Ratio),
    Leverage = mean(Leverage),
    SalesM = mean(SalesM)
  )

DATA_CONTROL[is.na(DATA_CONTROL) | DATA_CONTROL == "-Inf" | DATA_CONTROL == "Inf"] = NA
DATA_CONTROL <- DATA_CONTROL[complete.cases(DATA_CONTROL), ]

DATA_CONTROL$SIZE <- Winsorize(DATA_CONTROL$SIZE, 
                               val = quantile(DATA_CONTROL$SIZE,
                                              probs = c(0.01, 0.99)))
DATA_CONTROL$INVOP <- Winsorize(DATA_CONTROL$INVOP, 
                               val = quantile(DATA_CONTROL$INVOP,
                                              probs = c(0.01, 0.99)))
DATA_CONTROL$EXTFIN <- Winsorize(DATA_CONTROL$EXTFIN, 
                               val = quantile(DATA_CONTROL$EXTFIN,
                                              probs = c(0.01, 0.99)))
DATA_CONTROL$CAPEX_Ratio <- Winsorize(DATA_CONTROL$CAPEX_Ratio, 
                               val = quantile(DATA_CONTROL$CAPEX_Ratio,
                                              probs = c(0.01, 0.99)))
DATA_CONTROL$PPE_Ratio <- Winsorize(DATA_CONTROL$PPE_Ratio, 
                               val = quantile(DATA_CONTROL$PPE_Ratio,
                                              probs = c(0.01, 0.99)))
DATA_CONTROL$PPE_Ratio <- Winsorize(DATA_CONTROL$PPE_Ratio, 
                               val = quantile(DATA_CONTROL$PPE_Ratio,
                                              probs = c(0.01, 0.99)))
DATA_CONTROL$Cash_Ratio <- Winsorize(DATA_CONTROL$Cash_Ratio, 
                               val = quantile(DATA_CONTROL$Cash_Ratio,
                                              probs = c(0.01, 0.99)))
DATA_CONTROL$Leverage <- Winsorize(DATA_CONTROL$Leverage, 
                               val = quantile(DATA_CONTROL$Leverage,
                                              probs = c(0.01, 0.99)))
DATA_CONTROL$SalesM <- Winsorize(DATA_CONTROL$SalesM, 
                               val = quantile(DATA_CONTROL$SalesM,
                                              probs = c(0.01, 0.99)))

#### Merge all Data ####
colnames(DATA_RAW)[1] <- "Identifier"
DATA_COUNTRIES_INDUSTRY <- DATA_RAW[, c("Identifier", "Country of Incorporation", "TRBC Industry Group Name")]
DATA_FINAL <- merge(DATA_CROSS, DATA_CONTROL, by = "Identifier")
DATA_FINAL <- merge(DATA_FINAL, DATA_COUNTRIES_INDUSTRY, by = "Identifier", all.x = TRUE)
colnames(DATA_FINAL)[26] <- "Country"
colnames(DATA_FINAL)[27] <- "Industry"

source("Regression Results.R")





