#### Load Packages & Data Set ####
#Load Packages
library(readxl)
library(stargazer)
library(tidyr)
library(reshape2)
library(dplyr)
library(plm)
library(collapse)
library(purrr)
library(broom)

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

#5. Value Relevance
Formula_RELEV <- X15monthReturn ~ EARN + DELTA_EARN
Model_RELEV <- lm(DATA_PANEL, Formula_RELEV)

#6. Earnings Timeliness
Formula_TIMEL <- EARN ~ NEG + X15monthReturn + NEG*X15monthReturn
Model_TIMEL <- lm(DATA_PANEL, Formula_TIMEL)

#7. Earnings Conservatism
