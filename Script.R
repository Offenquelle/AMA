#Load Packages
library(readxl)
library(stargazer)
library(tidyr)
library(reshape2)
library(dplyr)
library(plm)
library(collapse)
#Import Data
DATA <- read_excel("Sample_V2.xlsx")

#Transform to Long Format
DATA_LONG = reshape(data = DATA,
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
Formula_AQ <- WCA ~ CFO/TotalAssets + CFO_LAG/TotalAssets + CFO_LEAD/TotalAssets
Model_AQ <- lm(Formula_AQ, data = DATA_PANEL)


