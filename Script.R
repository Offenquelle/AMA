#Load Packages
library(readxl)
library(stargazer)
library(tidyr)
library(reshape2)
library(dplyr)

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

#### Variable Construction ####
attach(DATA_LONG)


