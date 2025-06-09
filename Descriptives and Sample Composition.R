#### Sample Composition by Country ####
DATA_SAMPLE_BY_COUNTRY <- DATA_FINAL %>%
  group_by(Country) %>%
  summarise(Country_Count = n())

Country_Total <- sum(DATA_SAMPLE_BY_COUNTRY$Country_Count)

DATA_SAMPLE_BY_COUNTRY <- DATA_SAMPLE_BY_COUNTRY %>%
  mutate(Relative_Share = Country_Count / Country_Total * 100)

DATA_SAMPLE_BY_COUNTRY <- DATA_SAMPLE_BY_COUNTRY %>%
  arrange(desc(Country_Count)) %>%
  mutate(Cumulative_Share = cumsum(Relative_Share))

DATA_SAMPLE_BY_COUNTRY <- DATA_SAMPLE_BY_COUNTRY %>%
  mutate(
    Relative_Share = round(Relative_Share, 2),
    Cumulative_Share = round(Cumulative_Share, 2)
  )

DATA_SAMPLE_BY_COUNTRY %>%
  kable("html", col.names = c("Country", "Observations", "Relative Share (%)", "Cumulated (%)"), escape = FALSE) %>%
  kable_styling(full_width = F, position = "center", bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE, font_size = 14, color = "white", background = "#2E4053") %>%
  row_spec(1:nrow(DATA_SAMPLE_BY_COUNTRY), color = "black", font_size = 12) %>%
  save_kable("Sample Composition by Country.html")
#### Sample Composition by Industry ####
DATA_SAMPLE_BY_INDUSTRY <- DATA_FINAL %>%
  group_by(Industry) %>%
  summarise(Industry_Count = n())

Industry_Total <- sum(DATA_SAMPLE_BY_INDUSTRY$Industry_Count)

DATA_SAMPLE_BY_INDUSTRY <- DATA_SAMPLE_BY_INDUSTRY %>%
  mutate(Relative_Share = Industry_Count / Industry_Total * 100)

DATA_SAMPLE_BY_INDUSTRY <- DATA_SAMPLE_BY_INDUSTRY %>%
  arrange(desc(Industry_Count)) %>%
  mutate(Cumulative_Share = cumsum(Relative_Share))

DATA_SAMPLE_BY_INDUSTRY <- DATA_SAMPLE_BY_INDUSTRY %>%
  mutate(
    Relative_Share = round(Relative_Share, 2),
    Cumulative_Share = round(Cumulative_Share, 2)
  )

DATA_SAMPLE_BY_INDUSTRY %>%
  kable("html", col.names = c("Industry", "Observations", "Relative Share (%)", "Cumulated (%)"), escape = FALSE) %>%
  kable_styling(full_width = F, position = "center", bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE, font_size = 14, color = "white", background = "#2E4053") %>%
  row_spec(1:nrow(DATA_SAMPLE_BY_INDUSTRY), color = "black", font_size = 12) %>%
  save_kable("Sample Composition by Industry.html")
#### Descriptive Statistics ####
DATA_DESCRIPTIVES <- DATA_FINAL[c("TobinQ", "EQ", "SIZE", "INVOP", "EXTFIN", "CAPEXRATIO", "PPERATIO")]
stargazer(DATA_DESCRIPTIVES, 
          type = "html", 
          digits = 3, 
          omit.summary.stat = c("min", "max"),
          summary.stat = c("n", "mean", "sd", "p25", "median", "p75"),
          summary = TRUE, 
          out = "Descriptive_Statistics.html")

