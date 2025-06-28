#### Multicollinearity Test (only EQ) ####
cor_matrix_1 <- cor(DATA_FINAL[c("TobinQ", "EQ", "SIZE", "INVOP", "EXTFIN", "CAPEX_Ratio", "PPE_Ratio", "Cash_Ratio", "Leverage", "SalesM")])
png("Correlation Matrix (only EQ).png", 
    width = 2000,
    height = 2000,
    res = 300)

corrplot(cor_matrix_1, 
         method = "color",
         type = "lower",
         col = colorRampPalette(c("red", "white", "green"))(200),
         tl.col = "black",
         tl.srt = 45,
         family = "Times New Roman",
         addCoef.col = "black",
         number.cex = 0.7,
         p.mat = cor.mtest(DATA_FINAL[c("TobinQ", "EQ", "SIZE", "INVOP", "EXTFIN", "CAPEX_Ratio", "PPE_Ratio", "Cash_Ratio", "Leverage", "SalesM")])$p,
         sig.level = 0.05,
         insig = "label_sig",
         pch = "      *",
         pch.cex = 1.2,
         diag = FALSE)
dev.off()

#### Multicollinearity Test (individual EQ measures) ####
cor_matrix_2 <- cor(DATA_FINAL[c("TobinQ", "EQ", "AQ", "PERS", "PRED", "RELEV", "SMOOTH", "TIMEL", "CONSER","SIZE", "INVOP", "EXTFIN", "CAPEX_Ratio", "PPE_Ratio", "Cash_Ratio", "Leverage", "SalesM")])
png("Correlation Matrix (individual EQ measures).png", 
    width = 3000,
    height = 3000,
    res = 300)

corrplot(cor_matrix_2, 
         method = "color",
         type = "lower",
         col = colorRampPalette(c("red", "white", "green"))(200),
         tl.col = "black",
         tl.srt = 45,
         family = "Times New Roman",
         addCoef.col = "black",
         number.cex = 0.7,
         p.mat = cor.mtest(DATA_FINAL[c("TobinQ", "EQ", "AQ", "PERS", "PRED", "RELEV", "SMOOTH", "TIMEL", "CONSER","SIZE", "INVOP", "EXTFIN", "CAPEX_Ratio", "PPE_Ratio", "Cash_Ratio", "Leverage", "SalesM")])$p,
         sig.level = 0.05,
         insig = "label_sig",
         pch = "      *",
         pch.cex = 1.2,
         diag = FALSE)
dev.off()

#### Normal Distribution of Residuals (only EQ)  ####
residuals_1 <- resid(MODEL_EQ_1)
ggplot(data = data.frame(residuals_1),
       aes(sample = residuals_1)) +
       stat_qq(distribution = qnorm) +
       stat_qq_line(distribution = qnorm, color = "red") +
       labs(title = "Q-Q-Plot of Residuals", x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
       theme(text = element_text(family = "Times New Roman"))

ggsave(plot = last_plot(), filename = "Q-Q Plot (only EQ).png",
       device = png, type = "cairo", dpi = 300,
       width = 4, height = 3, units = "in")

#### Normal Distribution of Residuals (individual EQ measures) ####
residuals_2 <- resid(MODEL_EQ_2)
ggplot(data = data.frame(residuals_2),
       aes(sample = residuals_2)) +
  stat_qq(distribution = qnorm) +
  stat_qq_line(distribution = qnorm, color = "red") +
  labs(title = "Q-Q-Plot of Residuals", x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme(text = element_text(family = "Times New Roman"))

ggsave(plot = last_plot(), filename = "Q-Q Plot (individual EQ measures).png",
       device = png, type = "cairo", dpi = 300,
       width = 4, height = 3, units = "in")

#### Heteroskedasticity Test (only EQ) ####
white_res_1 <- MODEL_EQ_1$residuals
white_test_1 <- bptest(white_res_1 ~ fitted(MODEL_EQ_1), data = DATA_FINAL, studentize = FALSE)
print (white_test_1)

png("Residuals Plot (only EQ).png", width = 3000, height = 2000, res = 300)

plot(fitted(MODEL_EQ_1), resid(MODEL_EQ_1),
     family = "Times New Roman",
     main = "Residuals vs. Predicted Values",
     xlab = "Predicted Values", ylab = "Residuals",
     pch = 20, col = "black")
abline(h = 0, col = "red")

dev.off()

#### Heteroskedasticity Test (individual EQ measures) ####
white_res_2 <- MODEL_EQ_2$residuals
white_test_2 <- bptest(white_res_2 ~ fitted(MODEL_EQ_2), data = DATA_FINAL, studentize = FALSE)
print (white_test_2)

png("Residuals Plot (individual EQ measures).png", width = 3000, height = 2000, res = 300)

plot(fitted(MODEL_EQ_1), resid(MODEL_EQ_1),
     family = "Times New Roman",
     main = "Residuals vs. Predicted Values",
     xlab = "Predicted Values", ylab = "Residuals",
     pch = 20, col = "black")
abline(h = 0, col = "red")

dev.off()

source("Descriptives and Sample Composition.R")