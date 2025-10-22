# ==============================================
# PS02 Assignment - Q1 and Q2 R Code (Updated Version)
# Author: Qi Liu
# ==============================================

# --------------------------
# Q1: Political Science – Chi-Square Test
# --------------------------

# Step 1. Observed data (2×3 table)
obs <- matrix(c(14, 6, 7,
                7, 7, 1), 
              nrow = 2, byrow = TRUE)

rownames(obs) <- c("Upper", "Lower")
colnames(obs) <- c("NotStopped", "Bribe", "Warning")
obs

# Step 2. Expected frequencies
rowSums <- rowSums(obs)
colSums <- colSums(obs)
total <- sum(obs)
expected <- outer(rowSums, colSums) / total
expected

# Step 3. Compute Chi-square statistic manually
chi2 <- sum((obs - expected)^2 / expected)
chi2

# Step 4. Degrees of freedom and p-value
df <- (nrow(obs) - 1) * (ncol(obs) - 1)
p_value <- 1 - pchisq(chi2, df)
df
p_value

# Step 5. Built-in Chi-square test (verify)
chisq.test(obs, correct = FALSE)

# Step 6. Pearson residuals
pearson_res <- (obs - expected) / sqrt(expected)
pearson_res

# Step 7. Adjusted standardized residuals
row_prop <- rowSums / total
col_prop <- colSums / total
adj_res <- matrix(NA, nrow = nrow(obs), ncol = ncol(obs))
for (i in 1:nrow(obs)) {
  for (j in 1:ncol(obs)) {
    adj_res[i, j] <- (obs[i, j] - expected[i, j]) /
      sqrt(expected[i, j] * (1 - row_prop[i]) * (1 - col_prop[j]))
  }
}
rownames(adj_res) <- rownames(obs)
colnames(adj_res) <- colnames(obs)
adj_res


# --------------------------
# Q2: Economics – Regression Analysis (修正版)
# --------------------------

# Step 1. Load data from online source
url <- "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
women <- read.csv(url)

# Step 2. Explore data
head(women)
summary(women$water)
table(women$reserved)

# Step 3. Run simple linear regression (water ~ reserved)
model <- lm(water ~ reserved, data = women)
summary(model)

# Step 4. Install and load required packages (if not installed)
if (!require("sandwich")) install.packages("sandwich")
if (!require("lmtest")) install.packages("lmtest")

library(sandwich)
library(lmtest)

# Step 5. Compute robust standard errors (HC1)
coeftest(model, vcov = vcovHC(model, type = "HC1"))

# End of script

