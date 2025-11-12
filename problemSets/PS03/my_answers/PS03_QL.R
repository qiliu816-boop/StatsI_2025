rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
})

# Paths
data_path <- "incumbents_subset.csv"   
dir.create("figs", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# Helper: safe_ggsave
save_plot <- function(p, filename, width = 7, height = 5, dpi = 300) {
  ggsave(file.path("figs", filename), p, width = width, height = height, dpi = dpi)
  message(sprintf("Saved plot -> figs/%s", filename))
}

# -----------------------------
# 1) Load data
# -----------------------------
if (!file.exists(data_path)) {
  stop("Data file 'incumbents_subset.csv' not found in the working directory. ",
       "Place the CSV next to PS03.R and re-run.")
}
dat <- readr::read_csv(data_path)

# Quick glance
print(glimpse(dat))
print(summary(dat))

# -----------------------------
# 2) Q1: voteshare ~ difflog
# -----------------------------
m1 <- lm(voteshare ~ difflog, data = dat)
print(summary(m1))
res_q1 <- resid(m1)

p1 <- ggplot(dat, aes(difflog, voteshare)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Q1: voteshare ~ difflog",
       x = "difflog (incumbent - challenger spending, log-diff)",
       y = "voteshare (incumbent)") +
  theme_minimal()
save_plot(p1, "q1_voteshare_difflog.png")

coef_q1 <- coef(m1)
cat("\nQ1 prediction equation:\n")
cat(sprintf("voteshare_hat = %.4f + %.4f * difflog\n",
            unname(coef_q1[1]), unname(coef_q1[2])))

# -----------------------------
# 3) Q2: presvote ~ difflog
# -----------------------------
m2 <- lm(presvote ~ difflog, data = dat)
print(summary(m2))
res_q2 <- resid(m2)

p2 <- ggplot(dat, aes(difflog, presvote)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Q2: presvote ~ difflog",
       x = "difflog",
       y = "presvote") +
  theme_minimal()
save_plot(p2, "q2_presvote_difflog.png")

coef_q2 <- coef(m2)
cat("\nQ2 prediction equation:\n")
cat(sprintf("presvote_hat = %.4f + %.4f * difflog\n",
            unname(coef_q2[1]), unname(coef_q2[2])))

# -----------------------------
# 4) Q3: voteshare ~ presvote
# -----------------------------
m3 <- lm(voteshare ~ presvote, data = dat)
print(summary(m3))

p3 <- ggplot(dat, aes(presvote, voteshare)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Q3: voteshare ~ presvote",
       x = "presvote",
       y = "voteshare") +
  theme_minimal()
save_plot(p3, "q3_voteshare_presvote.png")

coef_q3 <- coef(m3)
cat("\nQ3 prediction equation:\n")
cat(sprintf("voteshare_hat = %.4f + %.4f * presvote\n",
            unname(coef_q3[1]), unname(coef_q3[2])))

# -----------------------------
# 5) Q4: residual-on-residual (FWL / add-variable plot numeric version)
#     res_q1 = residuals of voteshare ~ difflog
#     res_q2 = residuals of presvote ~ difflog
# -----------------------------
resdf <- tibble(res_q1 = res_q1, res_q2 = res_q2)
m4 <- lm(res_q1 ~ res_q2, data = resdf)   # intercept included by default
print(summary(m4))

p4 <- ggplot(resdf, aes(res_q2, res_q1)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Q4: Residuals(voteshare|difflog) ~ Residuals(presvote|difflog)",
       x = "residuals of presvote on difflog",
       y = "residuals of voteshare on difflog") +
  theme_minimal()
save_plot(p4, "q4_residual_on_residual.png")

coef_q4 <- coef(m4)
cat("\nQ4 prediction equation (residuals):\n")
cat(sprintf("res_q1_hat = %.6f + %.6f * res_q2\n",
            unname(coef_q4[1]), unname(coef_q4[2])))

# -----------------------------
# 6) Q5: Multiple regression and FWL check
# -----------------------------
m5 <- lm(voteshare ~ difflog + presvote, data = dat)
print(summary(m5))

coef_q5 <- coef(m5)
cat("\nQ5 prediction equation:\n")
cat(sprintf("voteshare_hat = %.4f + %.4f * difflog + %.4f * presvote\n",
            unname(coef_q5[1]), unname(coef_q5[2]), unname(coef_q5[3])))

# Compare Q4 slope vs Q5 coefficient on presvote (FWL theorem)
comp <- bind_rows(
  tidy(m4) |> filter(term == "res_q2") |> mutate(model = "Q4 residual-on-residual"),
  tidy(m5) |> filter(term == "presvote") |> mutate(model = "Q5 multiple OLS")
) |>
  select(model, estimate, std.error, statistic, p.value)

print(comp)
readr::write_csv(comp, file.path("outputs", "q4_q5_fwl_comparison.csv"))
message("Saved FWL comparison -> outputs/q4_q5_fwl_comparison.csv")

# Save tidy summaries for each model 
models_tidy <- list(
  Q1 = tidy(m1), Q2 = tidy(m2), Q3 = tidy(m3), Q4 = tidy(m4), Q5 = tidy(m5)
)
for (nm in names(models_tidy)) {
  out_path <- file.path("outputs", paste0("model_", tolower(nm), "_tidy.csv"))
  readr::write_csv(models_tidy[[nm]], out_path)
  message(sprintf("Saved tidy table -> %s", out_path))
}

# End of script
message("\nAll done. Check 'figs/' for plots and 'outputs/' for tables.\n")

