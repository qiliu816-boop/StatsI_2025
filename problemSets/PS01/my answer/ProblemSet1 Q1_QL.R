# Remove all objects from the environment
rm(list=ls())

# Detach all non-basic packages
detachAllPackages <- function() {
  basic.packages <- c(
    "package:stats", "package:graphics", "package:grDevices", 
    "package:utils", "package:datasets",
    "package:methods", "package:base"
  )
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list) > 0) {
    for (package in package.list) detach(package, character.only=TRUE)
  }
}
detachAllPackages()

# Load packages (none required for this problem)
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# Example: lapply(c("ggplot2"), pkgTest)
lapply(c(), pkgTest) # No packages needed here

# Problem 1: Education

# IQ scores of 25 sampled students
y <- c(
  105, 69, 86, 100, 82, 111, 104, 110, 87, 108,
  87, 90, 94, 113, 112, 98, 80, 97, 95, 111,
  114, 89, 95, 126, 98
)

# 1. 90% Confidence Interval

# Compute sample statistics
n <- length(y)
sample_mean <- mean(y)
sample_sd <- sd(y)

# Calculate 90% confidence interval for the mean
alpha <- 0.10
t_crit <- qt(1 - alpha/2, df = n - 1)
se <- sample_sd / sqrt(n)
ci_lower <- sample_mean - t_crit * se
ci_upper <- sample_mean + t_crit * se

cat("Sample mean:", sample_mean, "\n")
cat("Sample SD:", sample_sd, "\n")
cat("90% CI for population mean: [", ci_lower, ",", ci_upper, "]\n")

# 2. One-sample t-test:
#    Is the school's mean IQ above 100? (alpha = 0.05)

t_test_result <- t.test(y, mu = 100, alternative = "greater", conf.level = 0.95)
print(t_test_result)
