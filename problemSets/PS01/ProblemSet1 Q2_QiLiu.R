##############################
# (1) Environment Preparation
##############################

# Remove all objects from environment
rm(list=ls())

# Function: detach all non-basic packages from the session
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils",
                      "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0) for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

# Function: Automatically install and load required packages
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg)){
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}
pkgTest(c("ggplot2"))

##############################
# (2) Data Import & Inspection
##############################

# READ DATA FROM URL (changed as requested)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)

# Quick data inspection
str(expenditure)          # Data structure
summary(expenditure)      # Descriptive statistics
head(expenditure)         # First few rows

# Variable descriptions:
# Y     : Per capita expenditure on shelters/housing assistance (response)
# X1    : Per capita personal income in state
# X2    : # residents per 100,000 that are "financially insecure"
# X3    : # people per thousand residing in urban areas
# Region: US region (1=Northeast, 2=North Central, 3=South, 4=West)

##############################
# (3) Exploratory Data Analysis
##############################

# a) Scatterplot matrix for Y, X1, X2, X3
pairs(expenditure[,c("Y","X1","X2","X3")],
      main="Scatterplot Matrix: Y, X1, X2, X3")

# Correlation matrix for the four variables
cor_mat <- cor(expenditure[,c("Y","X1","X2","X3")])
print(round(cor_mat, 2))

##############################
# (4) Visualization: Y vs Region
##############################

region.lab <- c("Northeast","North Central","South","West")

# Box plot: Y by Region group
boxplot(Y ~ Region, 
        data = expenditure,
        names = region.lab,
        ylab = "Per Capita Housing Assistance Expenditure",
        xlab = "Region",
        main = "Per Capita Expenditure by US Region",
        col = c("skyblue","lightgreen","orange","red"))

# Calculate and print mean Y for each region
region.means <- tapply(expenditure$Y, expenditure$Region, mean)
print(region.means)

##############################
# (5) Visualization: Y vs X1
##############################

# Basic scatterplot with regression line
plot(expenditure$X1, expenditure$Y,
     xlab = "Per Capita Personal Income",
     ylab = "Per Capita Housing Assistance Expenditure",
     main = "Housing Assistance Expenditure vs Personal Income")
abline(lm(Y ~ X1, data = expenditure), col="red", lwd=2)

##############################
# (6) Visualization: Y vs X1 by Region (Color/Symbol)
##############################

# Advanced colored/scatterplot by region using ggplot2
library(ggplot2)
ggplot(expenditure, aes(x = X1, y = Y, color = factor(Region), shape = factor(Region))) +
  geom_point(size = 3) +
  labs(
    title = "Housing Assistance Expenditure vs Personal Income by Region",
    x = "Per Capita Personal Income",
    y = "Per Capita Housing Assistance Expenditure",
    color = "Region", shape = "Region"
  ) +
  scale_color_manual(values = c("steelblue","forestgreen","orange","red"),
                     labels = region.lab) +
  scale_shape_manual(values = 1:4, labels = region.lab) +
  theme_minimal()

# End of script


