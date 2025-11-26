library(car)
data(Prestige)               # Load Prestige dataset

# -------------------------------
# (a) Create dummy variable "professional"
# professional = 1 if type == "prof", otherwise 0
# -------------------------------
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# Check coding
table(Prestige$professional, Prestige$type)

# -------------------------------
# (b) Run regression model with interaction
# Model: prestige ~ income * professional
# income * professional automatically includes:
#   income + professional + income:professional
# -------------------------------
m1 <- lm(prestige ~ income * professional, data = Prestige)

# View regression results
summary(m1)

# -------------------------------
# (f) Marginal effect of income when professional = 1
# Marginal effect = beta1 + beta3
# Multiply by 1000 to get effect of $1000 increase
# -------------------------------
beta <- coef(m1)
marg_income_prof1_per1 <- beta["income"] + beta["income:professional"]
marg_income_prof1_1000 <- marg_income_prof1_per1 * 1000
marg_income_prof1_per1
marg_income_prof1_1000

# -------------------------------
# (g) Effect of switching from non-professional (0)
# to professional (1) when income = 6000
# Effect = beta2 + 6000 * beta3
# -------------------------------
effect_prof_switch_6000 <-
  beta["professional"] + 6000 * beta["income:professional"]
effect_prof_switch_6000


