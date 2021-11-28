# ========================================
# 1. Loading data from workspace
# ========================================

# Loading data from previous work
load("data/data.RData")

# Summary of data 
glimpse(data)
summary(data)


# Histogram of all numeric variables

data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Boxplot of dependent variable
boxplot(data$age)


# Represent it
data$sex <- as.factor(data$sex)

ggplot(data=data, aes(x=age, group=sex, fill=sex)) +
  geom_density(adjust=1.5, alpha=.4) 

# Kaplan-Meier survival estimates

fit <- survfit(Surv(time, status) ~ sex, data = data)
print(fit)

# Access to the sort summary table
summary(fit)$table

data_vis <- data.frame(time = fit$time,
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censor = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower
)

# Survival curves - need fix, function does not work as described on the website
ggsurvplot(data_vis,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

################################## DAMIAN

library(eha)

colnames(data)
formula <- Surv(time, status) ~ sex + age + meal.cal + ph.ecog +ph.karno + pat.karno + wt.loss
#formula <- Surv(time, status) ~ sex + ph.ecog + ph.karno

fit <- phreg(formula, data = data, dist = "weibull")
fit
plot(fit,
  main = NULL,
  xlab = "Duration",
  ylab = "",
  score = 1
)

fit.cr <- coxreg(formula, data = data)
fit.cr
plot(fit.cr)
check.dist(fit.cr, fit)

#How do you find the p-value from Wald?
#The p-value of a test gives the probability of observing a test statistic as extreme as the one observed, if the null hypothesis were true. For the Wald test: p = P(|Z| > |Tobs|), where Z ∼ N(0,1) is a standard normal random variable.


###

oldpar <- par(mfrow = c(2, 2))
fit.cr <- coxreg(formula, data = data)
fit.w <- phreg(formula, data = data)
fit.g <- phreg(formula, data = data,
               dist = "gompertz")
fit.ln <- phreg(formula, data = data,
                dist = "loglogistic")
fit.ev <- phreg(formula, data = data,
                dist = "ev")
check.dist(fit.cr, fit.w, printLegend = FALSE)
check.dist(fit.cr, fit.g, printLegend = FALSE)

10
check.surv
check.dist(fit.cr, fit.ln, printLegend = FALSE)
check.dist(fit.cr, fit.ev, printLegend = FALSE)
par(oldpar)

###
