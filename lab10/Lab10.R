# Load packages
library(psych)
library(aod)
library(QuantPsyc)

# Q1
data <- read.table("Stats1.13.HW.10.txt", header=T)
describe(data)
describeBy(data,data$change)

# Q2 & Q3
model <- glm(data$change ~ data$age + data$educ + data$gdp + data$co2, family= binomial)
summary(model)

# Q4
confint(model)

# Q5
confint.default(model)

# Q6
# model comparison
with(model, null.deviance - deviance)

# Q7
# df comparison
with(model, df.null - df.residual)

# Q8
# p-value
with(model, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = FALSE))

# Q9
wald.test(b = coef(model), Sigma = vcov(model), Terms = 2)

# Q10
ClassLog(model, data$change)
exp(coef(model))

# Tests 
model2 <- glm(data$change ~ data$age + data$educ, family= binomial)
summary(model2)

confint(model2)
with(model2, null.deviance - deviance)
ClassLog(model2, data$change)