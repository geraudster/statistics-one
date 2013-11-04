salaries <- read.table("Stats1.13.HW.04.txt", header=T)

salaries$ID <- factor(salaries$ID)
# Q1
describe(salaries)
round(cor(salaries[2:3]),2)
cor.test(salaries$salary,salaries$years)

# Q2
round(cor(salaries[2],salaries[4]),2)
cor.test(salaries$salary,salaries$courses)

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
hist(salaries$salary)
hist(salaries$years)
hist(salaries$courses)
layout(matrix(c(1,1), 1, 1, byrow = TRUE))

# Q3
model1 <- lm(salaries$salary ~ salaries$years)
summary(model1)
plot(salaries$salary ~ salaries$years, main = "Scatterplot", ylab = "Salary", xlab = "Years")
abline(model1, col="blue")

# Q4
model2 <- lm(salaries$salary ~ salaries$courses)
summary(model2)
plot(salaries$salary ~ salaries$courses, main = "Scatterplot", ylab = "Salary", xlab = "Courses")
abline(model2, col="blue")

# Q5
model3 <- lm(salaries$salary ~ salaries$years + salaries$courses)
summary(model3)

# Q6
model1.z <- lm(scale(salaries$salary) ~ scale(salaries$years))
summary(model1.z)

# Q7
model2.z <- lm(scale(salaries$salary) ~ scale(salaries$courses))
summary(model2.z)

# Q8
salaries$predicted <- fitted(model3)

plot(salaries$salary ~ salaries$predicted, main = "Scatterplot", ylab = "Salary", xlab = "Model 3 Predicted Scores")
abline(lm(salaries$salary ~ salaries$predicted), col="blue")

describe(salaries$predicted)

# Q9
salaries$resid <- resid(model3)
hist(salaries$resid)
plot(salaries$predicted ~ salaries$resid, main = "Scatterplot", ylab = "Model 3 Predicted Scores", xlab = "Model 3 Residuals")
abline(lm(salaries$predicted ~ salaries$resid), col="blue")

describe(salaries$resid)

# Q10
hist(salaries$resid)
