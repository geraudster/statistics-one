setwd("/home/geraud/projets/stat1/lab6")
library(psych)
data <- read.table("Stats1.13.HW.06.txt", header=T)
describe(data)
cor(data[2:4])

# Q1
model1 <- lm(data$salary ~ data$years)
summary(model1)
model1$coefficients[2]

# Q2
confint(model1)

# Q3
model2 <- lm(data$salary ~ data$years + data$courses)
summary(model2)
model2$coefficients[2]

# Q4
confint(model2)

# Q5
profession.code <- C(data$profession, treatment)
model3 <- lm(data$salary ~ data$years + data$courses + (profession.code))
summary(model3)
model3$coefficients['profession.codelawyer']

# Q6
summary(model3)

# Q7
model3$coefficients['profession.codeteacher']

# Q8
summary(model3)

# Q9
data.means <- tapply(data$salary, data$profession, mean)
data.means[1]-data.means[3]

# Q10
model4 <- lm(data$salary ~ data$courses + profession.code)
summary(model4)
anova(model1,model3)
anova(model2,model3)
anova(model4,model3)