data <- read.table("Stats1.13.HW.04.txt", header=T)
describe(data)

# Q1
round(cor(data[2:4]),2)
cor.test(data$years, data$salary)
model1 <- lm(data$salary ~ data$years)
summary(model1)
ggplot(data, aes(x = years, y = salary)) + geom_smooth(method = "lm") + 
  geom_point()

round(confint(model1),2)

# Q2
cor.test(data$courses, data$salary)
model2 <- lm(data$salary ~ data$courses)
summary(model2)
ggplot(data, aes(x = courses, y = salary)) + geom_smooth(method = "lm") + 
  geom_point()

round(confint(model2),2)

# Q3
model3 <- lm(data$salary ~ data$courses + data$years)
summary(model3)
ggplot(data, aes(x = courses + years, y = salary)) + geom_smooth(method = "lm") + 
  geom_point()

round(confint(model3),2)

anova(model1, model3)
anova(model2, model3)

# Q4
model3.z <- lm(scale(data$salary) ~ scale(data$courses) + scale(data$years))
summary(model3.z)
round(confint(model3),2)

# Q5
# sample

# Q6
set.seed(1)
data.15 <- data[sample(nrow(data),15),]
data.cor <- round(cor(data[2:4]),2)
data.15.cor <- round(cor(data.15[2:4]),2)
cor.test(data.15$years, data.15$salary)
data.15.cor[2,1] < data.cor[2,1]

# Q7
data.51_70 <- data[51:70,]
model3.51_70.z <- lm(scale(data.51_70$salary) ~ scale(data.51_70$courses) + scale(data.51_70$years))
summary(model3.51_70.z)
round(confint(model3.51_70.z),2)
ggplot(data.51_70, aes(x = courses + years, y = salary)) + geom_smooth(method = "lm") + 
  geom_point()

# Q8
model1.51_70 <- lm(data.51_70$salary ~ data.51_70$years)
model2.51_70 <- lm(data.51_70$salary ~ data.51_70$courses)
model3.51_70 <- lm(data.51_70$salary ~ data.51_70$courses + data.51_70$years)
anova(model1.51_70, model3.51_70)
anova(model2.51_70, model3.51_70)
anova(model1.51_70, model2.51_70)

# Q9
summary(model3.51_70)
data.predicted <-fitted(model3.51_70)
