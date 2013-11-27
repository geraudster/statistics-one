data <- read.table("Stats1.13.HW.07.txt", header=T)

describe(data)

library(psych)
# Q1 / Q2 / Q3
cor(data)

# Q4
model1 <- lm(data$happy ~ data$extra)
summary(model1)

# Q5
model2 <- lm(data$happy ~ data$extra + data$diverse)
summary(model2)

# Q6
confint(model1)

# Q7
confint(model2)

# Q8
model3 <- lm(data$diverse ~ data$extra)
summary(model3)

# Q9
model.ALL <- sobel(data$extra, data$diverse, data$happy)
model.ALL

# Q10
