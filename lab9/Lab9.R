library(psych)
library(car)
library(lsr)
library(ggplot2)

data <- read.table("Stats1.13.HW.09.txt", header=T)

# Q1
class(data$Prime)
class(data$Haste)

# Q2,Q3,Q4
data$Prime <- factor(data$Prime, levels = c(1,2), labels = c("Parable", "Control"))
data$Haste <- factor(data$Haste, levels = c(1,2,3), labels = c("Early", "On Time", "Late"))

ggplot(data, aes(x=Haste, y=Helping, color=Prime)) + 
  geom_boxplot() +
  guides(fill=FALSE) 

# Check for homogeneity of variance
leveneTest(data$Helping ~ data$Prime * data$Haste)
model.aov <- aov(data$Helping ~ data$Prime * data$Haste)
summary(model.aov)

# Q5
TukeyHSD(model.aov)

# Q6, Q7
etaSquared(model.aov,anova=T)

# Q8
PH1 <- subset(data, data$Haste == 1)
PH2 <- subset(data, data$Haste == 2)
PH3 <- subset(data, data$Haste == 3)

aov.PH1 <- aov(PH1$Helping ~ PH1$Prime)
aov.PH2 <- aov(PH2$Helping ~ PH2$Prime)
aov.PH3 <- aov(PH3$Helping ~ PH3$Prime)

summary(aov.PH1)
summary(aov.PH2)
summary(aov.PH3)

# Q9
etaSquared(aov.PH1)