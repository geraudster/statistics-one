#install.packages('reshape')
#install.packages('lsr')

library(psych)
library(car)
library(lsr)
library(ggplot2)
library(reshape)
data <- read.table('Stats1.13.HW.02.txt', header=T)

# Q1
describeBy(data, data$time)

data.pre <- subset(data, data$time == 'pre')
data.post <- subset(data, data$time == 'post')

t.test(data.post$SR, data.pre$SR, paired=T)

# Q2
data.DS <- subset(data, data$condition == 'DS')
data.PE <- subset(data, data$condition == 'PE')
data.WM <- subset(data, data$condition == 'WM')

describeBy(data.DS, data.DS$time)
describeBy(data.PE, data.PE$time)

# Boxplot
ggplot(data, aes(x=condition, y=SR, color=time)) + 
  geom_boxplot() +
  guides(fill=FALSE) 

data.PE.pre <- subset(data.PE, data.PE$time == 'pre')
data.PE.post <- subset(data.PE, data.PE$time == 'post')

t.test(data.PE.post$SR, data.PE.pre$SR, paired = T)

# Q3
data.DS.pre <- subset(data.DS, data.DS$time == 'pre')
data.DS.post <- subset(data.DS, data.DS$time == 'post')

data.WM.pre <- subset(data.WM, data.WM$time == 'pre')
data.WM.post <- subset(data.WM, data.WM$time == 'post')


cohensD(data.PE.post$SR, data.PE.pre$SR, method = 'paired')
cohensD(data.DS.post$SR, data.DS.pre$SR, method = 'paired')
cohensD(data.WM.post$SR, data.WM.pre$SR, method = 'paired')

# Q4

data.reshape <- cast(data, subject+condition~time,value='SR')
data.reshape$gain <- data.reshape$post - data.reshape$pre

data.reshape.DS <- subset(data.reshape, data.reshape$condition == 'DS')
data.reshape.WM <- subset(data.reshape, data.reshape$condition == 'WM')
data.reshape.PE <- subset(data.reshape, data.reshape$condition == 'PE')

t.test(data.reshape.DS$gain, data.reshape.WM$gain, var.equal = T)

t.test(data.reshape.WM$gain, data.reshape.PE$gain, var.equal = T)

t.test(data.reshape.PE$gain, data.reshape.DS$gain, var.equal = T)

# Q5
leveneTest(data.reshape$gain ~ data.reshape$condition)

# Q6
aov.model <- aov(data.reshape$gain ~ data.reshape$condition)
summary(aov.model)

# Q7
etaSquared(aov.model, anova=T)

# Q8
TukeyHSD(aov.model)