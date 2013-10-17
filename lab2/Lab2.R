# Load packages
library(psych)
library(sm)

data <- read.table("Stats1.13.HW.02.txt", header= T)
class(data)
names(data)
class(data$SR)
# Q1
dim(data)
nrow(data)

# Q2
# condition

# Q3
describe(data)
mean(data[,4])

# Q4
describe(data)
var(data[,4])

# Q5
describeBy(data, data$time)
pretest <- subset(data, data[,3] == "pre")
mean(pretest[,4])

# Q6
describeBy(data, data$time)
posttest <- subset(data, data[,3] == "post")
sd(posttest[,4])

# Q7
describe(posttest)

# Q8
describeBy(posttest, posttest$condition)

# Q9
par(mfrow= c(2,3))
hist(subset(pretest, pretest[,2] == "WM")[, 4], xlab = "WM group at pretest", main = "" ) 
hist(subset(pretest, pretest[,2] == "PE")[, 4], xlab = "PE group at pretest", main = "" ) 
hist(subset(pretest, pretest[,2] == "DS")[, 4], xlab = "DS group at pretest", main = "" ) 
hist(subset(posttest, posttest[,2] == "WM")[, 4], xlab = "WM group at posttest", main = "" ) 
hist(subset(posttest, posttest[,2] == "PE")[, 4], xlab = "PE group at posttest", main = "" ) 
hist(subset(posttest, posttest[,2] == "DS")[, 4], xlab = "DS group at posttest", main = "" ) 

par(mfrow= c(2,3))
plot(density(subset(pretest, pretest[,2] == "WM")[, 4]), xlab = "WM group at pretest", main = "" ) 
plot(density(subset(pretest, pretest[,2] == "PE")[, 4]), xlab = "PE group at pretest", main = "" ) 
plot(density(subset(pretest, pretest[,2] == "DS")[, 4]), xlab = "DS group at pretest", main = "" ) 
plot(density(subset(posttest, posttest[,2] == "WM")[, 4]), xlab = "WM group at posttest", main = "" ) 
plot(density(subset(posttest, posttest[,2] == "PE")[, 4]), xlab = "PE group at posttest", main = "" ) 
plot(density(subset(posttest, posttest[,2] == "DS")[, 4]), xlab = "DS group at posttest", main = "" ) 

# Q10
par(mfrow = c(1,2))
colfill <- c(2:(2+length(levels(pretest$condition))))
sm.density.compare(pretest$SR, pretest$condition, xlab = "Pretest SR" )
legend(locator(1),levels(posttest$condition), fill=colfill)
sm.density.compare(posttest$SR, 
                   pretest$condition, xlab = "Posttest SR")
legend(locator(1),levels(posttest$condition), fill=colfill)
