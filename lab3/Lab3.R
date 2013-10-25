# Q1
data <- read.table("Stats1.13.HW.03.txt", header=T)
describe(data)

# tableaux commencent à 1
round(cor(data[3:4]),2) # 3 = S1.pre, 4 = S2.pre

# Q2
round(cor(data[7:8]),2) # 7 = V1.pre, 8 = V2.pre

# Q3
data$spatial.pre <- (data$S1.pre + data$S2.pre) / 2
data$visual.pre <- (data$V1.pre + data$V2.pre) / 2
round(cor(data$spatial.pre, data$visual.pre),2)
plot(data$spatial.pre ~ data$visual.pre)
abline(lm(data$spatial.pre ~ data$visual.pre), col = "green")

# Q4
# control group = aer
control <- subset(data, data$cond == "aer")
round(cor(control[3:4]),2)
round(cor(control[7:8]),2)
# V2 ?

# Q5
data$spatial.post <- (data$S1.post + data$S2.post) / 2
data$spatial.improve <- (data$spatial.post - data$spatial.pre)
round(cor(data[11], data[13]),2)

plot(data$spatial.pre ~ data$spatial.improve)
abline(lm(data$spatial.pre ~ data$spatial.improve), col = "green")

#pairs(~data$S1.pre + data$S2.pre + data$S1.post + data$S2.post, cex.labels = 1.2)

# Q6
data$visual.post <- (data$V1.post + data$V2.post) / 2
data$visual.improve <- (data$visual.post - data$visual.pre)
round(cor(data[14], data[16]),2)

plot(data$visual.pre ~ data$visual.improve)
abline(lm(data$visual.pre ~ data$visual.improve), col = "green")

#Q7
describeBy(data$spatial.improve, data$cond)
# des

# Q8
#pairs(~data$S1.pre + data$S2.pre + data$V1.pre + data$V2.pre, cex.labels = 1.2)
base <- data[c(3,4,7,8)]
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r) 
cpairs(base, base.order, panel.colors = base.color, gap = .5,
       main = "Variables Ordered and Colored by Correlation")

# Q9
#pairs(~data$S1.pre + data$S2.pre + data$V1.pre + data$V2.pre, cex.labels = 1.2)
base <- data[c(5,6,9,10)]
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r) 
cpairs(base, base.order, panel.colors = base.color, gap = .5,
       main = "Variables Ordered and Colored by Correlation")