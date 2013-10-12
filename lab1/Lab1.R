# Q1
install.packages("car")

# Q2
library(car)

# Q3
search()

# Q4
?data.frame

# Q5
v1 = c(1:10)
v2 = c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten")
nw = data.frame(numbers=v1, words=v2)
attributes(nw)

# Q6
nw[8,'numbers']
nw[8,1]

# Q7
nw[8,'words']
nw[8,2]

# Q8
a=matrix(1:10,2,5)

# Q9
x=c(1:6)
y=c(5:10)
rbind(x,y)

# Q10
cbind(x,y)