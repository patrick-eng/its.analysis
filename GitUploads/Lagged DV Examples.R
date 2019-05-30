## Demonstration of lagged dependent variable term impact

library(its.analysis)

a <- seq(0,0, length=8)
b <- seq(1,1, length=9)
x <- c(a,b)

y1 <- c(1.3, 2.1, 3.3, 4.5, 5.4, 6.1, 6.8, 7.5, 8.1, 8.9, 9.6, 10.4, 11.5, 12.4, 13.2, 14.4, 15.6)
y2 <- c(1.1, 2.4, 3.4, 4.6, 5.7, 6.1, 8.2, 9.7, 10.2, 9.7, 10.1, 9.7, 10.1, 10.2, 10.5, 10.2, 10.3)
y3 <- c(1.1, 2.4, 3.4, 4.6, 5.7, 6.1, 8.2, 9.7, 10.2, 9.7, 8.2, 6.1, 5.7, 4.6, 3.4, 2.4, 1.1)


t <- (0:16)

data1 <- cbind(x, y1, t)
data2 <- cbind(x, y2, t)
data3 <- cbind(x, y3, t)

data1 <- as.data.frame(data1)
data2 <- as.data.frame(data2)
data3 <- as.data.frame(data3)


### Standard Type II SS Anova

plot(y1, type="l", main="ANOVA Model");segments(x0=9, y0=0, y1=16, col="red", lty=5)

anovaplot <- grDevices::recordPlot()

mod <- aov(lm(y1 ~ x, data=data1))

car::Anova(mod, type=2)


## Model on data 1
itsa.model(data=data1, time="t", depvar="y1", interrupt_var = "x")


## Model on data 2
itsa.model(data=data2, time="t", depvar="y2", interrupt_var = "x")


## Model on data 3
itsa.model(data=data3, time="t", depvar="y3", interrupt_var = "x")



