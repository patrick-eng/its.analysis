## Demonstration of lagged dependent variable term impact

library(its.analysis)

a <- seq(0,0, length=8)
b <- seq(1,1, length=9)
x <- c(a,b)

y1 <- c(1.3, 2.1, 3.3, 4.5, 5.4, 6.1, 6.8, 7.5, 8.1, 8.9, 9.6, 10.4, 11.5, 12.4, 13.2, 14.4, 15.6)
y2 <- c(1.1, 2.4, 3.4, 4.6, 5.7, 6.1, 8.2, 9.7, 10.2, 9.7, 10.1, 9.7, 10.1, 10.2, 10.5, 10.2, 10.3)
y3 <- c(1.1, 2.4, 3.4, 4.6, 5.7, 6.1, 8.2, 9.7, 10.2, 9.7, 8.2, 6.1, 5.7, 4.6, 3.4, 2.4, 1.1)

c1 <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)),
                                 ma=(round(runif(1, 0.1,0.99), digits=2)),
                                 order =c(1,0,1)), n=17, mean=2.5)

t <- (0:16)

data1 <- cbind(x, y1, c1, t)
data2 <- cbind(x, y2, c1, t)
data3 <- cbind(x, y3, c1, t)

data1 <- as.data.frame(data1)
data2 <- as.data.frame(data2)
data3 <- as.data.frame(data3)


data1$ly1 <- c(NA, data1$y[1:(length(data1$y)-1)])


### Standard Type II SS Anova

plot(y1, type="l", main="ANOVA Model");segments(x0=9, y0=0, y1=16, col="red", lty=5)

anovaplot <- grDevices::recordPlot()

mod <- aov(lm(y1 ~ x + ly1 + c1, data=data1))

summary(mod)



## Model on data 1
model1 <- itsa.model(data=data1, time="t", depvar="y1", interrupt_var = "x", covariates = "c1", bootstrap = TRUE, no.plots = TRUE)



## Model on data 2
model2 <- itsa.model(data=data2, time="t", depvar="y2", interrupt_var = "x")


## Model on data 3
itsa.model(data=data3, time="t", depvar="y3", interrupt_var = "x")
