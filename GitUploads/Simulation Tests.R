### SIMULATION OF TIME SERIES DATA 

library(stats)
library(forecast)
library(its.analysis)

#### ITSA MODEL ####

set.seed(12345)

## Totally randomised 

results_tr1 <- matrix(nrow = 10000,ncol=4)

for(i in 1:10000){
  
y <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                          ma=(round(runif(1, 0.1,0.99), digits=2)), 
                          order =c(1,0,1)), n=30, mean=2.5)

c <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                          ma=(round(runif(1, 0.1,0.99), digits=2)), 
                          order =c(1,0,1)), n=30, mean=2.5)

x <- round(runif(30, 0,2), digits=0)

t <- (0:29)

test.data <- as.data.frame(cbind(y, x, c, t))

itsa.model(data=test.data, depvar ="y", time="t", interrupt_var = "x", covariates = "c")

results_tr1[i,1] <- itsa.fit$itsa.result
results_tr1[i,2] <- itsa.fit$aov.result$`Pr(>F)`[1]
results_tr1[i,3] <- itsa.fit$post_sums
results_tr1[i,4] <- itsa.fit$adjr_sq

}

table(results_tr1[,1])
table(results_tr1[,3])
summary(as.numeric(results_tr1[,2]))
summary(as.numeric(results_tr1[,4]))




## Random length sequencing 

results_rl <- matrix(nrow = 5000,ncol=4)

set.seed(12345)

for(i in 1:5000){
  
  y <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                            ma=(round(runif(1, 0.1,0.99), digits=2)), 
                            order =c(1,0,1)), n=30, mean=2.5)
  
  ## Randomise length and position of time-periods
  p <- round(runif(1, 7, 15), digits=0)
  d <- round(runif(1, 7, 15), digits=0)
  q <- round(runif(1, 7, 15), digits=0)
  
  lim <- 30/(p + d + q)
  
  p <- p*lim
  d <- d*lim
  q <- q*lim
  
  p <- round(p, digits=0)
  d <- round(d, digits=0)
  q <- round(q, digits=0)
  
  first <- seq(0,0, length=p)
  second <- seq(1,1, length=d)
  third <- seq(2,2, length=q)
  
  x <- c(first, second, third)
  
  if(length(x) > 30){
    x <- x[0:30]
  }
  
  if(length(x) < 30){
    x <- c(x, 2)
  }
  
  t <- (0:29)

  test.data <- as.data.frame(cbind(y, x, t))
  
  itsa.model(data=test.data, depvar ="y", time="t", interrupt_var = "x",
             no.plots = TRUE)
  
  results_rl[i,1] <- itsa.fit$itsa.result
  results_rl[i,2] <- itsa.fit$aov.result$`Pr(>F)`[1]
  results_rl[i,3] <- itsa.fit$post_sums
  results_rl[i,4] <- itsa.fit$adjr_sq
  
}

table(results_rl[,1])
table(results_rl[,3])
summary(as.numeric(results_rl[,2]))
summary(as.numeric(results_rl[,4]))

table(results_rl[,1]=="Significant variation between time periods with chosen alpha" &
        results_rl[,3]!="Post-Est Warning")



table(results_rl[,2]<0.02)

table(results_rl[,2]<0.01 &
        results_rl[,3]!="Post-Est Warning")



## Random DV and IV, autocorrelation estimate test: 95% confidence

results_acf <- matrix(nrow = 5000,ncol=2)

a <- seq(0,0, length=10)
b <- seq(1,1, length=10)
c <- seq(2,2, length=10)

x <- c(a, b, c)

set.seed(12345)

for(i in 1:5000){
  
  y <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                            ma=(round(runif(1, 0.1,0.99), digits=2)), 
                            order =c(1,0,1)), n=30, mean=2.5)
  
  x <- x
  
  t <- (0:29)
  
  test.data <- as.data.frame(cbind(y, x, c, t))
  
  itsa.model(data=test.data, depvar ="y", time="t", interrupt_var = "x")
  
  acf.test <- taperedacf(itsa.fit$residuals, lag.max = 10)
  
  acf.data <- cbind(acf.test$z, acf.test$upper, acf.test$lower)
  
  acf.results <- as.matrix(ifelse((acf.data[,1] < acf.data[,3] | acf.data[,1] > acf.data[,2]), "FAIL", "PASS"))
  
  results_acf[i,1] <- ifelse("FAIL" %in% acf.results[1:3,1], "Autocorrelated", "Not Autocorrelated")
  results_acf[i,2] <- ifelse("FAIL" %in% acf.results[4:10,1], "Autocorrelated", "Not Autocorrelated")
  
  
}

table(results_acf[,1])
table(results_acf[,2])
