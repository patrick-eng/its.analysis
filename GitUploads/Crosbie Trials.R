
#### TESTING CROSBIE ####

library(its.analysis)


# 30 Observations

results_crosbie <- matrix(nrow = 10000,ncol=4)

set.seed(12345)

# Generate equal-length vector

a <- seq(0,0, length=15)
b <- seq(1,1, length=15)

x <- c(a, b)

for(i in 1:10000){
  
  y <- arima.sim(list(order = c(1,0,0), ar=0.8), n=30, mean=2.5)
  
  x <- x
  
  t <- (0:29)
  
  test.data <- as.data.frame(cbind(y, x, t))
  
  itsa.model(data=test.data, depvar ="y", time="t", interrupt_var = "x")
  
  results_crosbie[i,1] <- itsa.fit$itsa.result
  results_crosbie[i,2] <- itsa.fit$aov.result$`Pr(>F)`[1]
  results_crosbie[i,3] <- itsa.fit$post_sums
  results_crosbie[i,4] <- itsa.fit$adjr_sq
  
}

table(results_crosbie[,1])
table(results_crosbie[,3])
summary(as.numeric(results_crosbie[,2]))
summary(as.numeric(results_crosbie[,4]))

table(results_crosbie[,1]=="Significant variation between time periods with chosen alpha" & results_crosbie[,3]=="No Post-Est Warning")


