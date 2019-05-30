### Testing the reliability of its.analysis model in small sample scenarios.

library(its.analysis)

set.seed(12345)


#### 15 Observations ####

# 15 obs, no covariates

results_cov_15_0 <- matrix(nrow = 1000,ncol=4)


for(i in 1:1000){
  
  y <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                            ma=(round(runif(1, 0.1,0.99), digits=2)), 
                            order =c(1,0,1)), n=15, mean=2.5)
  
  a <- seq(0,0, length=7)
  b <- seq(1,1, length=8)
  
  x <- c(a, b)
  
  t <- (0:14)
  
  test.data <- as.data.frame(cbind(y, x, t))
  
  itsa.model(data=test.data, depvar ="y", time="t", interrupt_var = "x", no.plots = TRUE)
  
  results_cov_15_0[i,1] <- itsa.fit$itsa.result
  results_cov_15_0[i,2] <- itsa.fit$aov.result$`Pr(>F)`[1]
  results_cov_15_0[i,3] <- itsa.fit$post_sums
  results_cov_15_0[i,4] <- itsa.fit$adjr_sq
}

  table(results_cov_15_0[,1])
  table(results_cov_15_0[,3])
  summary(as.numeric(results_cov_15_0[,2]))
  summary(as.numeric(results_cov_15_0[,4]))
  
  table(results_cov_15_0[,1]=="Significant variation between time periods with chosen alpha" &
          results_cov_15_0[,3]!="Post-Est Warning")
  
  
  
  # 15 obs, one covariate
  
  results_cov_15_1 <- matrix(nrow = 1000,ncol=4)

  
  
  for(i in 1:1000){
    
    y <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                              ma=(round(runif(1, 0.1,0.99), digits=2)), 
                              order =c(1,0,1)), n=15, mean=2.5)
    
    a <- seq(0,0, length=7)
    b <- seq(1,1, length=8)
    
    x <- c(a, b)
    
    t <- (0:14)
    
    
    c <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                              ma=(round(runif(1, 0.1,0.99), digits=2)), 
                              order =c(1,0,1)), n=15, mean=2.5)
    
    test.data <- as.data.frame(cbind(y, x, t, c))
    
    itsa.model(data=test.data, depvar ="y", time="t", interrupt_var = "x", covariates = "c", no.plots = TRUE)
    
    results_cov_15_1[i,1] <- itsa.fit$itsa.result
    results_cov_15_1[i,2] <- itsa.fit$aov.result$`Pr(>F)`[1]
    results_cov_15_1[i,3] <- itsa.fit$post_sums
    results_cov_15_1[i,4] <- itsa.fit$adjr_sq
  }
  
  table(results_cov_15_1[,1])
  table(results_cov_15_1[,3])
  summary(as.numeric(results_cov_15_1[,2]))
  summary(as.numeric(results_cov_15_1[,4]))
  
  table(results_cov_15_1[,1]=="Significant variation between time periods with chosen alpha" &
          results_cov_15_1[,3]!="Post-Est Warning")
  
  
  # 15 obs, two covariates
  
  results_cov_15_2 <- matrix(nrow = 1000,ncol=4)
  
  
  for(i in 1:1000){
    
    y <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                              ma=(round(runif(1, 0.1,0.99), digits=2)), 
                              order =c(1,0,1)), n=15, mean=2.5)
    
    a <- seq(0,0, length=7)
    b <- seq(1,1, length=8)
    
    x <- c(a, b)
    
    t <- (0:14)
    
    
    c1 <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                              ma=(round(runif(1, 0.1,0.99), digits=2)), 
                              order =c(1,0,1)), n=15, mean=2.5)
    
    c2 <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                              ma=(round(runif(1, 0.1,0.99), digits=2)), 
                              order =c(1,0,1)), n=15, mean=2.5)
    
    test.data <- as.data.frame(cbind(y, x, t, c1, c2))
    
    itsa.model(data=test.data, depvar ="y", time="t", interrupt_var = "x", covariates = c("c1", "c2"), no.plots = TRUE)
    
    results_cov_15_2[i,1] <- itsa.fit$itsa.result
    results_cov_15_2[i,2] <- itsa.fit$aov.result$`Pr(>F)`[1]
    results_cov_15_2[i,3] <- itsa.fit$post_sums
    results_cov_15_2[i,4] <- itsa.fit$adjr_sq
  }
  
  table(results_cov_15_2[,1])
  table(results_cov_15_2[,3])
  summary(as.numeric(results_cov_15_2[,2]))
  summary(as.numeric(results_cov_15_2[,4]))
  
  table(results_cov_15_2[,1]=="Significant variation between time periods with chosen alpha" &
          results_cov_15_2[,3]!="Post-Est Warning")
  
  
  
  # 15 obs, three covariates
  
  results_cov_15_3 <- matrix(nrow = 1000,ncol=4)
  
  
  for(i in 1:1000){
    
    y <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                              ma=(round(runif(1, 0.1,0.99), digits=2)), 
                              order =c(1,0,1)), n=15, mean=2.5)
    
    a <- seq(0,0, length=7)
    b <- seq(1,1, length=8)
    
    x <- c(a, b)
    
    t <- (0:14)
    
    
    c1 <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                               ma=(round(runif(1, 0.1,0.99), digits=2)), 
                               order =c(1,0,1)), n=15, mean=2.5)
    
    c2 <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                               ma=(round(runif(1, 0.1,0.99), digits=2)), 
                               order =c(1,0,1)), n=15, mean=2.5)
    
    c3 <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                               ma=(round(runif(1, 0.1,0.99), digits=2)), 
                               order =c(1,0,1)), n=15, mean=2.5)
    
    test.data <- as.data.frame(cbind(y, x, t, c1, c2, c3))
    
    itsa.model(data=test.data, depvar ="y", time="t", interrupt_var = "x", covariates = c("c1", "c2", "c3"), no.plots = TRUE)
    
    results_cov_15_3[i,1] <- itsa.fit$itsa.result
    results_cov_15_3[i,2] <- itsa.fit$aov.result$`Pr(>F)`[1]
    results_cov_15_3[i,3] <- itsa.fit$post_sums
    results_cov_15_3[i,4] <- itsa.fit$adjr_sq
  }
  
  table(results_cov_15_3[,1])
  table(results_cov_15_3[,3])
  summary(as.numeric(results_cov_15_3[,2]))
  summary(as.numeric(results_cov_15_3[,4]))
  
  table(results_cov_15_3[,1]=="Significant variation between time periods with chosen alpha" &
          results_cov_15_3[,3]!="Post-Est Warning")
  
  
  
#### 21 Observations ##### 


# 21 Observations, 0 covariates

results_cov_21_0 <- matrix(nrow = 1000,ncol=4)

set.seed(12345)


for(i in 1:1000){
  
  y <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                            ma=(round(runif(1, 0.1,0.99), digits=2)), 
                            order =c(1,0,1)), n=21, mean=2.5)
  
  choice <- round(runif(1,1,2), digits=0)
  
  if(choice==1){
    
    a <- seq(0,0, length=7)
    b <- seq(1,1, length=7)
    c <- seq(3,3, length=7)
    
    x <- c(a,b,c)
    
  }
  
  if(choice==2){
    a <- seq(0,0, length=11)
    b <- seq(1,1, length=10)
    
    x <- c(a, b)
  }
  
  t <- (0:20)

  
  test.data <- as.data.frame(cbind(y, x, t))
  
  itsa.model(data=test.data, depvar ="y", time="t", interrupt_var = "x", no.plots = TRUE)
  
  results_cov_21_0[i,1] <- itsa.fit$itsa.result
  results_cov_21_0[i,2] <- itsa.fit$aov.result$`Pr(>F)`[1]
  results_cov_21_0[i,3] <- itsa.fit$post_sums
  results_cov_21_0[i,4] <- itsa.fit$adjr_sq
  
}

table(results_cov_21_0[,1])
table(results_cov_21_0[,3])
summary(as.numeric(results_cov_21_0[,2]))
summary(as.numeric(results_cov_21_0[,4]))

table(results_cov_21_0[,1]=="Significant variation between time periods with chosen alpha" &
        results_cov_21_0[,3]!="Post-Est Warning")






# 20 Observations, 1 covariate

results_cov_21_1 <- matrix(nrow = 1000,ncol=4)

# Runs sims
for(i in 1:1000){
  
  y <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                            ma=(round(runif(1, 0.1,0.99), digits=2)), 
                            order =c(1,0,1)), n=21, mean=2.5)
  
  choice <- round(runif(1,1,2), digits=0)
  
  if(choice==1){
    
    a <- seq(0,0, length=7)
    b <- seq(1,1, length=7)
    c <- seq(3,3, length=7)
    
    x <- c(a,b,c)
    
  }
  
  if(choice==2){
    a <- seq(0,0, length=11)
    b <- seq(1,1, length=10)
    
    x <- c(a, b)
  }
  
  t <- (0:20)
  
  
  c <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                            ma=(round(runif(1, 0.1,0.99), digits=2)), 
                            order =c(1,0,1)), n=21, mean=2.5)
  
  test.data <- as.data.frame(cbind(y, x, t, c))
  
  itsa.model(data=test.data, depvar ="y", time="t", interrupt_var = "x", covariates= "c", no.plots = TRUE)
  
  results_cov_21_1[i,1] <- itsa.fit$itsa.result
  results_cov_21_1[i,2] <- itsa.fit$aov.result$`Pr(>F)`[1]
  results_cov_21_1[i,3] <- itsa.fit$post_sums
  results_cov_21_1[i,4] <- itsa.fit$adjr_sq
  
}

table(results_cov_21_1[,1])
table(results_cov_21_1[,3])
summary(as.numeric(results_cov_21_1[,2]))
summary(as.numeric(results_cov_21_1[,4]))

table(results_cov_21_1[,1]=="Significant variation between time periods with chosen alpha" &
        results_cov_21_1[,3]!="Post-Est Warning")


# 21 Observations, 2 covariates

results_cov_21_2 <- matrix(nrow = 1000,ncol=4)


# Runs sims
for(i in 1:1000){
  
  y <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                            ma=(round(runif(1, 0.1,0.99), digits=2)), 
                            order =c(1,0,1)), n=21, mean=2.5)
  
  choice <- round(runif(1,1,2), digits=0)
  
  if(choice==1){
    
    a <- seq(0,0, length=7)
    b <- seq(1,1, length=7)
    c <- seq(3,3, length=7)
    
    x <- c(a,b,c)
    
  }
  
  if(choice==2){
    a <- seq(0,0, length=11)
    b <- seq(1,1, length=10)
    
    x <- c(a, b)
  }
  
  t <- (0:20)
  
  
  c <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                            ma=(round(runif(1, 0.1,0.99), digits=2)), 
                            order =c(1,0,1)), n=21, mean=2.5)
  
  c2 <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                            ma=(round(runif(1, 0.1,0.99), digits=2)), 
                            order =c(1,0,1)), n=21, mean=2.5)
  
  test.data <- as.data.frame(cbind(y, x, t, c, c2))
  
  itsa.model(data=test.data, depvar ="y", time="t", interrupt_var = "x", covariates= c("c", "c2"), no.plots = TRUE)
  
  results_cov_21_2[i,1] <- itsa.fit$itsa.result
  results_cov_21_2[i,2] <- itsa.fit$aov.result$`Pr(>F)`[1]
  results_cov_21_2[i,3] <- itsa.fit$post_sums
  results_cov_21_2[i,4] <- itsa.fit$adjr_sq
  
}

table(results_cov_21_2[,1])
table(results_cov_21_2[,3])
summary(as.numeric(results_cov_21_2[,2]))
summary(as.numeric(results_cov_21_2[,4]))

table(results_cov_21_2[,1]=="Significant variation between time periods with chosen alpha" &
        results_cov_21_2[,3]!="Post-Est Warning")


# 21 Observations, 3 covariates

results_cov_21_3 <- matrix(nrow = 1000,ncol=4)


# Runs sims
for(i in 1:1000){
  
  y <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                            ma=(round(runif(1, 0.1,0.99), digits=2)), 
                            order =c(1,0,1)), n=21, mean=2.5)
  
  choice <- round(runif(1,1,2), digits=0)
  
  if(choice==1){
    
    a <- seq(0,0, length=7)
    b <- seq(1,1, length=7)
    c <- seq(3,3, length=7)
    
    x <- c(a,b,c)
    
  }
  
  if(choice==2){
    a <- seq(0,0, length=11)
    b <- seq(1,1, length=10)
    
    x <- c(a, b)
  }
  
  t <- (0:20)
  
  
  c <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                            ma=(round(runif(1, 0.1,0.99), digits=2)), 
                            order =c(1,0,1)), n=21, mean=2.5)
  
  c2 <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                             ma=(round(runif(1, 0.1,0.99), digits=2)), 
                             order =c(1,0,1)), n=21, mean=2.5)
  
  
  c3 <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                             ma=(round(runif(1, 0.1,0.99), digits=2)), 
                             order =c(1,0,1)), n=21, mean=2.5)
  
  test.data <- as.data.frame(cbind(y, x, t, c,  c2))
  
  itsa.model(data=test.data, depvar ="y", time="t", interrupt_var = "x", covariates= "c", no.plots = TRUE)
  
  results_cov_21_3[i,1] <- itsa.fit$itsa.result
  results_cov_21_3[i,2] <- itsa.fit$aov.result$`Pr(>F)`[1]
  results_cov_21_3[i,3] <- itsa.fit$post_sums
  results_cov_21_3[i,4] <- itsa.fit$adjr_sq
  
}

table(results_cov_21_3[,1])
table(results_cov_21_3[,3])
summary(as.numeric(results_cov_21_3[,2]))
summary(as.numeric(results_cov_21_3[,4]))

table(results_cov_21_3[,1]=="Significant variation between time periods with chosen alpha" &
        results_cov_21_3[,3]!="Post-Est Warning")
  



# 15 obs, three covariates, alpha 0.01

results_cov_15_3 <- matrix(nrow = 1000,ncol=4)


for(i in 1:1000){
  
  y <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                            ma=(round(runif(1, 0.1,0.99), digits=2)), 
                            order =c(1,0,1)), n=15, mean=2.5)
  
  a <- seq(0,0, length=7)
  b <- seq(1,1, length=8)
  
  x <- c(a, b)
  
  t <- (0:14)
  
  
  c1 <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                             ma=(round(runif(1, 0.1,0.99), digits=2)), 
                             order =c(1,0,1)), n=15, mean=2.5)
  
  c2 <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                             ma=(round(runif(1, 0.1,0.99), digits=2)), 
                             order =c(1,0,1)), n=15, mean=2.5)
  
  c3 <- arima.sim(model=list(ar=(round(runif(1, 0.1,0.99), digits=2)), 
                             ma=(round(runif(1, 0.1,0.99), digits=2)), 
                             order =c(1,0,1)), n=15, mean=2.5)
  
  test.data <- as.data.frame(cbind(y, x, t, c1, c2, c3))
  
  itsa.model(data=test.data, depvar ="y", time="t", interrupt_var = "x",
             covariates = c("c1", "c2", "c3"), no.plots = TRUE, alpha = 0.01)
  
  results_cov_15_3[i,1] <- itsa.fit$itsa.result
  results_cov_15_3[i,2] <- itsa.fit$aov.result$`Pr(>F)`[1]
  results_cov_15_3[i,3] <- itsa.fit$post_sums
  results_cov_15_3[i,4] <- itsa.fit$adjr_sq
}

table(results_cov_15_3[,1])
table(results_cov_15_3[,3])
summary(as.numeric(results_cov_15_3[,2]))
summary(as.numeric(results_cov_15_3[,4]))

table(results_cov_15_3[,1]=="Significant variation between time periods with chosen alpha" &
        results_cov_15_3[,3]!="Post-Est Warning")



  