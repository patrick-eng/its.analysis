##### British immigration opinions #####
library(ggplot2)

t.dat <- read.csv("/Users/patrickenglish/Documents/ITS 2019/publicopinioneexample_2.csv")

head(t.dat)

t.dat$public_opinion <- t.dat$public_opinion/100

## plot interruption graph

g <- ggplot(data=t.dat, aes(y=immigration, x=year))

(graph <- g + geom_line())

(graph <- graph + geom_segment(x=1997, y=100, xend=1997, yend=700, color="red") + 
    geom_segment(x=2004, y=100, xend=2004, yend=700, color="red") + theme_bw()) 



## run model (proper)
t.dat$public_opinion2 <- scale(t.dat$public_opinion)


model <- itsa.model(data=t.dat, time="year", depvar="public_opinion2", interrupt_var = "interruption")


itsa.postest(model)


## run model (two-way)

t.dat$interruption_2 <- ifelse(t.dat$year < 1997, 0, 1)

model <- itsa.model(data=t.dat, time="year", depvar="public_opinion", interrupt_var = "interruption_2")

itsa.postest(model)





## Check interaction - not significant, original model applies
t.dat$lag_y <-c(NA, t.dat$public_opinion[1:(length(t.dat$public_opinion)-1)])

t.dat$interact <- t.dat$interruption * t.dat$lag_y 

model <- itsa.model(data=t.dat, time="year", depvar="public_opinion", interrupt_var = "interruption_2", covariates = "interact")
