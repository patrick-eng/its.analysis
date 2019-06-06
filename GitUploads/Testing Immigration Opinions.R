#### Testing the model on immigration data ####
library(ggplot2)
library(its.analysis)


## Read in
t.dat <- read.csv("/Users/patrickenglish/Documents/ITS 2019/public-opinion-example.csv")

head(t.dat)

t.dat$public_opinion2 <- scale(t.dat$public_opinion)

### Plot the interruptions
g <- ggplot(data=t.dat, aes(y=immigration, x=year))

(graph <- g + geom_line() +
    xlab("") + ylab("Immigration (Thousands)") +
    scale_x_continuous(breaks=seq(min(1980), max(2020), by=5)) + theme_bw())

(graph <- graph + geom_segment(x=1997, y=100, xend=1997, yend=700, color="red") +
    geom_segment(x=2006, y=100, xend=2006, yend=700, color="red"))



## Run model (proper)


model <- itsa.model(data=t.dat, time="year", depvar="public_opinion2",
                    interrupt_var = "interruption")

itsa.postest(model)



## run model (two-way)

t.dat$interruption_2 <- ifelse(t.dat$year < 1997, 0, 1)

model <- itsa.model(data=t.dat, time="year", depvar="public_opinion", interrupt_var = "interruption_2")

itsa.postest(model)



## Check interaction - not significant, original model applies
t.dat$lag_y <-c(NA, t.dat$public_opinion[1:(length(t.dat$public_opinion)-1)])

t.dat$interact <- t.dat$interruption * t.dat$lag_y

model <- itsa.model(data=t.dat, time="year", depvar="public_opinion", interrupt_var = "interruption_2", covariates = "interact")

