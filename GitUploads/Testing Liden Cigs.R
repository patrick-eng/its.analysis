#### TESTING LIDEN DATA #####
library(its.analysis)


data <- read.csv("cigsales.csv")

head(data)

cal <- subset(data, state=="California")

cal$policy <- ifelse(cal$year < 1989, 0, 1)

cal$time <- 0:30

## run its.analysis model - result is no significant difference

itsa.model(data=cal, time="year", depvar="cigsale", interrupt_var="policy")



## Demonstrate with plot

library(ggplot2)

# Full data
g <- ggplot(data=cal, aes(y=cigsale, x=year))

(graph <- g + geom_line())

(graph <- graph + geom_smooth(method="lm"))

(graph <- graph + geom_segment(x=1989, y=40, xend=1989, yend=150, color="red"))


# Cut to trend

cal_short <- subset(cal, cal$year > 1975)

g <- ggplot(data=cal_short, aes(y=cigsale, x=year))

(graph <- g + geom_line())

(graph <- graph + geom_smooth(method="lm"))

(graph <- graph + geom_segment(x=1989, y=20, xend=1989, yend=150, color="red") + theme_bw())
