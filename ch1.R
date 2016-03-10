y <- c(35,34,38,35,37)
y <- rep(c(0,1), c(300,700))
y <- rep(c(0,1,2,3,4), c(600,300,50,30,20))

n <- length(y)
estimate <- mean(y)
se <- sd(y)/sqrt(n)
int.95 <- estimate + qt(c(.025,.975), n-1)*se
int.50 <- estimate + qt(c(.25,.75), n-1)*se


#Confidence Intervals for Ratios

# out of 1000, .75 of 500 men support, .65 of 500 women support: men support 15% more than women
#What is the Confidence interval on the ratio? 
# se men:
n.men <- 500
p.hat.men <- 0.75
se.men <- sqrt(p.hat.men*(1-p.hat.men)/n.men)

#se women:
n.women <- 500
p.hat.women <- 0.65
se.women <- sqrt(p.hat.women*(1-p.hat.women)/n.women)

#sims
n.sims <- 10000
#simulate 10,000 p.hat.men 
p.men <- rnorm(n.sims, p.hat.men, se.men)
#simulate 10,000 p.hat.women
p.women <- rnorm(n.sims, p.hat.women, se.women)
#Porportion
ratio <- p.men/p.women
#Quantiles: 
int.95 <- quantile (ratio, c(.025,.975))


#Exercise 3: 20 uniform (0,1)  dunif(x, min=0, max=1, log = FALSE)
runif(20, min=0, max=1)

