
pd <- read.dta("pollution.dta")

#Scater Plot Mortality vs. Nitric Oxide 
# plot(pd$nox, pd$mort, pch = 20)
qt <- quantile(pd$nox, probs= c(.95))
qb <- quantile(pd$nox, probs= c(.05))

#remove outliers
f.nox1 <- ifelse(pd$nox<qt,pd$nox, NA)
f.nox <- ifelse(f.nox1>qb,f.nox1, NA)

#simple model
fit1 <- lm(pd$mort~f.nox, data = pd)
# display(fit1)
# plot(f.nox, pd$mort, xlab ="Relative NOX Potential", ylab= "Age Adj. Mortality Rate per 100k", pch = 20)
# curve (coef(fit1)[1]+coef(fit1)[2]*x, add=TRUE)

#setting f.nox in data frame and using it for predict interval
x.new <- data.frame(f.nox=200)
pred.interval <- predict (fit1, x.new, interval="prediction", level=.95)
print(pred.interval)

#rep uncertanty in coefficients: 
n.sims <- 100
sim1 <- sim (fit1, n.sims)
height.coef <- sim1@coef[,2]
print("model 1:")
print(mean (height.coef))
print(sd (height.coef))
print(quantile (height.coef, c(.025, .975)))

#Log model
print("LOG Model:")
log.mort <- log(pd$mort)
noxmortmodel.log <- lm(log.mort~f.nox)
display(noxmortmodel.log)


#setting f.nox in data frame and using it for predict interval
x.new.log <- data.frame(f.nox=200)
pred.interval.log <- predict (noxmortmodel.log, x.new.log, interval="prediction", level=.95)
print(exp(pred.interval.log))

sim.log <- sim (noxmortmodel.log, n.sims)
# print(sim.log)
height.coef.log <- sim.log@coef[,2]
print(mean (height.coef.log))
print(sd (height.coef))
print(quantile ((height.coef.log), c(.025, .975)))
print(height.coef.log)
hist(height.coef.log)
b_mean <- mean(height.coef.log)
b_sd <- sd(height.coef)
curve(dnorm(x, mean=b_mean, sd=b_sd), add= TRUE)
