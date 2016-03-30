
pd <- read.dta("pollution.dta")

#Scater Plot Mortality vs. Nitric Oxide 
plot(pd$nox, pd$mort, pch = 20)
qt <- quantile(pd$nox, probs= c(.95))
qb <- quantile(pd$nox, probs= c(.05))

f.nox1 <- ifelse(pd$nox<qt,pd$nox, NA)
f.nox <- ifelse(f.nox1>qb,f.nox1, NA)

#simple model
fit1 <- lm(pd$mort~f.nox, data = pd)
display(fit1)
plot(f.nox, pd$mort, xlab ="Relative NOX Potential", ylab= "Age Adj. Mortality Rate per 100k", pch = 20)
curve (coef(fit1)[1]+coef(fit1)[2]*x, add=TRUE)

xnew <- data.frame(f.nox=12)

