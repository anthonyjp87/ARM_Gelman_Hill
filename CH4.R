#Notes on Chapter 4: 

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

resid <- fit1$residuals
sd.resid <- sd(resid)
f.nox.rmna <- na.omit(f.nox)
plot (f.nox.rmna, resid, ylab="Residuals", pch=20)
abline (sd.resid,0,lty=2)
abline(0,0)
abline (-sd.resid,0,lty=2)

#log transformations
log.mort<- log(pd$mort)
noxmortmodel.log <- lm(log.mort~f.nox)
print(noxmortmodel.log)
plot(f.nox, log.mort, main ="log Mort", xlab ="Relative NOX Potential", ylab= "Age Adj. Mortality Rate per 100k", pch = 20)
curve (coef(noxmortmodel.log)[1]+coef(noxmortmodel.log)[2]*x, add=TRUE)

#double log transformation
log.f.nox <- log(f.nox)
plot(log.f.nox)
plot(f.nox)
noxmortmodel.dlog <- lm(log.mort~log.f.nox)
display(noxmortmodel.dlog)
plot(log.f.nox, log.mort, main ="D.log Mort", xlab ="log NOX Potential", ylab= "log mortality", pch = 20)
curve (coef(noxmortmodel.dlog)[1]+coef(noxmortmodel.dlog)[2]*x, add=TRUE)

#double log transformation residuals plot
resid <- noxmortmodel.dlog$residuals
sd.resid <- sd(resid)
log.f.nox.rmna <- na.omit(log.f.nox)
plot (log.f.nox.rmna, resid, ylab="Residuals", pch=20)
abline (sd.resid,0,lty=2)
abline(0,0)
abline (-sd.resid,0,lty=2)

#another log transformations--can I log parameter w/o log dependent var?
noxmortmodel.log.2<- lm(pd$mort~log.f.nox)
display(noxmortmodel.log.2)
plot(log.f.nox, pd$mort, main ="log NOX", xlab ="Relative LOG NOX Potential", ylab= "Age Adj. Mortality Rate per 100k", pch = 20)
curve (coef(noxmortmodel.log.2)[1]+coef(noxmortmodel.log.2)[2]*x, add=TRUE)

#Z transform
z.nox <- (f.nox-mean(f.nox, na.rm=TRUE)/sd(f.nox,na.rm=TRUE))
noxmort.log.z <- lm(log.mort ~ z.nox)
print(noxmort.log.z)
plot(z.nox,log.mort)

#clean HC
qt <- quantile(pd$hc, probs= c(.95))
qb <- quantile(pd$hc, probs= c(.05))

hc1.f <- ifelse(pd$hc<qt,pd$nox, NA)
hc.f <- ifelse(hc1.f>qb,hc1.f, NA)
hc.f.log<-log(hc.f)
z.hc.f.log <-(hc.f.log-mean(hc.f.log, na.rm=TRUE)/sd(hc.f.log,na.rm = TRUE))

#LM w/HC
fit.hc.nox <- lm(pd$mort~ pd$hc+ pd$nox)
display(fit.hc.nox)

fit.hc.nox.logmort <- lm(log.mort~ hc.f.log+ log.f.nox)
display(fit.hc.nox.logmort)

# PREC   Average annual precipitation in inches
# JANT   Average January temperature in degrees F
# JULT   Same for July
# OVR65  % of 1960 SMSA population aged 65 or older
# POPN   Average household size
# EDUC   Median school years completed by those over 22
# HOUS   % of housing units which are sound & with all facilities
# DENS   Population per sq. mile in urbanized areas, 1960
# NONW   % non-white population in urbanized areas, 1960
# WWDRK  % employed in white collar occupations
# POOR   % of families with income < $3000
# HC     Relative hydrocarbon pollution potential
# NOX    Same for nitric oxides
# SO@    Same for sulphur dioxide
# HUMID  Annual average % relative humidity at 1pm
# MORT   Total age-adjusted mortality rate per 100,000
