
rb <- read.dta("risky_behaviors.dta")

hiv <- ifelse(rb$bs_hiv=="negative", 0, 1)

n<-434
k<-2

fit.1 <- glm (bupacts~women_alone+couples, data=rb, family=poisson)
fit.1.qp <- glm (bupacts~women_alone+couples, data=rb, family=quasipoisson)
fit.2 <- glm (bupacts~couples, data=rb, family=poisson)

display(fit.1)
yhat <- predict (fit.1, type="response")
z <- (rb$bupacts-yhat)/sqrt(yhat)
cat ("overdispersion ratio is ", sum(z^2)/(n-k), "\n")
cat ("p-value of overdispersion test is ", pchisq (sum(z^2), n-k), "\n")
 
display(fit.1.qp)
yhat <- predict (fit.1.qp, type="response")
z <- (rb$bupacts-yhat)/sqrt(yhat)
cat ("overdispersion ratio is ", sum(z^2)/(n-k), "\n")
cat ("p-value of overdispersion test is ", pchisq (sum(z^2), n-k), "\n")


fit.3 <- glm (bupacts~women_alone+couples+hiv+sex+fupacts, data=rb, family=poisson)
display(fit.3)


fit.4 <- glm (bupacts~women_alone+couples+hiv+sex+fupacts, data=rb, family=quasipoisson)
display(fit.4)

pred <- predict(fit.4, type = "response")
#overlay two histograms? 
#modeled behavior missing large number of 0's... 

