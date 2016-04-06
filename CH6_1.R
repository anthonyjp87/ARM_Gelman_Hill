
rb <- read.dta("risky_behaviors.dta")

hiv <- ifelse(rb$bs_hiv=="negative", 0, 1)

n<-434
k<-2

#Model this outcome as a function of treatment assignment using a Poisson regression. Does the model fit well? Is there evidence of overdispersion?
fit.1 <- glm (bupacts~women_alone+couples, data=rb, family=poisson)
fit.1.qp <- glm (bupacts~women_alone+couples, data=rb, family=quasipoisson)
fit.2 <- glm (bupacts~couples, data=rb, family=poisson)

display(fit.1)
yhat <- predict (fit.1, type="response")
z <- (rb$bupacts-yhat)/sqrt(yhat)
cat ("overdispersion ratio is ", sum(z^2)/(n-k), "\n")
cat ("p-value of overdispersion test is ", pchisq (sum(z^2), n-k), "\n")
# 
# display(fit.1.od)
# yhat <- predict (fit.1.qp, type="response")
# z <- (rb$bupacts-yhat)/sqrt(yhat)
# cat ("overdispersion ratio is ", sum(z^2)/(n-k), "\n")
# cat ("p-value of overdispersion test is ", pchisq (sum(z^2), n-k), "\n")

#Next extend the model to include pre-treatment measures of the outcome and the additional pre-treatment variables included in the dataset. Does the model fit well? Is there evidence of overdispersion?


#Fit an overdispersed Poisson model. What do you conclude regarding effec- tiveness of the intervention?

