

n.sims <-100
fit <- lm(ppvt~momage, data = iq.data)
plot(iq.data$momage, iq.data$ppvt, xlab="Mother's Age", ylab = "Child Test Score", pch=20)
curve (coef(fit)[1]+coef(fit)[2]*x, add=TRUE)
fit.sim <- sim(fit, n.sims)
print(fit.sim)
print(fit.sim$coef)
for (i in 1:10){
  curve(fit.sim$coef[i,1]+fit.sim$coef[i,2]*x, add=TRUE, col="gray")
}


display(fit)
resid <- fit$residuals
sd.resid <- sd(resid)
plot (iq.data$momage, resid, ylab="Residuals", pch=20)
abline (sd.resid,0,lty=2)
abline(0,0)
abline (-sd.resid,0,lty=2)





fit2 <- lm(iq.data$ppvt~iq.data$momage + iq.data$educ_cat)
colors <- as.factor(iq.data$educ_cat[])
plot(iq.data$educ_cat, iq.data$ppvt, xlab="Education", ylab = "Child Test Score", col=colors, pch=20)
#plot(iq.data$educ_cat, iq.data$educ_cat, add=TRUE)
curve (coef(fit2)[1]+coef(fit2)[2]*x, add=TRUE)
display(fit2)

fit3 <- lm(iq.data$ppvt~iq.data$momage + iq.data$educ_cat + iq.data$momage:iq.data$educ_cat)
plot(iq.data$momage, iq.data$ppvt, xlab="Education", ylab = "Child Teest Score", pch=20)
curve (coef(fit3)[1]+coef(fit3)[4]*x, add=TRUE)
display(fit3)
resid3 <- fit3$residuals
sd.resid3 <- sd(resid3)

plot (iq.data$educ_cat, resid3, ylab="Residuals", pch=20)
abline (sd.resid3,0,lty=2)
abline(0,0)
abline (-sd.resid3,0,lty=2)

plot (iq.data$momage, resid3, ylab="Residuals", pch=20)
abline (sd.resid3,0,lty=2)
abline(0,0)
abline (-sd.resid3,0,lty=2)

plot (iq.data$momage*iq.data$educ_cat, resid3, ylab="Residuals", pch=20, col=colors)
abline (sd.resid3,0,lty=2)
abline(0,0)
abline (-sd.resid3,0,lty=2)



iq.data$mom_HS <- ifelse (iq.data$educ_cat<3, 0, 1)
fit4 <- lm(iq.data$ppvt~iq.data$mom_HS + iq.data$momage)
colors <- ifelse (iq.data$mom_HS==1, "black", "gray")
plot(iq.data$momage, iq.data$ppvt, main= "Black = HS, Grey = No HS", xlab="Mother Age", ylab="Child test score", col=colors, pch=20)
curve (cbind (1, 1, x) %*% coef(fit4), add=TRUE, col="black")
curve (cbind (1, 0, x) %*% coef(fit4), add=TRUE, col="gray")
display(fit4)

scores.new <- fit4$coef[1] + fit4$coef[2]*iq.data$mom_HS + fit4$coef[3]*iq.data$momage
res <- (iq.data$ppvt - scores.new)
a <- sqrt(var(res))
plot(scores.new,res,pch=20)
abline(h=0)
abline(h=a, lty=2)
abline(h=-a, lty=2)


iq.data$mom_HS <- ifelse (iq.data$educ_cat<3, 0, 1)
fit4 <- lm(ppvt~mom_HS + momage, data=iq.data)
colors <- ifelse (iq.data$mom_HS==1, "black", "gray")
plot(iq.data$momage, iq.data$ppvt, main= "Black = HS, Grey = No HS", xlab="Mother Age", ylab="Child test score", col=colors, pch=20)
curve (cbind (1, 1, x) %*% coef(fit4), add=TRUE, col="black")
curve (cbind (1, 0, x) %*% coef(fit4), add=TRUE, col="gray")
display(fit4)
momage.mean <- mean(iq.data$momage)
momage.sd <- sd(iq.data$momage)

mom_HS.rate<-sum(mom_HS)/400
print(mom_HS.rate)
mom_HS <- rbinom(400, 1, mom_HS.rate)
momage <- rnorm(400, mean=momage.mean, sd=momage.sd)
x4.new <- data.frame(mom_HS,momage)
x4.predict <- predict(fit4, x4.new, level = 0.95)
#print(x4.predict)

plot(momage,x4.predict)


