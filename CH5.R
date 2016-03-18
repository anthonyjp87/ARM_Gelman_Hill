#rodents

r <- read.table("rodents.dat")
fit.1 <- glm(rodent2~dilap, family=binomial, data=r)
display(fit.1)
plot(jitter(dilap, factor=.5), jitter(rodent2, factor=.5), pch=20)
curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x),xlim=c(-.2,1.2), add=TRUE)
