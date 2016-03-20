#rodents

r <- read.table("rodents.dat")
r.training <- r[1:1000,]
r.test <- r[1001:1747,]
print(sapply(r,function(x) sum(is.na(x))))
print("unique")
print(sapply(r, function(x) length(unique(x))))
# missmap(r,main="missing data")

fit.1 <- glm(rodent2~dilap, family=binomial, data=r.training)
display(fit.1)
plot(jitter(dilap, factor=.5), jitter(rodent2, factor=.5), pch=20)
curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x),xlim=c(-.2,1.2), add=TRUE)
