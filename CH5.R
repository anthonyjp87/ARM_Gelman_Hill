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
print(anova(fit.1, test="Chisq"))
print(pR2(fit.1))


fitted.results <- predict(fit.1,newdata=r.test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misclas <-c(0)

r.test$rodent2[is.na(r.test$rodent2)] <- 0

for(n in 1:length(fitted.results)){
  misclas[n]<-ifelse(fitted.results[n]!=r.test$rodent2[n],0,1)
}

predictionrate<-(1-(length(which(misclas==0))/(length(which(misclas==1)))))

