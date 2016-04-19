p<-.4
n_trials<-1000
x<-rep(1,n_trials)


#Simulate data, the stochastic element from rbin
ybin <- rbinom(n_trials, 1, p)

#Regress X on Y
fit.1 <- glm(ybin~x, family=binomial)
display(fit.1)

p.hat <- coef (fit.1)[1]     
p.se <- se.coef (fit.1)[1]   

cover.68 <- abs (p + p.hat) < p.se    
cover.95 <- abs (p + p.hat) < 2*p.se 

cat (paste ("68% coverage: ", cover.68, "\n"))
cat (paste ("95% coverage: ", cover.95, "\n"))

n.fake <- 1000
cover.68 <- rep (NA, n.fake)
cover.95 <- rep (NA, n.fake)

for (s in 1:n.fake){
  ybin <- rbinom(n_trials, 1, p)
  fit.1 <- glm(ybin~x, family=binomial)
  p.hat <- coef (fit.1)[1]     
  p.se <- se.coef (fit.1)[1]   
  cover.68[s] <- abs (p + p.hat) < p.se    
  cover.95[s] <- abs (p + p.hat) < 2*p.se 
}
cat (paste ("68% coverage: ", mean(cover.68), "\n"))
cat (paste ("95% coverage: ", mean(cover.95), "\n"))

