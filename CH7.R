n.girls <- rbinom (1, 400, .488)
print (n.girls)

n.sims <- 1000
n.girls <- rep (NA, n.sims)
for (s in 1:n.sims){
  n.girls[s] <- rbinom (1, 400, .488)
}