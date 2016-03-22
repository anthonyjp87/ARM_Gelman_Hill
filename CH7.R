n.girls <- rbinom (1, 400, .488)

n.sims <- 1000
n.girls <- rep (NA, n.sims)
for (s in 1:n.sims){
  n.girls[s] <- rbinom (1, 400, .488)
}
n.girls.mean <- mean(n.girls)
n.girls.sd <- sd(n.girls)
hist(n.girls, freq = FALSE)
curve(dnorm(x, mean = n.girls.mean, sd = n.girls.sd), add = TRUE)


n.sims1 <- 1000
n.girls1 <- rep (NA, n.sims)
for (s in 1:n.sims1){
birth.type <- sample (c("fraternal twin", "identical twin", "single birth"), size=400, replace=TRUE, prob=c(1/25, 1/300, 1 - 1/25 - 1/300))
girls1 <- rep (NA, 400)
for (i in 1:400){
  if (birth.type[i]=="single birth"){
    girls1[i] <- rbinom (1, 1, .488)}
  else if (birth.type[i]=="identical twin"){
    girls1[i] <- 2*rbinom (1, 1, .495)}
  else if (birth.type[i]=="fraternal twin"){
    girls1[i] <- rbinom (1, 2, .495)}
  }
}

curve(dnorm(x, mean = girls.mean, sd = girls.sd), add = TRUE)


