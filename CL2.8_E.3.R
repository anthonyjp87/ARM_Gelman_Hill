#This is question 3 from Chapter 2
sum.of.uniform <- function(x){
  result <- sum(runif(20, min = 0, max =1))
  #result <-runif(1, min = 0, max =20)
  return(result)
}

d.length <- 1000


d <- rep(c(0), c(d.length))
d.run.mean <- rep(c(0), c(d.length))

for(n in 1:length(d)){
  d[n] <- sum.of.uniform(1)
  d.run.mean[n] <- sum(d)/n
  }


d.mean <- mean(d)
d.sd <- sd(d)

cat("Mean :", d.mean, "\nSd :", d.sd)
hist(d, breaks = 30, freq=FALSE)
curve(dnorm(x, mean = d.mean, sd = d.sd), add = TRUE)
plot(d.run.mean,type = "l")
