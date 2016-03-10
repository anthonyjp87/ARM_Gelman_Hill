var1 <- rnorm(1000,0,1)
var2 <- rnorm(1000,0,1)

fit <- lm(var1~var2)
display(fit)
plot (var1, var2)
curve(coef(fit)[1] + coef(fit)[2]*x,add=TRUE)

z.scores <- rep (NA, 10000)

for (k in 1:length(z.scores)){
  var1 <- rnorm(1000,0,1)
  var2 <- rnorm(1000,0,1)
  fit.2 <- lm(var1~var2)
  z.scores[k] <- coef(fit.2)[2]/se.coef(fit.2)[2]
}
print(z.scores)
z.scores.sig <- (z.scores[z.scores>2])
sig<- which (z.scores<(-2), z.scores>2)
print(length(sig))
