p<-rep(NA,1)
j<-rep(NA,1)
ppp<-(.6)

for(h in 1:1000){
  b<-rep(NA,50)  
  for(i in 1:1000){
      b[i] <- rbinom(1, 1, ppp)
      if (i > 1) {
        if (b[i] + b[i - 1] == 0) {
          break
        }
      }
    }
  j[h]<-(i)
  p[h]<-sum(b, na.rm=TRUE)/i
}
hist(j, breaks =30)
print(summary(j))
print(summary(p))

print(sd(j))
plot(jitter(j, factor=10),jitter(p,factor = 150),pch=20)
abline(ppp,0)
