
rb <- read.dta("risky_behaviors.dta")

hiv <- ifelse(rb$bs_hiv=="negative", 0, 1)

fit.1 <- glm (hiv~bupacts, data=rb, family=poisson)
display(fit.1)

