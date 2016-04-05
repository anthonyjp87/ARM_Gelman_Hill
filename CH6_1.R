
rb <- read.dta("risky_behaviors.dta")

hiv <- ifelse(rb$bs_hiv=="negative", 0, 1)

#Model this outcome as a function of treatment assignment using a Poisson regression. Does the model fit well? Is there evidence of overdispersion?
fit.1 <- glm (hiv~women_alone+couples, data=rb, family=poisson)
display(fit.1)


#Next extend the model to include pre-treatment measures of the outcome and the additional pre-treatment variables included in the dataset. Does the model fit well? Is there evidence of overdispersion?


#Fit an overdispersed Poisson model. What do you conclude regarding effec- tiveness of the intervention?

