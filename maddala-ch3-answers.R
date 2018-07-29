q6.data <- read.csv("maddala-c3q6.csv", sep=",", header=TRUE)

# Simple linear regression
q6 <- lm(y~x, data=q6.data)
summary(q6)


# constructing confidence intervals by hand
beta.hat <- coef(q6)[2]
se.beta.hat <- sqrt(vcov(q6)[2,2])

deg.free <- length(q6.data$x)-length(coef(q6)) # n - k

# qt() is the quantile function of the t distribution
confint.beta.hat <- beta.hat + qt(p=c(0.025,0.975), df=deg.free)*se.beta.hat

confint.beta.hat

# Or use built-in function
confint(q6, level=0.95)

# confidence intervals for sigma squared
sigma.sq.hat <- sum(resid(q6)^2)/deg.free

confint.sigma.sq.hat <- deg.free*sigma.sq.hat/qchisq(c(0.95, 0.05), df=deg.free)
confint.sigma.sq.hat


plot(q6, which=1)
