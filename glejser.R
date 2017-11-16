glejsertest <- function(model, regressor, power=c(1, 0.5, -0.5,-1), intercept=TRUE){
  if (class(model) != "lm") stop("Not an object of class 'lm' ")
  
  # matrix for storing purposes
  glejser.test <- matrix(1:(4*length(power)), nrow=length(power))
  
  for (i in 1:length(power)) {
    # store information on which power used
    glejser.test[i,1] <- power[i]
    # estimate the auxillary regression
    if(intercept==TRUE) {
        glejser.model <- lm(abs(resid(model)) ~ I(regressor^power[i]))
    } else {
        glejser.model <- lm(abs(resid(model)) ~ -1 + I(regressor^power[i]))
    }
    # Extract t-ratio
    glejser.test[i,2] <- coef(summary(glejser.model))[3]
    # Degrees of freedom 
    glejser.test[i,3] <- df.residual(glejser.model)
    glejser.test[i,4] <- pt(glejser.test[i,2], glejser.test[i,3],lower.tail=FALSE)*2
  }
  # Producing the output
  cat("\n Glejser test \n")
  hi <- which(glejser.test[,2] == max(glejser.test[,2]), arr.ind = TRUE)[1]
  cat("\nPower = ", glejser.test[hi,1])
  cat("\nGlejser = ", round(glejser.test[hi,2],3), 
      ", df = ", glejser.test[hi,3], 
      ", p-value = ", round(glejser.test[hi,4],4), sep="")
}
