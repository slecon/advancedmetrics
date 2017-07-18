glejsertest <- function(model, regressor, power=c(1, 0.5, -0.5,-1)){
  if (class(model) != "lm") stop("Not an object of class 'lm' ")
  uhat <- abs(resid(model))
  xvar <- model.frame(model)[paste(regressor)]
  
  glejser.test <- matrix(1:(5*length(power)), nrow=length(power))
  
  for (i in 1:length(power)) {
    glejser.test[i,1] <- power[i]
    f <- summary(lm(uhat ~ I(xvar^power[i])))$fstatistic
    glejser.test[i,2:4] <- f
    glejser.test[i,5] <- pf(f[1],f[2],f[3],lower.tail=F)
  }
  print("Glejser test", quote=FALSE)
  print(" ", quote=FALSE)
  hi <- which(glejser.test[,2] == max(glejser.test[,2]), arr.ind = TRUE)[1]
  print(paste("Power: ", glejser.test[hi,1], sep=""), quote=FALSE)
  print(paste("Glejser = ", round(glejser.test[hi,2],3), 
              ", df1 = ", glejser.test[hi,3], 
              ", df2 = ", glejser.test[hi,4], 
              ", p-value = ", round(glejser.test[hi,5],4),sep = ""), quote=FALSE)
}
