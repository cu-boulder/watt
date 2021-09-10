fim_weights_logit = function(X,beta){
  z = as.numeric(X %*% beta)

  num = exp(z)
  denom = (1+exp(z))^2

  return(num/denom)
}
