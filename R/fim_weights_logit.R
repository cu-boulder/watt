#' fim_weights_logit
#' Computes the Fisher Information Matrix weights for a logistic regression model
#'
#' @param X matrix The design matrix of the model
#' @param beta vector The coefficient vector
#'
#' @return vector
#' @export
#'
fim_weights_logit = function(X,beta){
  z = as.numeric(X %*% beta)

  num = exp(z)
  denom = (1+exp(z))^2

  return(num/denom)
}
