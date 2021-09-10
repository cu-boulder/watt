#' wald.power
#' Compute the power of a Wald test statistic for the jth coefficient of parameter vector beta
#'
#' @param X matrix The design matrix of the experiment
#' @param beta numeric The coefficient vector
#' @param j numeric The index of the coefficient whose power is to be estimated
#' @param sig numeric The statistical significance of the test, default is .05
#' @param model character The model type, default is 'logit'
#'
#' @return numeric
#' @export
#'
#'
wald_power = function(X, beta, j, sig=.05,model='logit'){
  I = fim(X,beta,model=model)
  Vj = solve(I)[j,j]

  return( pnorm( -z_alpha(sig)  + beta[j]/sqrt(Vj) ) + pnorm( -z_alpha(sig)  - beta[j]/sqrt(Vj) ) )
}
