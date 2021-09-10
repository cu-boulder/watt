#' mde_objective
#' An objective function passed to the root finder which solves for the MDE
#'
#' @param MDE numeric main argument of the objective
#' @param power numeric
#' @param n numeric
#' @param X matrix
#' @param beta numeric
#' @param j numeric
#' @param sig numeric
#'
#' @return numeric
#'
mde_objective = function(MDE, power, n, X, beta, j, sig=.05){
  parms = beta
  parms[j] = MDE
  power - watt::wald_power(X, parms, j, sig=sig)
}

#' mde
#' Compute the minimum detectable effect for a logit model
#'
#' @param power numeric Desired power level
#' @param n numeric Sample size
#' @param X matrix Design matrix
#' @param beta numeric Coefficient vector
#' @param j numeric Coefficient to estimate MDE for
#' @param upper numeric Upper bound on MDE search, default is 100 TODO: choose this smarter or eliminate
#' @param sig numeric Statistical power, default is .05
#'
#' @return numeric
#' @export
#'
mde = function(power, n, X, beta, j, upper=100, sig=.05){
  res = stats::uniroot(mde_objective, c(0,upper), power, n, X, j, beta, sig=sig, extendInt='yes')
  return(res$root)
}
