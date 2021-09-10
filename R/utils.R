# SCALAR FUNCS FOR INTERNAL USE

#' inv_logit
#' Evaluate the inverse logit function, ie. the logitistic function
#'
#' @param z numeric
#'
#' @return numeric
#'
inv_logit = function(z) {
  return(1/(1+exp(-z)))
}

#' z_alpha
#' Evaluate the 1-alpha/2 quantile of a std normal
#'
#' @param alpha numeric
#'
#' @return numeric
#'
z_alpha = function(alpha){
  stats::qnorm(1-(alpha/2))
}
