#' fim
#' Return the specified Fisher Information Matrix, given design matrix, coefficient vector, and model choice
#'
#' @param X matrix
#' @param beta numeric
#' @param model character
#'
#' @return matrix
#' @export
#'
#'
fim = function(X, beta, model){
  if (model=='logit'){
    w = fim_weights_logit(X,beta)
  }

  else {
    stop('Value error: invalid argument for `model`')
  }

  I = t(X) %*% sweep(X,1,w,`*`)

  return(I)
}
