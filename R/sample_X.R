#' sample_X
#'
#' @param n
#' @param m
#'
#' @return
#' matrix
#'
#' @export
#'
#' @examples
sample_X = function(n,m){
  dat = stats::runif(n*m)
  return(matrix(dat,nrow=n,ncol=m))

}
