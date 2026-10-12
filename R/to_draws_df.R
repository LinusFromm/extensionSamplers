#' Converting from matrix to draws_df
#' @param x Matrix containing the sample including chain, iteration and draw numbers.
#' @param varNames Fill names for the variables if NULL names will be $xi$ for the ith variable.
#' @param matrix Is the sample a matrix? (TRUE for any samples from the (extension) samples in this package)
#'
#' @return Returns the sample object of any of the samplers as a draws_df data type from the posterior package
#' @export
to_draws_df <- function(x,
                        varNames = NULL,
                        matrix = TRUE){

  if(matrix){
    x_df = as.data.frame(x)
  } else {
    x_df = as.data.frame(do.call(rbind, x))
  }

  if(is.null(varNames)){
    colnames(x_df) <- c(paste0("x",1:(ncol(x_df)-3)), ".chain", ".iteration", ".draw")
  } else {
    colnames(x_df) <- c(varNames, ".chain", ".iteration", ".draw")
  }
  return(posterior::as_draws_df(x_df))
}
