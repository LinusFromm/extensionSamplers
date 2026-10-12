#' Is the vector in the extension?
#' @param x Vector that is tested
#'
#' @return Returns true/false for whether any of `x`s entries is negative
#' @export
isOutside <- function(x){
  x <- as.numeric(x)

  return(ifelse(all(x >= 0), FALSE, TRUE))
}
