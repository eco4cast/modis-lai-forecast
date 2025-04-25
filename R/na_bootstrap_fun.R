#' Perform bootstrap re-sampling for NA values
#'
#' @param x numeric; vector of data, possibly with NA values
#' @return numeric; vector of re-sampled values
#' @examples 
#' x <- c(1, 2, 3, NA)
#' na_boostrap_fun(x)
#' @export
#' 

na_bootstrap_fun <- function(x){
  ## check if all x values are NA. stop if so 
  if (all(is.na(x))) stop('All values NA, cannot perform bootstrap')
  ## check if all x values are non-missing. return x if so
  if (all(!is.na(x))) return(x)
  ## if some values are missing, perform bootstrap resampling
  x[is.na(x)] <- sample(x[!is.na(x)], size = length(x[is.na(x)]), replace = TRUE)
  return(x)
}
