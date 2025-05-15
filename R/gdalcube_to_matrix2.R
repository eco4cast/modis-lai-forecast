#' Extract matrix from gdalcube
#'
#' @param cube A data cube proxy object (gdalcubes) to extract values from
#' @param d dimension value object for rastercube 'cube'
#' @param threshold: numeric, upper bounds for LAI. values above threshold are discarded
#' @param dir character, directory to write temporary GeoTiff files to
#' @return matrix of LAI values indexed by time which may be used to fit arima-grid models
#' @examples 
#' @export
#' 



## function to extract nimble matrix
gdalcube_to_matrix2 <- function(cube, d, threshold = 12){
  ## Outputs:
  ## nimble_mat: matrix of LAI values indexed by time to be
  ## passed to nimble to fit the baseline spatio-temporal
  ## random walk model.
  
  array <- as_array(cube)
  
  mat <- c()
  for(i in 1:dim(array)[3]){
    for(j in 1:dim(array)[4]){
      a  <- array[,,i,j]
      mat <- cbind(mat,a)
    }
  }
  
  mat[mat > threshold] <- NA
  mat[is.nan(mat)] <- NA
  mat[mat == 0] <- NA
  
  return(mat)
}
