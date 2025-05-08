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
gdalcube_to_matrix <- function(cube, d, threshold = 12, dir = '../GeoTiffs/'){
  ## Outputs:
  ## nimble_mat: matrix of LAI values indexed by time to be
  ## passed to nimble to fit the baseline spatio-temporal
  ## random walk model.
  
  ## initialize nimnble_mat using information from d object
  mat <- matrix(NA, nrow = length(d$t), 
                       ncol = length(d$x)*length(d$y))
  ## loop through time indices of d
  for (i in 1:length(d$t)){
    ## slice cube at time i and write GeoTiff file
    cube %>% gdalcubes::select_time(d$t[i]) %>% write_tif(dir = dir, prefix = 'tmp')
    ## recreate filename
    filename <- paste0(dir, 'tmp', d$t[i], '.tif')
    ## read in file as raster
    tmp_rast <- rast(filename, vsi = TRUE)
    ## extract values from raster
    tmp_values <- as.vector(terra::values(tmp_rast))
    ## subset values using threshold
    tmp_values[tmp_values > threshold] <- NA
    ## write i-th row of nimble matrix using raster values
    mat[i,] <- tmp_values
    ## remove temporary GeoTiff file
    if (paste0('tmp', d$t[i], '.tif') %in% list.files(dir)) file.remove(filename)
  }
  return(mat)
}
