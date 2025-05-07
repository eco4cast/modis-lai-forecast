#' Extract spatio-temporal matrix
#'
#' @param cube rastercube object to extract values from
#' @param d dimension value object for rastercube 'cube'
#' @param dir character, directory to write temporary GeoTiff files to
#' @return numeric matrix of LAI values indexed by time to be
#' @examples
#' # Bounding box ------------------------------------------------------------
#' # pull box, mask
#' fire_box <- fire_bbox(fire = "august_complex", pad_box = TRUE)
#' # Ingest data ------------------------------------------------------------
#' raster_cube <- ingest_planetary_data(start_date = "2002-01-01",
#'                                      end_date = "2023-07-01",
#'                                      box = fire_box$bbox)
#' # Generate targets dir/files ------------------------------------------------------------
#' target_forecast_dir <- create_target_file(cuberast = raster_cube,
#'                                           date = '2023-06-22',
#'                                           dir = 'targets',
#'                                           bucket = c(NULL,"efi/spat4cast-targets")[1],
#'                                           mask = fire_box$maskLayer)

extract_spat_matrix <- function(cube, d, threshold = 12, dir = '../GeoTiffs/'){
  ## extract_spat_matrix
  ## Author: John W. Smith
  ## Inputs:
  ## cube: rastercube object to extract values from
  ## d: dimension value object for rastercube 'cube'
  ## threshold: numeric, upper bounds for LAI. values above threshold are discarded
  ## dir: character, directory to write temporary GeoTiff files to

  ## Outputs:
  ## spat_mat: matrix of LAI values indexed by time to be
  ## passed to nimble to fit the baseline spatio-temporal
  ## random walk model.

  ## initialize nimnble_mat using information from d object
  spat_mat <- matrix(NA, nrow = length(d$t),
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
    spat_mat[i,] <- tmp_values
    ## remove temporary GeoTiff file
    if (paste0('tmp', d$t[i], '.tif') %in% list.files(dir)) file.remove(filename)
  }
  return(spat_mat)
}
