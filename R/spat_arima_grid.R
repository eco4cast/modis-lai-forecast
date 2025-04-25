#' Build arima forecast for each gridcell. Currently just works for august complex with grid data.
#'
#' @param data_matrix matrix of data to generate forecasts with
#' @param date date as character format yyyy-mm-dd; date to generate forecast
#' @param dir character; directory to store geotiff files. If the specified directory does not exist, it is created
#' @return character; directory that geotiff files are written to.
#' @examples 
#' # Current examples not relevant
#' # Bounding box ------------------------------------------------------------
#' # pull box, mask
#' fire_box <- fire_bbox(fire = "august_complex", pad_box = TRUE)
#' # Ingest data ------------------------------------------------------------
#' raster_cube <- ingest_planetary_data(start_date = "2002-01-01", 
#'                                      end_date = "2023-07-01", 
#'                                      box = fire_box$bbox)
#' # Generate targets dir/files ------------------------------------------------------------
#' # Forecast ----------------------------------------------------------------
#' ensemble_forecast_dir <- spat_climatology(cuberast = raster_cube,
#'                                           date = '2023-06-22',
#'                                           dir = 'climatology')
#' @export
#' 


spat_arima_grid <- function(data_csv, dir = 'parametric', target){
  ## FUNCTION: spat_grid_ensemble
  ## PURPOSE: spat_grid_ensemble takes a data matrix, target, and directory
  ## and generates a one-step-ahead arima forecast for each grid.
  ## This is an parametric forecasting method, and saves the parameters as
  ## a two layer geotiff file in the specified directory
  ## INPUTS: 
  ## data_csv - csv containing data for grid cells. rows are time and 
  ## columns are gridcells. 
  ## dir - character vector, directory to store geotiff files.
  ## if the specified directory does not exist, it is created
  ## target - target raster. Used to set the spatial attributes 
  ## for the forecast raster/tif
  ## dir - character vector, directory to store geotiff files.
  ## if the specified directory does not exist, it is created
  ## OUTPUTS: N/A
  
  # Read in data matrix
  mat_dat <- read_csv(data_csv, na =c("", "NA", "0"))
  
  # Load target raster
  target_rast <- terra::rast(target, vsi = TRUE)
  
  # Initialize objects to hold the lognormal mean and sd
  forecast_means <- c()
  forecast_sds <- c()
  
  # Loop through colums (grid cells) and fit lognormal arima
  for(i in 1:ncol(mat_dat)){
    # only generate forecasts for grid cells with 25+ observations
    if(sum(is.na(mat_dat[,i]) != TRUE) >= 25){
      # fit arima 
      model <- forecast::auto.arima(log(mat_dat[,i]))
      # store mean
      forecast_means[i] <- forecast(model, h = 1, level = 95)$mean
      # get sd from upper and lower forecast bounds
      forecast_upper <- forecast(model, h = 1, level = 95)$upper
      forecast_lower <- forecast(model, h = 1, level = 95)$lower
      forecast_sds[i] <- (forecast_upper - forecast_lower)/(2*1.96)
    }
    else{
      forecast_means[i] <- NA
      forecast_sds[i] <- NA
    }
  }
  # Make means and sds into a matrix
  fc_mat <- cbind(forecast_means, forecast_sds)
  
  ## convert parameters to raster
  
  fc_rast <- 
    terra::rast(ext(target_rast), # Get dimensions from parameter raster
              res = res(target_rast), # Get res from parameter raster
              crs = crs(target_rast),
              nlyr = 2) %>% # Get cood system from parameter raster
    setValues(fc_mat[,c("forecast_means", "forecast_sds")])
    set.names(fc_rast, c("mu", "sigma"), index = 1:2)
  
  ## check for scoring directory
  dir.create(dir, FALSE)
  
  terra::writeRaster(fc_rast, 
                     filename = paste0(dir,"/lognormal_forecast.tif"),
                     overwrite = TRUE)
  
  return(dir)
}




