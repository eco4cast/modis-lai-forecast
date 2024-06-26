#' Score spatial parametric forecast using CRPS and Logarithmic Score
#'
#' @param fc_dir character; directory that geotiff ensemble forecasts are stored
#' @param target_dir character; directory that target geotiff is stored
#' @param scores_dir character; directory to store geotiff files of scores. If the specified directory does not exist, it is created
#' @return character; directory that geotiff score files are written to.
#' @examples 
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
#' # Generate targets dir/files ------------------------------------------------------------
#' target_forecast_dir <- create_target_file(cuberast = raster_cube, 
#'                                           date = '2023-06-22',
#'                                           dir = 'targets',
#'                                           mask = fire_box$maskLayer)
#' # Score ----------------------------------------------------------------
#' scored_forecast_dir <- scoring_spat_ensemble(fc_dir = ensemble_forecast_dir,
#'                                              target_dir = target_forecast_dir,
#'                                              scores_dir = 'scores')
#' @export
#' 


spatial_score <- function(fc_dir, target_dir, scores_dir){
  
  # get targets as a raster
  target <- raster(paste0(target_dir,"/",list.files(path = target_dir)[1]))
  
  # identify parametric forecast family
  family = str_split_1(list.files(path = fc_dir)[1], "_")[1]
  
  if( family == "lognormal"){
    # parameters for lognormal distribution
    params = c("mu", "sigma")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    } 
    
    crps_scores = crps_lnorm(getValues(target), 
                             getValues(param_tiffs$mu), 
                             getValues(param_tiffs$sigma))
    logs_scores = logs_lnorm(getValues(target), 
                             getValues(param_tiffs$mu), 
                             getValues(param_tiffs$sigma))
  }
  
  if( family == "normal"){
    # parameters for normal distribution
    params = c("mu", "sigma")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    } 
    
    crps_scores = crps_norm(getValues(target), 
                            getValues(param_tiffs$mu),
                            getValues(param_tiffs$sigma))
    logs_scores = logs_norm(getValues(target), 
                            getValues(param_tiffs$mu),
                            getValues(param_tiffs$sigma))
  }
  
  if( family == "bernoulli"){
    # parameters for bernoulli distribution
    params = c("prob")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    } 
    
    crps_scores = crps_binom(getValues(target), 
                             getValues(param_tiffs$prob))
    logs_scores = logs_binom(getValues(target), 
                             getValues(param_tiffs$prob))
  }
  
  if( family == "beta"){
    # parameters for beta distribution
    params = c("shape1", "shape2")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    } 
    
    crps_scores = crps_beta(getValues(target), 
                            getValues(param_tiffs$shape1),
                            getValues(param_tiffs$shape2))
    logs_scores = logs_beta(getValues(target), 
                            getValues(param_tiffs$shape1),
                            getValues(param_tiffs$shape2))
  }
  
  if( family == "uniform"){
    # parameters for uniform distribution
    params = c("min", "max")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    } 
    
    crps_scores = crps_unif(getValues(target), 
                            getValues(param_tiffs$min),
                            getValues(param_tiffs$max))
    logs_scores = logs_unif(getValues(target), 
                            getValues(param_tiffs$min),
                            getValues(param_tiffs$max))
  }
  
  if( family == "gamma"){
    # parameters for gamma distribution
    params = c("shape", "rate")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    } 
    
    crps_scores = crps_gamma(getValues(target), 
                            getValues(param_tiffs$shape),
                            getValues(param_tiffs$rate))
    logs_scores = logs_gamma(getValues(target), 
                            getValues(param_tiffs$shape),
                            getValues(param_tiffs$rate))
  }
  
  if( family == "logistic"){
    # parameters for logistic distribution
    params = c("location", "scale")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    } 
    
    crps_scores = crps_logis(getValues(target), 
                            getValues(param_tiffs$location),
                            getValues(param_tiffs$scale))
    logs_scores = logs_logis(getValues(target), 
                            getValues(param_tiffs$location),
                            getValues(param_tiffs$scale))
  }
  
  if( family == "exponential"){
    # parameters for exponential distribution
    params = c("rate")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    } 
    
    crps_scores = crps_exp(getValues(target), 
                            getValues(param_tiffs$rate))
    logs_scores = logs_exp(getValues(target), 
                            getValues(param_tiffs$rate))
  }
  
  if( family == "poisson"){
    # parameters for poisson distribution
    params = c("lambda")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    } 
    
    crps_scores = crps_pois(getValues(target), 
                            getValues(param_tiffs$lambda))
    logs_scores = logs_pois(getValues(target), 
                            getValues(param_tiffs$lambda))
  }
  
  
  # Make raster for crps scores
  crps_raster <- 
    raster::raster(extent(param_tiffs[1]), # Get dimensions from parameter raster
                   res = res(param_tiffs[1]), # Get res from parameter raster
                   crs = crs(param_tiffs[1])) %>% # Get cood system from parameter raster
    setValues(crps_scores)
  
  # Make raster for log scores
  logs_raster <- 
    raster::raster(extent(param_tiffs[1]), # Get dimensions from parameter raster
                   res = res(param_tiffs[1]), # Get res from parameter raster
                   crs = crs(param_tiffs[1])) %>% # Get cood system from parameter raster
    setValues(logs_scores)
  
  # Save scores into .tifs
  raster::writeRaster(crps_raster, 
                     filename = paste0(scores_dir, "/crps_scores.tif"),
                     overwrite=TRUE)
  raster::writeRaster(logs_raster, 
                     filename = paste0(scores_dir, '/logs_scores.tif'),
                     overwrite=TRUE)
  
  
  return(scores_dir) 
}



