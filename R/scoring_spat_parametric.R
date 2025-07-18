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


scoring_spat_parametric <- function(fc_dir, target, scores_dir = "scores", tiff = TRUE){
  
  # get targets as a raster
  target_rast <- rast(target, vsi=TRUE)
  
  ## create vector of observations from target rast
  y <- as.vector(values(target_rast))
  mask <- is.na(y)
  
  if(length(list.files(paste0(fc_dir,"/"))) > 1){
  
  # identify parametric forecast family
  family = str_split_1(list.files(paste0(fc_dir,"/"))[1], "_")[1]
  
  if( family == "lognormal"){
    # parameters for lognormal distribution
    params = c("mu", "sigma")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = paste0(fc_dir,"/")), pattern = params[i])
      param_tiffs[[params[i]]] <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))[ind]))
    } 
    
    crps_scores = scoringRules::crps_lnorm(values(target_rast), 
                                           values(param_tiffs$mu), 
                                           values(param_tiffs$sigma))
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_lnorm(values(target_rast), 
                                           values(param_tiffs$mu), 
                                           values(param_tiffs$sigma))
    logs_scores[mask] <- NA
  }
  
  if( family == "normal"){
    # parameters for normal distribution
    params = c("mu", "sigma")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = paste0(fc_dir,"/")), pattern = params[i])
      param_tiffs[[params[i]]] <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))[ind]))
    } 
    
    crps_scores = scoringRules::crps_norm(values(target_rast), 
                                          values(param_tiffs$mu),
                                          values(param_tiffs$sigma))
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_norm(values(target_rast), 
                                          values(param_tiffs$mu),
                                          values(param_tiffs$sigma))
    logs_scores[mask] <- NA
  }
  
  if( family == "bernoulli"){
    # parameters for bernoulli distribution
    params = c("prob")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = paste0(fc_dir,"/")), pattern = params[i])
      param_tiffs[[params[i]]] <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))[ind]))
    } 
    
    crps_scores = scoringRules::crps_binom(values(target_rast), 
                                           values(param_tiffs$prob))
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_binom(values(target_rast), 
                                           values(param_tiffs$prob))
    logs_scores[mask] <- NA
  }
  
  if( family == "beta"){
    # parameters for beta distribution
    params = c("shape1", "shape2")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = paste0(fc_dir,"/")), pattern = params[i])
      param_tiffs[[params[i]]] <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))[ind]))
    } 
    
    crps_scores = scoringRules::crps_beta(values(target_rast), 
                                          values(param_tiffs$shape1),
                                          values(param_tiffs$shape2))
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_beta(values(target_rast), 
                                          values(param_tiffs$shape1),
                                          values(param_tiffs$shape2))
    logs_scores[mask] <- NA
  }
  
  if( family == "uniform"){
    # parameters for uniform distribution
    params = c("min", "max")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = paste0(fc_dir,"/")), pattern = params[i])
      param_tiffs[[params[i]]] <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))[ind]))
    } 
    
    crps_scores = scoringRules::crps_unif(values(target_rast), 
                                          values(param_tiffs$min),
                                          values(param_tiffs$max))
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_unif(values(target_rast), 
                                          values(param_tiffs$min),
                                          values(param_tiffs$max))
    logs_scores[mask] <- NA
  }
  
  if( family == "gamma"){
    # parameters for gamma distribution
    params = c("shape", "rate")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = paste0(fc_dir,"/")), pattern = params[i])
      param_tiffs[[params[i]]] <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))[ind]))
    } 
    
    crps_scores = scoringRules::crps_gamma(values(target_rast), 
                                           values(param_tiffs$shape),
                                           values(param_tiffs$rate))
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_gamma(values(target_rast), 
                                           values(param_tiffs$shape),
                                           values(param_tiffs$rate))
    logs_scores[mask] <- NA
  }
  
  if( family == "logistic"){
    # parameters for logistic distribution
    params = c("location", "scale")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = paste0(fc_dir,"/")), pattern = params[i])
      param_tiffs[[params[i]]] <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))[ind]))
    } 
    
    crps_scores = scoringRules::crps_logis(values(target_rast), 
                                           values(param_tiffs$location),
                                           values(param_tiffs$scale))
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_logis(values(target_rast), 
                                           values(param_tiffs$location),
                                           values(param_tiffs$scale))
    logs_scores[mask] <- NA
  }
  
  if( family == "exponential"){
    # parameters for exponential distribution
    params = c("rate")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = paste0(fc_dir,"/")), pattern = params[i])
      param_tiffs[[params[i]]] <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))[ind]))
    } 
    
    crps_scores = scoringRules::crps_exp(values(target_rast), 
                                         values(param_tiffs$rate))
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_exp(values(target_rast), 
                                         values(param_tiffs$rate))
    logs_scores[mask] <- NA
  }
  
  if( family == "poisson"){
    # parameters for poisson distribution
    params = c("lambda")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    # Make rasters for parameters by name
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- rast(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    } 
    
    crps_scores = scoringRules::crps_pois(values(target_rast), 
                                          values(param_tiffs$lambda))
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_pois(values(target_rast), 
                                          values(param_tiffs$lambda))
    logs_scores[mask] <- NA
  }
  
  
  
}
else{
  
  # identify parametric forecast family
  family = str_split_1(list.files(paste0(fc_dir,"/"))[1], "_")[1]
  
  if( family == "lognormal"){
    # parameters for lognormal distribution
    params = c("mu", "sigma")
    fc_rast <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))))
    
    crps_scores = scoringRules::crps_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]], 
                                           values(fc_rast)[,params[2]])
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]], 
                                           values(fc_rast)[,params[2]])
    logs_scores[mask] <- NA
  }
  
  if( family == "normal"){
    # parameters for normal distribution
    params = c("mu", "sigma")
    fc_rast <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))))
    
    crps_scores = scoringRules::crps_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]], 
                                           values(fc_rast)[,params[2]])
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]], 
                                           values(fc_rast)[,params[2]])
    logs_scores[mask] <- NA
  }
  
  if( family == "bernoulli"){
    # parameters for bernoulli distribution
    params = c("prob")
    fc_rast <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))))
    
    crps_scores = scoringRules::crps_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]])
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]])
    logs_scores[mask] <- NA
  }
  
  if( family == "beta"){
    # parameters for beta distribution
    params = c("shape1", "shape2")
    fc_rast <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))))
    
    crps_scores = scoringRules::crps_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]], 
                                           values(fc_rast)[,params[2]])
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]], 
                                           values(fc_rast)[,params[2]])
    logs_scores[mask] <- NA
  }
  
  if( family == "uniform"){
    # parameters for uniform distribution
    params = c("min", "max")
    fc_rast <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))))
    
    crps_scores = scoringRules::crps_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]], 
                                           values(fc_rast)[,params[2]])
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]], 
                                           values(fc_rast)[,params[2]])
    logs_scores[mask] <- NA
  }
  
  if( family == "gamma"){
    # parameters for gamma distribution
    params = c("shape", "rate")
    fc_rast <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))))
    
    crps_scores = scoringRules::crps_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]], 
                                           values(fc_rast)[,params[2]])
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]], 
                                           values(fc_rast)[,params[2]])
    logs_scores[mask] <- NA
  }
  
  if( family == "logistic"){
    # parameters for logistic distribution
    params = c("location", "scale")
    fc_rast <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))))
    
    crps_scores = scoringRules::crps_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]], 
                                           values(fc_rast)[,params[2]])
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]], 
                                           values(fc_rast)[,params[2]])
    logs_scores[mask] <- NA
  }
  
  if( family == "exponential"){
    # parameters for exponential distribution
    params = c("rate")
    fc_rast <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))))
    
    crps_scores = scoringRules::crps_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]])
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]])
    logs_scores[mask] <- NA
  }
  
  if( family == "poisson"){
    # parameters for poisson distribution
    params = c("lambda")
    fc_rast <- rast(paste0(fc_dir,"/",list.files(path = paste0(fc_dir,"/"))))
    
    crps_scores = scoringRules::crps_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]])
    crps_scores[mask] <- NA
    logs_scores = scoringRules::logs_lnorm(values(target_rast), 
                                           values(fc_rast)[,params[1]])
    logs_scores[mask] <- NA
  }
}

# Make raster for crps scores
crps_raster <- 
  terra::rast(ext(target_rast), # Get dimensions from parameter raster
              res = res(target_rast), # Get res from parameter raster
              crs = crs(target_rast)) %>% # Get cood system from parameter raster
  setValues(crps_scores)

# Make raster for log scores
logs_raster <- 
  terra::rast(ext(target_rast), # Get dimensions from parameter raster
              res = res(target_rast), # Get res from parameter raster
              crs = crs(target_rast)) %>% # Get cood system from parameter raster
  setValues(logs_scores)

# Name bands in raster
set.names(crps_raster, c("crps_score"), index = 1)
set.names(logs_raster, c("logs_score"), index = 1)





# Get mean scores across spatial distribution
mean_logs <- mean(values(logs_raster), na.rm = TRUE)
mean_crps <- mean(values(crps_raster), na.rm = TRUE)

mean_scores <- c(mean_logs = mean_logs, mean_crps = mean_crps)

if(tiff == TRUE){
## check for scoring directory
dir.create(scores_dir, FALSE)
  
# Save scores into .tifs
terra::writeRaster(crps_raster, 
                     filename = paste0(scores_dir, "/crps_scores.tif"),
                     overwrite=TRUE)
terra::writeRaster(logs_raster, 
                   filename = paste0(scores_dir, '/logs_scores.tif'),
                   overwrite=TRUE)
print(paste0("Mean LogS : ", mean_logs, " , Mean CRPS: ", mean_crps))
return(scores_dir)
  }else{
  return(mean_scores)
}


}
