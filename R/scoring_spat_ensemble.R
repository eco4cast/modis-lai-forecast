#' Score spatial ensemble forecast using CRPS and Logarithmic Score
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


scoring_spat_ensemble <- function(fc_dir, target, scores_dir = "scores", tiff = TRUE){
  ## pull most recent target raster
  #last(sort(dir_ls(paste0(target_dir, '/'))))
  target_rast <- rast(target, vsi=TRUE)
  
  ## read in forecast as raster
  fc <- rast(dir_ls(paste0(fc_dir, '/')))
  
  ## create vector of observations from target rast
  y <- as.vector(values(target_rast))
  
  mask <- is.na(y)
  y[mask] <- 1
  
  ## create matrix of data to be used during scoring
  dat <- values(fc)
  
  dat_bootstrap <- apply(dat, 2, FUN = na_bootstrap_fun)
  
  ## compute crps and log score from ensemble
  crps_ensemble <- scoringRules::crps_sample(y = y, dat = dat_bootstrap)
  crps_ensemble[mask] <- NA
  logs_ensemble <- scoringRules::logs_sample(y = y, dat = dat_bootstrap)
  logs_ensemble[mask] <- NA
  
  ## convert scores to raster
  crps_scores <- target_rast
  logs_scores <- target_rast
  values(crps_scores) <- matrix(crps_ensemble, ncol = 1)
  values(logs_scores) <- matrix(logs_ensemble, ncol = 1)
  # Name bands in raster
  set.names(crps_scores, c("crps_score"), index = 1)
  set.names(logs_scores, c("logs_score"), index = 1)
  # 
  
  # Get mean scores across spatial distribution
  mean_logs <- mean(values(logs_scores), na.rm = TRUE)
  mean_crps <- mean(values(crps_scores), na.rm = TRUE)
  
  mean_scores <- c(mean_logs = mean_logs, mean_crps = mean_crps)
  
  if(tiff == TRUE){
    ## check for scoring directory
    dir.create(scores_dir, FALSE)
    
    # Save scores into .tifs
    terra::writeRaster(crps_scores, 
                       filename = paste0(scores_dir, "/crps_scores.tif"),
                       overwrite=TRUE)
    terra::writeRaster(logs_scores, 
                       filename = paste0(scores_dir, '/logs_scores.tif'),
                       overwrite=TRUE)
    print(paste0("Mean LogS : ", mean_logs, " , Mean CRPS: ", mean_crps))
    return(scores_dir)
  }else{
    return(mean_scores)
  }
  
}
