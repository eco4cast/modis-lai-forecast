#' Download data from spat4cast-data bucket and turn it into a cube raster 
#'
#' @param start_date start date as character format yyyy-mm-dd
#' @param end_date end date as character format yyyy-mm-dd
#' @param fire character, the name of the fire/ site
#' @param box numeric vector in the format of (xmin, ymin, xmax, ymax)
#' @param asset_name mame of asset 
#' @param srs target spatial reference system as a string; can be a proj4 definition, WKT, or in the form "EPSG:XXXX"
#' @param dx size of pixels in x-direction (longitude / easting)
#' @param dy size of pixels in y-direction (latitude / northing)
#' @param dt size of pixels in time-direction, expressed as ISO8601 period string (only 1 number and unit is allowed) such as "P16D"
#' @param aggregation aggregation method as string, defining how to deal with pixels containing data from multiple images, can be "min", "max", "mean", "median", or "first"
#' @param resampling resampling method used in gdalwarp when images are read, can be "near", "bilinear", "bicubic" or others as supported by gdalwarp (see https://gdal.org/programs/gdalwarp.html)
#' @return A data cube proxy object
#' @examples 
#' ingest_planetary_data(start_date = "2022-01-01", end_date = "2023-07-01", box =  c("xmin" = -123, "ymin" = 39, "xmax" = -122, "ymax" = 40))
#' @export
#' 
spat4cast_get_score <- function(date = "2025-04-01", fire = "august_complex", model_id = "arima_grid_model"){
  
  # set mc_alias
  mc_alias_set("efi", "data.ecoforecast.org", "", "")
  
  # create temporary directory to hold files
  dir.create("score")
  
  # Get the most recent reference date
  
  # Copy files from miniobucket
  mc_cp(paste0("efi/spat4cast-targets/duration=P1M/variable=lai_recovery/site_id=",fire,"/reference_date=", ref_date,".tif"), "target")
  
  out <- paste0("target/lai_recovery-target-",date,".tif")
  
  return(out)
}
