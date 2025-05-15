#' Download targets from spat4cast-targets bucket and turn it into a cube raster 
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
#' @export
#' 
spat4cast_get_targets <- function(start_date = "2002-01-01",
                               end_date = "2025-03-01",
                               fire,
                               box,
                               collection = "modis-15A2H-061",
                               asset_name = "Lai_500m",
                               srs = "EPSG:4326",
                               dx = 0.1, 
                               dy = 0.1, 
                               dt = "P30D",
                               aggregation = "mean",
                               resampling = "near"){
  
  # check box
  assertthat::are_equal(length(box), 4)
  
  # set mc_alias
  mc_alias_set("efi", "data.ecoforecast.org", "", "")
  
  # create temporary directory to hold files
  dir.create("files")
  
  # Copy files from miniobucket
  mc_cp(paste0("efi/spat4cast-targets/duration=P1M/variable=lai_recovery/site_id=",fire,"/"), "files", recursive = TRUE)
  
  # extract dates from copied files
  d <-str_extract(dir_ls("files/"), "(?<=/).*(?=\\.)")
  
  # set dimensions of the cube
  v <- gdalcubes::cube_view(srs = srs, #lat/lon
                            extent = list(t0 = as.character(start_date), t1 = as.character(end_date),
                                          left = box[1], right = box[3],
                                          top = box[4], bottom = box[2]),
                            dx = dx, dy = dy, dt= dt,
                            aggregation = aggregation, resampling = resampling)
  
  # create image collection
  cube <- gdalcubes::create_image_collection(dir_ls("files/"), date_time = d)
  
  # create proxy data cube
  proxy_cube <- gdalcubes::raster_cube(cube, v)
  return(proxy_cube)
}