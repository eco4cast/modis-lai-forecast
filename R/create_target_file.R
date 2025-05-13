#' Create target geotiff
#'
#' @param cuberast object of class cube; raster cube to generate target file on
#' @param site_id The side ID of the area being forecasted. Currently "august_complex" is the only option.
#' @param dir character; directory that target geotiff is stored
#' @param date character; date to create target geotiff for
#' @param mask sf object; optional, mask polygon to use
#' @param bucket output bucket
#' @param dt timestep
#' @param var variable name
#' @return character; target raster
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

create_target_file <- function(
    cuberast,
    site_id = c("august_complex")[1],
    date,
    dir=tempdir(), 
    mask = NULL,
    bucket = NULL,
    dt = "P1M",
    var = "lai_recovery"
)
  {
  
  #Grab just the time period desired
  target <- cuberast %>%
    gdalcubes::slice_time(date) 
  
  #Filter with the mask for the fire region
  if (!is.null(mask)) {
    target <- target %>%
      gdalcubes::filter_geom(geom = mask,
                             srs = "EPSG:4326")
  }

  #Write output
  if (!is.null(dir)) {
    
    # needed to write geotif to VSI
    Sys.setenv("CPL_VSIL_USE_TEMP_FILE_FOR_RANDOM_WRITE"="YES") 
  
    out <- glue::glue("{dir}/duration={dt}/variable={var}/site_id={site_id}/lai_recovery-target-{date}.tif")
    
    # doesn't take VSI yet; so convert to stars first instead:
    # write_tif(target, dir, "lai_recovery_target_") 
    
    target %>% 
     stars::st_as_stars() %>%
     stars::write_stars(out)

  }
  #return target
  invisible(out)
}
