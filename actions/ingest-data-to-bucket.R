#Source packages and functions
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)
install_mc()

for(fire in c(list.files("shp"))){

# list.files(here::here("~/Projects/modis-lai-forecast/R"), full.names = TRUE)
## read in fire bounding box
fire_box <- fire_bbox(fire = fire, pad_box = TRUE, dir = "shp")
## turn parallelization on
gdalcubes::gdalcubes_options(parallel=TRUE)
# use ingest_planetary_data function to extract raster cube for fire bounding box between Jan 1 2002 and Mar 1 2023. Loop through 5 year increments to avoid ingest_planetary_data timing out.
#dates <- c("2002-01-01", "2007-01-01", "2012-01-01", "2017-01-01", "2022-01-01", "2025-03-01")

mc_alias_set("efi", "data.ecoforecast.org",
             Sys.getenv("AWS_ACCESS_KEY_ID"), Sys.getenv("AWS_SECRET_ACCESS_KEY"))

dir.create("cube")

# raster_cube <- ingest_planetary_data(start_date = "2002-01-01",
#                                      end_date = "2007-01-01",
#                                      box = fire_box$bbox,
#                                      srs = "EPSG:4326",
#                                      dx = 0.1,
#                                      dy = 0.1,
#                                      dt = "P30D",
#                                      collection = "modis-15A2H-061",
#                                      asset_name = "Lai_500m")
# 
# 
# write_tif(raster_cube, dir = "cube", prefix = "Lai_500m_")
# 
# raster_cube <- ingest_planetary_data(start_date = "2007-01-01",
#                                      end_date = "2012-01-01",
#                                      box = fire_box$bbox,
#                                      srs = "EPSG:4326",
#                                      dx = 0.1,
#                                      dy = 0.1,
#                                      dt = "P30D",
#                                      collection = "modis-15A2H-061",
#                                      asset_name = "Lai_500m")
# 
# 
# write_tif(raster_cube, dir = "cube", prefix = "Lai_500m_")
# 
# raster_cube <- ingest_planetary_data(start_date = "2012-01-01",
#                                      end_date = "2017-01-01",
#                                      box = fire_box$bbox,
#                                      srs = "EPSG:4326",
#                                      dx = 0.1,
#                                      dy = 0.1,
#                                      dt = "P30D",
#                                      collection = "modis-15A2H-061",
#                                      asset_name = "Lai_500m")
# 
# 
# write_tif(raster_cube, dir = "cube", prefix = "Lai_500m_")
# 
# raster_cube <- ingest_planetary_data(start_date = "2017-01-01",
#                                      end_date = "2022-01-01",
#                                      box = fire_box$bbox,
#                                      srs = "EPSG:4326",
#                                      dx = 0.1,
#                                      dy = 0.1,
#                                      dt = "P30D",
#                                      collection = "modis-15A2H-061",
#                                      asset_name = "Lai_500m")
# 
# 
# write_tif(raster_cube, dir = "cube", prefix = "Lai_500m_")
# 
# raster_cube <- ingest_planetary_data(start_date = "2022-01-01",
#                                      end_date = "2025-05-01",
#                                      box = fire_box$bbox,
#                                      srs = "EPSG:4326",
#                                      dx = 0.1,
#                                      dy = 0.1,
#                                      dt = "P30D",
#                                      collection = "modis-15A2H-061",
#                                      asset_name = "Lai_500m")
# 
# 
# write_tif(raster_cube, dir = "cube", prefix = "Lai_500m_")

raster_cube <- ingest_planetary_data(start_date = "2025-03-01",
                                     end_date = Sys.Date(),
                                     box = fire_box$bbox,
                                     srs = "EPSG:4326",
                                     dx = 0.1,
                                     dy = 0.1,
                                     dt = "P30D",
                                     collection = "modis-15A2H-061",
                                     asset_name = "Lai_500m")


write_tif(raster_cube, dir = "cube", prefix = "Lai_500m_")

mc_cp("cube/", paste0("efi/spat4cast-data/duration=P1M/variable=lai_recovery/site_id=", fire,"/"), recursive = TRUE)

unlink("cube/*")

}
