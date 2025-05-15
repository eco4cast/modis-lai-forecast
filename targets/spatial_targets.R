
library(here)
library(sf)
library(lubridate)
library(gdalcubes)
library(rstac)
library(stars)
library(assertthat)

#Source functions
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)
# devtools::load_all()

# input target sites
fire <- "sawmill"

dir = "targets"
set_Site <- list.dirs("./shp", full.names = F,recursive = F)
#site_id = set_Site[3]

#Loop around the sites for creating targets
for(site_id in set_Site){

dt = "P1M"
dx = 0.1
dy = 0.1

print(paste0("Running Creating Terrestrial Targets at ", Sys.time()))



#Create fire bounding box
fire_box <- fire_bbox(fire = site_id, pad_box = TRUE)

#Target date
date <- lubridate::floor_date(as.Date(Sys.time()), "month") #first day of the month
start_date <- date - months(1)
end_date <- lubridate::ceiling_date(start_date, "month") - days(1) #first day of the month

# Ingest data ------------------------------------------------------------
gdalcubes::gdalcubes_options(parallel=TRUE)

# use ingest_planetary_data function to extract raster cube for fire bounding box between Jan 1 2002 and July 1 2023.
raster_cube <- ingest_planetary_data(start_date = start_date, 
                                     end_date = end_date, 
                                     box = fire_box$bbox,
                                     srs = "EPSG:4326",
                                     dx = dx, 
                                     dy = dy, 
                                     dt = dt,
                                     collection = "modis-15A2H-061",
                                     asset_name = "Lai_500m")


# create target file
target <- create_target_file(cuberast = raster_cube,
                             site_id = site_id,
                             date = as.character(start_date),
                             dir = "/vsis3/spat4cast-targets",
                             bucket = NULL,
                             mask = fire_box$maskLayer,
                             dt = dt,
                             var = "lai_recovery")

}                           
