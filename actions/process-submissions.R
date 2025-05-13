#Source packages and functions
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)
install_mc()

mc_alias_set("efi", "data.ecoforecast.org",
             Sys.getenv("AWS_ACCESS_KEY_ID"), Sys.getenv("AWS_SECRET_ACCESS_KEY"))


#for(fire in c(list.files("shp"))){

  fire <- "august_complex"
  
  submitted <- mc_ls(paste0("efi/spat4cast-submissions/duration=P1M/variable=lai_recovery/site_id=",fire,"/"), recursive = TRUE)
  
  # get submission forecasts
  dir.create("submissions")
  mc_cp(paste0("efi/spat4cast-submissions/duration=P1M/variable=lai_recovery/site_id=",fire,"/"), "submissions",recursive =  TRUE)
  
  for (i in 1:length(submitted)){
    forecast <- str_extract(submitted[i], "(?<=reference_date=.{10}/).*(?=\\.)")
    
    dir.create("forecasts")
    temp_rast <- rast(paste0("submissions/", submitted[i]))
    writeRaster(temp_rast, paste0("forecasts/",forecast), filetype = "COG", overwrite = TRUE)
    mc_cp(paste0("forecasts/",forecast), paste0("efi/spat4cast-forecasts/duration=P1M/variable=lai_recovery/site_id=",fire,"/",submitted[i]), recursive = TRUE)
  }
#}
