#Source packages and functions
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)
install_mc()

mc_alias_set("efi", "data.ecoforecast.org",
             Sys.getenv("AWS_ACCESS_KEY_ID"), Sys.getenv("AWS_SECRET_ACCESS_KEY"))

mc_alias_set("efi", "data.ecoforecast.org",
             "spatial-team", "EXk4HARe6AXoZRR3uVwQ")

  
# for(fire in list.files("shp")){
#   if(length(mc_ls(paste0("efi/spat4cast-submissions/duration=P1M/variable=lai_recovery/site_id=",fire,"/"))) > 0){
  
fire <- "august_complex"
  
  submitted <- mc_ls(paste0("efi/spat4cast-submissions/duration=P1M/variable=lai_recovery/site_id=",fire,"/"), recursive = TRUE)
  
  # get submission forecasts
  dir.create("submissions")
  mc_cp(paste0("efi/spat4cast-submissions/duration=P1M/variable=lai_recovery/site_id=",fire,"/"), "submissions",recursive =  TRUE)
  
  for (i in 1:length(submitted)){
    model_id <- str_extract(submitted[i], "(?<=model_id=).*(?=/reference_date=)")
    ref_date <- str_extract(submitted[i], "(?<=reference_date=).*(?=/)")
    forecast_file <- str_extract(submitted[i], "(?<=reference_date=.{10}/).*")
    
    dir.create("forecasts")
    temp_rast <- rast(paste0("submissions/", submitted[i]))
    writeRaster(temp_rast, paste0("forecasts/",forecast_file), filetype = "COG", overwrite = TRUE)
    mc_cp(paste0("forecasts/",forecast_file), paste0("efi/spat4cast-forecasts/duration=P1M/variable=lai_recovery/site_id=",fire,"/model_id=",model_id,"/reference_date=",ref_date,"/"), recursive = TRUE)
  }

#   }else{}
# }

