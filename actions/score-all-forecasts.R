#Source packages and functions
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)
install_mc()

# Apply scoring functions to unscored forecasts

# Get date for the relevant monthly target
# target_date <- lubridate::floor_date(as.Date(Sys.time()), "month") %m-% months(1)
date <- "2025-04-01"


for(fire in list.files("shp")){
  if(length(mc_ls(paste0("efi/spat4cast-forecasts/duration=P1M/variable=lai_recovery/site_id=",fire,"/"))) > 0){
    
    target <- spat4cast_get_target(date = date, fire = fire)
    
    
    
    # Look through submitted forecasts and identify which have no score
    
    mc_alias_set("efi", "data.ecoforecast.org",
                 Sys.getenv("AWS_ACCESS_KEY_ID"), Sys.getenv("AWS_SECRET_ACCESS_KEY"))
    
    forecasts <- mc_ls(paste0("efi/spat4cast-forecasts/duration=P1M/variable=lai_recovery/site_id=",fire,"/"), recursive = TRUE)
    
    scored <- mc_ls(paste0("efi/spat4cast-scores/duration=P1M/variable=lai_recovery/site_id=",fire,"/"), recursive = TRUE)
    
    forecasts <- unique(str_extract(forecasts, ".*/reference_date=.*(?=/)"))
    
    scored <- unique(str_extract(scored, ".*/reference_date=.*(?=/)"))
    
    fcs_to_score <- forecasts[which(forecasts %in% scored == 0)]
    
    # Loop through unscored forecasts and get scores
    
    if(length(fcs_to_score) > 0){
      fcs_to_score <- paste0("efi/spat4cast-forecasts/duration=P1M/variable=lai_recovery/site_id=",fire,"/",fcs_to_score,"/")
      for(i in fcs_to_score){
        model_id <- str_extract(i, "(?<=model_id=).*(?=/reference_date=)")
        ref_date <- str_extract(i, "(?<=reference_date=).*(?=/)" )
        ref_date_floor <- lubridate::floor_date(as.Date(ref_date), "month")
        
        # if(ref_date_floor == target_date){
        #   date <- ref_date_floor
        #   target <- spat4cast_get_target(date = date, fire = fire) 
        # }else{
        #   date <- target_date
        #   target <- spat4cast_get_target(date = date, fire = fire) 
        # }
        
        
        #dir <- str_sub(i, 1L, end = -2L)
        dir.create("files")
        mc_cp(i, "files", recursive = TRUE)
        
        file_names <- mc_ls(i)
        parametric_ind <- if_else(1 %in% str_detect(file_names, c("lognormal|normal|bernoulli|beta|uniform|gamma|logistic|exponential|poisson")) == TRUE, 1, 0)
        if(parametric_ind == 1){
          score_dir <- scoring_spat_parametric("files", target, "score_dir")
          spat4cast_score_submit(score_dir, model_id, reference_date = ref_date)
        }else{
          score_dir <- scoring_spat_ensemble("files", target, "score_dir")
          spat4cast_score_submit(score_dir, model_id, reference_date = ref_date)
        }
        unlink("files/*")
        unlink("score_dir/*")
      }
    }else{}
    
  }else{}
}
