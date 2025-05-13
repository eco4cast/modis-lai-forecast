#Source packages and functions
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

# Apply scoring functions to unscored forecasts
for(fire in c(list.files("shp"))){
# fire <- "august_complex"
# Get date for the relevant monthly target
date <- lubridate::floor_date(as.Date(Sys.time()), "month")

target <- spat4cast_get_target(date = date, fire = fire)


# Look through submitted forecasts and identify which have no score

mc_alias_set("efi", "data.ecoforecast.org",
             Sys.getenv("AWS_ACCESS_KEY_ID"), Sys.getenv("AWS_SECRET_ACCESS_KEY"))

submitted <- mc_ls(paste0("efi/spat4cast-submissions/duration=P1M/variable=lai_recovery/site_id=",fire,"/"), recursive = TRUE)

scored <- mc_ls(paste0("efi/spat4cast-scores/duration=P1M/variable=lai_recovery/site_id=",fire,"/"), recursive = TRUE)

submitted <- unique(str_extract(submitted, ".*/reference_date=.*(?=/)"))

scored <- unique(str_extract(scored, ".*/reference_date=.*(?=/)"))

subs_to_score <- submitted[which(submitted %in% scored == 0)]

# Loop through unscored forecasts and get scores

if(length(subs_to_score) > 0){
  subs_to_score <- paste0("efi/spat4cast-submissions/duration=P1M/variable=lai_recovery/site_id=",fire,"/",subs_to_score,"/")
  for(i in subs_to_score){
    model_id <- str_extract(i, "(?<=model_id=).*(?=/reference_date=)")
    ref_date <- str_extract(i, "(?<=reference_date=).*(?=/)" )
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
  }
}else{}
}