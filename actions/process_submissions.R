## Send from submissions to forecasts

minioclient::mc_alias_set(alias = "efi", 
                          endpoint = "data.ecoforecast.org", 
                          access_key = Sys.getenv("AWS_ACCESS_KEY_ID"), 
                          secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"))

transfer_output <- minioclient::mc_mv(from = "efi/spat4cast-submissions/",
                                      to = "efi/spat4cast-forecasts/",
                                      recursive = TRUE)
