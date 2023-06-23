
#' @Title Write output to aws
#'

#' @param dirs_local character; directory that output files are stored in

#' @return No data returned.

write_s3 <- function(dirs_local){
  ## write geotif file
#remotes::install_github("cboettig/minio")
#library(minio)
minio::install_mc()

  #Set ENV variable for minio
Sys.setenv("CPL_VSIL_USE_TEMP_FILE_FOR_RANDOM_WRITE"="YES")


 #Set alias for writing to bucket
minio::mc_alias_set("efi",  endpoint="minio.carlboettiger.info",
             access_key = Sys.getenv("AWS_ACCESS_KEY_ID"), secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY")

 cp_file <- paste0("cp -r", dirs_local, "efi/neon4cast-spatial")
  
 #Write all files to AWS
 lapply(cp_file, minio::mc())  
             
#write output to aws
#minio::mc("cp -r targets efi/neon4cast-spatial") #copy targets directory to s3
#minio::mc("cp -r climatology efi/neon4cast-spatial") 
#minio::mc("cp -r scores efi/neon4cast-spatial")

} #End function write aws
