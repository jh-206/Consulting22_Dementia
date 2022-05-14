# Function returns error if cannot read
try_read_sas <- function(filename){
  require(haven)
  
  tryCatch(
    haven::read_sas(paste0(filename)),
    error = function(e) print(paste0("Read error for ", basename(filename)))
  )
}


# Read all sas files in directory
# Exclude: file names to not read in this step
readHABCSubdir <- function(path, exclude){
  require(haven)
  
  # Get all file names in a character vector
  files <- sort(list.files(path, pattern = "*.sas7bdat", full.names = T))
  # Don't read files matching exclude pattern
  files <- files[!grepl(pattern = paste0(exclude, collapse = "|"), basename(files))]
  
  # If no files, return error
  if(length(files) == 0){stop(paste0("No sas files in '", basename(path),"'"))}
  
  # Read each sas file and merge by HABC ID
  for(i in 1:length(files)){
    temp_df <- try_read_sas(files[i]) # create temporary dataframe, check whether to join
    names(temp_df) <- tolower(names(temp_df)) # upper case names for consistency
    if("habcid" %in% names(temp_df)) { 
      if(!exists("return_df")){ 
        return_df = temp_df # Create dataframe to return on first iteration
      }else{
        # join if it has HABCID column
        return_df <- dplyr::left_join(return_df, temp_df, by = "habcid")
      }
    }
  }
  invisible(gc()) # quietly garbage collect to clear memory for large loads
  return(return_df)
}




