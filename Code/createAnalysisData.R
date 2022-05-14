
# Environment -------------------------------------------------------------

  # Packages
  library(dplyr); library(lubridate)

  # If not using R Project, set working directory manually
  if(F){
    setwd("...")
  }
  
  # Path to write out combined data
  output_path <- file.path(getwd(), "Data/")
  

# Load Data ---------------------------------------------------------------

  files <- list.files(output_path, pattern = ".RData", full.names = T)  

  for(f in files){
    load(f)
  }
  

# Create Date of Dementia -------------------------------------------------
  
  # Create indicator for 3M<90, extract minimum year for each patient
  year_dem <- year_df %>% 
    mutate(mmm_dem = mmmscore < 90) %>% 
    filter(mmm_dem) %>% 
    group_by(habcid) %>% 
    summarise(dem_year = min(year))
  
  
  diag_dem <- diagnosis_df %>%
    left_join(patient_df %>% select("habcid", "cv1date"), by = "habcid") %>% 
    mutate(dem_year = lubridate::year(dem_date) - lubridate::year(cv1date) + 1) %>% 
    select("habcid", "dem_year")
  
  med_df$dem_year <- lubridate::year(med_df$mifdate)
  med_dem <- med_df %>% 
    left_join(patient_df %>% select("habcid", "cv1date"), by = "habcid") %>% 
    mutate(dem_year = lubridate::year(mifdate) - lubridate::year(cv1date) + 1) %>% 
    select("habcid", "dem_year")
  
  # Combine Dementia Years
  dem_df <- dplyr::bind_rows(
    year_dem, diag_dem, med_dem
    )
  
  # Find minimum Year per patient
  dem_df <- dem_df %>%
    group_by(habcid) %>%
    slice_min(dem_year, with_ties = F)
  
  # Exclude those years after 11
  dem_df <- dem_df[dem_df$dem_year <= 11,]
  
  # Create Dementia Column in yearly df
  dem_df$dementia <- 1
  
  # Encode work variable
  year_df$work[which(year_df$work > 1)] <- 1

  
# Merge Patient Data ------------------------------------------------------
  
  df <- dplyr::right_join(
    dem_df, patient_df, by = "habcid"
  )
  # Handle dementia variable
  df$dementia[is.na(df$dementia)] <- 0
  df$dem_year[df$dementia == 0] <- 11

# Handle Work Variable ----------------------------------------------------
  
  # Total Years worked
  total_work <- year_df %>% 
    left_join(dem_df %>% select(habcid, dem_year), by = "habcid") %>% 
    group_by(habcid) %>% 
    summarise(
      total_work_years = sum(work, na.rm = T),
      years_worked_pre_dem = sum(work[year<dem_year], na.rm = T),
      years_worked_post_dem = sum(work[year>=dem_year], na.rm = T)
      )
  
  
  

  total_work$any_work <- total_work$total_work_years != 0  
  
  # Join data
  df <- dplyr::left_join(
    df, total_work, by = "habcid"
  )

# Save Data ---------------------------------------------------------------

  save(df, file = file.path(output_path, "Analysis Data.RData"))
  