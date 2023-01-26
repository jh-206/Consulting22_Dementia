
# Environment -------------------------------------------------------------
  # Read in packages
  library(haven); library(dplyr); library(readxl)
  
  # Path with SAS Data directories
  base_path <- Sys.getenv("USERPROFILE")
  data_path <- file.path(base_path, "/The University of Colorado Denver/Rooks, Ronica - Health ABC data 10.15.2021/")
  
  # If not using R Project, set working directory manually
  if(F){
    setwd("...")
  }
  
  # Path to write out combined data
  output_path <- file.path(getwd(), "Data/")


# Read and Merge Longitudinal Data ----------------------------------------

  years <- c(1:11) 
  
  # Vector of naming conventions of needed variable. 
  ## Note: some Need to be changed for different years, using pattern matching
  vars_needed_yr1_vec <- c("habcid", "mmmscore", "fpvwcurj", "fpvwcurv", "faminc", "educ", "lpschool", 
                           "hqssrclo", "hqssfclo")
  vars_needed_vec <- c("habcid", "mmmscore", "curj", "wcurv",
                       "hqssrclo", "hqssfclo")
  
  # Function returns error if cannot read
  try_read_sas <- function(filename){
    require(haven)
    
    tryCatch(
      haven::read_sas(paste0(filename)),
      error = function(e) print(paste0("Read error for ", basename(filename)))
    )
  }
  
  for(yr in years){
    # HABC Directory
    temp_dir <- file.path(data_path, paste0("Year ", yr))
    ## Must run these lines in order
    files <- tools::file_path_sans_ext(list.files(temp_dir, pattern = "*.sas7bdat", full.names = F)) # file names to match
    paths <- list.files(temp_dir, pattern = "*.sas7bdat", full.names = T) # file paths to read
    
    if(yr == 1){
      vars_vec <- vars_needed_yr1_vec
    } else {
      vars_vec <- vars_needed_vec
    }
    
    # Remove dataframe for previous year
    suppressWarnings(rm(year_df))
    for(i in 1:length(files)){
      temp_df <- try_read_sas(paths[i])
      names(temp_df) <- tolower(names(temp_df))
      
      # Skip files that cause error in read
      if(class(temp_df)[1] == "character"){next}
      
      # Get list of needed variables in current file
      temp_vars <- grep(paste(vars_vec[-1], collapse = "|"), names(temp_df), value = T)
      
      # Save Variables
      if(length(temp_vars) == 0){next} else{temp_df <- temp_df[,c("habcid", temp_vars)]}
      # Merge
      if(!exists("year_df")){year_df = temp_df} else{year_df = dplyr::inner_join(year_df, temp_df, by = "habcid")}
    }
    if(!exists("year_df")){
      print(paste0("No needed variables found for year: ", yr))
      next
    }
    
    output_name <- paste0("year", yr)
    # Save as RData File, need to create temporary environment to name variable correctly
    env <- new.env()
    env[[output_name]] <- year_df
    save(list=output_name, envir=env, file=file.path(output_path, "Years", paste0(output_name, ".RData"))) # Save RData to specified output path
    
    print(paste0("Saving Year ", yr, ". N rows=", nrow(year_df)))
  }

  
  
  
  # Merge Files with Available Data
  year_files <- list.files(file.path(output_path, "Years"), full.names = T)
  for(f in year_files){load(f)}
  
  ## Rename and merge. Add year column
  year1$rfclose <- year1$hqssrclo + year1$hqssfclo # sum of social support vars
  year1 <- year1[,!names(year1) %in% c("hqssrclo", "hqssfclo")] # remove other 2 social vars
  colnames(year1) <- c("habcid", "mmmscore", "educ", "faminc", 
                       "work", "volunteer", "educ_year", "rfclose")
  year1$year <- 1
  year1$educ_year[is.na(year1$educ)] <- NA
  
  colnames(year3) <- c("habcid", "mmmscore", "work", "volunteer")
  year3$year <- 3
  
  colnames(year5) <- c("habcid", "mmmscore")
  year5$year <- 5
  
  colnames(year8) <- c("habcid", "mmmscore", "work", "volunteer")
  year8$year <- 8
  
  colnames(year9) <- c("habcid", "work", "volunteer")
  year9$year <- 9
  
  colnames(year10) <- c("habcid", "mmmscore", "work", "volunteer")
  year10$year <- 10
  
  colnames(year11) <- c("habcid", "mmmscore", "work", "volunteer")
  year11$year <- 11
  # Merge
  year_df <- dplyr::bind_rows(
    year1 %>% dplyr::select("habcid", "mmmscore", "work", "volunteer", "year"), 
    year3, year5, year8, year9, year10, year11
  )
  save(year_df, file = file.path(output_path, "Yearly Data.RData"))  
  
# Handle Patient Characteristics Data -------------------------------------

  # These are data that do not change over time (includes Age at year 1)

  
  # Genetics Data
  ## Only keep apoehap protein column
  dna <- haven::read_sas(file.path(data_path, "Genetics/dna.sas7bdat"))
  names(dna) <- tolower(names(dna))
  dna <- dna[,c("habcid", "apoehap")]
  
  # Patient History
  ## Keep race, ag
  ph <- haven::read_sas(file.path(data_path, "Year 1/ph.sas7bdat"))
  names(ph) <- tolower(names(ph))
  ph <- ph[,c("habcid", "race", "cv1age", "cv1date", "site", "gender")]
  
  patient_df <- dplyr::left_join(ph, dna, by = "habcid")
  patient_df <- dplyr::left_join(patient_df, year1 %>% select("habcid", "educ", "faminc", "rfclose"), 
                                 by = "habcid")

  save(patient_df, file = file.path(output_path, "Patient Data.RData"))  

# Handle Meds Data --------------------------------------------------------

  # Abbreviations for dementia meds, from SAS Code methodology
  dementia_med_abbrv <- c("donep", "arciept", "gelanta", "razadyne", "rivastig", 
                     "exelon", "excelon", "mema", "namenda", "namaenda")
  
  
  years <- c(1, 3, 5, 8, 10, 11)  
  
  suppressWarnings(remove(med_df))
  for(yr in years){
    print(paste0("Finished with year ", yr))
    # Read in Med Info data
    temp_df <- haven::read_sas(file.path(data_path, paste0("Year ", yr, "/", "y", yr, "mifcod.sas7bdat")))
    # Lower case For consistency
    names(temp_df) <- tolower(names(temp_df)) 
    temp_df$mifname <- tolower(temp_df$mifname)
    
    # Extract Dementia Medications
    dementia_meds <- grep(paste(dementia_med_abbrv, collapse = "|"), temp_df$mifname, value = T)
    if(length(dementia_meds) == 0){
      next
    }
    
    temp_df <- temp_df[which(temp_df$mifname %in% dementia_meds),]
    
    if(!exists("med_df")){
      med_df <- temp_df
    } else{
      med_df <- dplyr::bind_rows(med_df, temp_df)
    }
  }
  
  
  dim(med_df)
  sum(duplicated(med_df$mifdate))
  
  
  med_df <- med_df %>% 
    group_by(habcid) %>%
    slice_min(mifdate, with_ties = FALSE)
  
  save(med_df, file = file.path(output_path, "Medication Info.RData"))
  
# Handle Psych Data -------------------------------------------------------
  
  
  ### Making all column names lower case for consistency, some datasets are caps some are lower
  # Other Patient Data - data not in yearly subdirectories
  
  # Psychiatric outcomes data
  psych <- haven::read_sas(file.path(data_path, "Outcomes/psych.sas7bdat"))
  names(psych) <- tolower(names(psych))
  admdt_vars <- c("s1admdt", "s2admdt", "s3admdt", "s4admdt", "s5admdt", "s6admdt", "s7admdt",
                  "s8admdt", "s9admdt", "s10admdt", "s11admdt", "s12admdt", "s13admdt", "s14admdt") # hospital visit date
  dem_vars <- c("s1dem1", "s2dem1", "s3dem1", "s4dem1", "s5dem1", "s6dem1", "s7dem1", 
                "s8dem1", "s9dem1", "s10dem1", "s11dem1", "s12dem1", "s13dem1", "s14dem1") # dementia diagnosis 
  
  psych <- psych[,c("habcid", admdt_vars,dem_vars)]
  
  
  # Write Output
  save(psych, file = file.path(output_path, "Psych Data.RData"))
  
  
  # Find date of dementia diagnosis
  diagnosis_df <- data.frame(
    habcid = psych$habcid,
    dem_date = rep(NA, length(psych$habcid))
  )
  for(i in 1:length(diagnosis_df$habcid)){
    id = diagnosis_df$habcid[i]
    temp_df <- psych[which(psych$habcid == id),]
    temp_admdt <- names(which(!apply(temp_df[,admdt_vars], 2, is.na)))
    temp_dem <- names(which(!apply(temp_df[,dem_vars], 2, is.na)))
    if(length(temp_dem) == 0){
      next
    }else{
      diagnosis_df$dem_date[i] <- min(t(temp_df[,temp_admdt])[,1]) # Earliest diagnosis date
    }
  }
  
  diagnosis_df <- diagnosis_df[!is.na(diagnosis_df$dem_date),]
  
  save(diagnosis_df, file = file.path(output_path, "Diagnosis Data.RData"))
  
  

# Chronic Conditions ------------------------------------------------------

  ## Other chronic conditions
  df <- haven::read_sas(file.path(data_path, "Outcomes/previncdz.sas7bdat"))
  names(df) <- tolower(names(df))
  
  chronic_vars <- c("y1pcbvd", "y1pcvd", "y1phbp1", "y1pdepr1") # cerebral vascular disease variables
  df <- df[, names(df) %in% c("habcid", chronic_vars)]

  save(diagnosis_df, file = file.path(output_path, "Chronic Conditions Data.RData"))
  
    
  
  ## Diabetes
  # df <- haven::read_sas(file.path(data_path, "Outcomes/diabetes.sas7bdat"))
  # names(df) <- tolower(names(df))
  # 
  # "y1pdiab"