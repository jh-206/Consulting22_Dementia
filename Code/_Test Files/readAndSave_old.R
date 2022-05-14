
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

  years <- c(1, 3, 5, 7, 8, 11)  
  
  vars_needed_file <- file.path(getwd(), "MCCFAD grant Vars Needed.xlsx")
  
  
  for(yr in years){
    # Read in Vars needed spreadsheet
    sheetname <- paste0("HABC Yr ", yr)
    if(!sheetname %in% readxl::excel_sheets(vars_needed_file)){
      print(paste0("No Sheet for Year ", yr))
      next
    }
    vars_df <- suppressMessages(readxl::read_excel(vars_needed_file, sheet = sheetname, skip = 4))
    vars_df <- vars_df[!is.na(vars_df$`File name`),] # remove header rows
    # Clean Variables Needed Data
    vars_df$`File name` <- tolower(vars_df$`File name`)
    vars_df$`Var name` <- tolower(vars_df$`Var name`)
    
    # Get Unique File names
    needed_files <- unique(vars_df$`File name`)
    ## Clean names to readable files
    ### If "and" in name, get first one
    needed_files[grepl("and", needed_files)] <- gsub(" .*", "", needed_files[grepl("and", needed_files)])
    
    
    # HABC Directory
    temp_dir <- file.path(data_path, paste0("Year ", yr))
    ## Must run these lines in order
    files <- tools::file_path_sans_ext(list.files(temp_dir, pattern = "*.sas7bdat", full.names = F)) # file names to match
    paths <- list.files(temp_dir, pattern = "*.sas7bdat", full.names = T) # file paths to read
    
    paths <- paths[files %in% needed_files]
    files <- files[files %in% needed_files]
    
    for(i in 1:length(files)){
      temp_df <- haven::read_sas(paths[i])
      names(temp_df) <- tolower(names(temp_df))
      
      # Match variables in SAS data to excel sheet variables
      temp_vars <- vars_df$`Var name`[which(files[i] == vars_df$`File name`)]
      # Handle case of multiple variables in one cell
      if(length(temp_vars) == 1){
        if(grepl(" ", temp_vars)){
          # Remove line breaks, split by white space
          temp_vars <- strsplit(gsub("\\r|\\n", "", temp_vars), " +")[[1]]
        }
      }
      # Remove variables not in data, print message
      if(sum(!temp_vars %in% names(temp_df))>0){
        print(paste0("Variables not in Year ", yr, " directory: ", 
                     paste(temp_vars[which(!temp_vars %in% names(temp_df))], collapse = ", ")))
        temp_vars <- temp_vars[which(temp_vars %in% names(temp_df))] 
      }
      # Extract needed variables
      temp_df <- temp_df[,c("habcid", temp_vars)]
      
      if(i == 1){
        output_df <- temp_df
        # long_df <- temp_df
      } else{
        output_df <- dplyr::left_join(output_df, temp_df, by = "habcid")
        # long_df <- dplyr::bind_rows(long_df, output_df)
      }
    }
    
    output_name <- paste0("year", yr)
    # Save as RData File, need to create temporary environment to name variable correctly
    env <- new.env()
    env[[output_name]] <- output_df
    save(list=output_name, envir=env, file=file.path(output_path, paste0(output_name, ".RData"))) # Save RData to specified output path
    
    print(paste0("Finished with Year ", yr, ". N rows=", nrow(output_df)))
  }

# Handle Genetics Data ----------------------------------------------------

  # Genetics Data
  ## Only keep apoehap protein column
  dna <- haven::read_sas(file.path(data_path, "Genetics/dna.sas7bdat"))
  names(dna) <- tolower(names(dna))
  dna <- dna[,c("habcid", "apoehap")]
  
  save(dna, file = file.path(output_path, "dna.RData"))

  
  

# Handle Meds Data --------------------------------------------------------

  # Abbreviations for dementia meds, from SAS Code methodology
  dementia_med_abbrv <- c("donep", "arciept", "gelanta", "razadyne", "rivastig", 
                     "exelon", "excelon", "mema", "namenda", "namaenda")
  
  
  years <- c(1, 3, 5, 8, 11)  
  
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
  
  save(med_df, file = file.path(output_path, "Medication Info.RData"))
  
  dim(med_df)
  sum(duplicated(med_df$mifdate))
  
  
  med_df <- med_df %>% 
    group_by(habcid) %>%
    slice_min(mifdate, with_ties = FALSE)
  
  
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
  
  
  
  
  