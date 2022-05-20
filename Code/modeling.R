
# Environment -------------------------------------------------------------

  # Packages
  library(dplyr); library(survival); library(ggplot2); library(survminer)
  
  # If not using R Project, set working directory manually
  if(F){
    setwd("...")
  }
  
  # Paths
  data_path <- file.path(getwd(), "Data/")
  output_path <- file.path(getwd(), "Outputs/")


# Read --------------------------------------------------------------------

  load(file.path(data_path, "Analysis Data.RData"))

# Filter and Transform ----------------------------------------------------
  
  df2 <- df
  
  # Filter out missing data completely
  sapply(df, function(col) sum(is.na(col)))
  print(sum(!complete.cases(df2)))
  df2 <- df2[complete.cases(df2),]
  
  # Recode factors
  df2$any_work <- as.numeric(df2$any_work)
  df2$apo4 <- as.numeric(df2$apoehap %in% c(5,6))
  df2 <- df2 %>% mutate_at(.vars = c("any_work", "site", "gender", "apoehap", "educ", "faminc", "race"),
                    as.factor)
  
  
  # Remove patients w dementia in year 1
  print(sum(df2$dem_year == 1))
  
  df2 <- df2[df2$dem_year!=1,]
  
  print(nrow(df)-nrow(df2))

  
  
# Plots and Tables --------------------------------------------------------
  
  data_tab <- data.frame(data.frame(
    "Original Data" = nrow(df),
    "Complete Data" = sum(!complete.cases(df)),
    "Dementia Free Year One" = nrow(df2), check.names = F
  ) %>% t())
  names(data_tab) <- NULL
  data_tab[,1] <- scales::comma(data_tab[,1])
  
  
  format_num_pct <- function(x, val = 1){
    paste0(scales::comma(sum(x==val))," (",c(scales::percent(mean(x==val))), ")")
  }
  
  table1 <- df2 %>% 
    group_by(`Paid Work` = any_work) %>% 
    summarise(
      `Number of Participants` = scales::comma(n()),
      `White race` = format_num_pct(race),
      `Mean Age` = round(mean(cv1age), 2),
      `Rate of Incident Dementia` = format_num_pct(dementia),
      `Mean Onset Year of Dementia` = round(mean(dem_year[dementia==1]), 2)
    ) 
  table1$`Paid Work` <- c("No", "Yes")
  table1 <- data.table::transpose(table1, keep.names = "Paid Work")
  colnames(table1) <- NULL
  knitr::kable(table1)
  
  
  work_tab <- df2 %>%
    filter(any_work==1) %>% 
    group_by(Race = race) %>% 
    summarise(
      `N` = scales::comma(n()),
      `Dementia Diagnosis` = format_num_pct(dementia)
      ) %>% 
    mutate(Race = c("Black", "White"))
  knitr::kable(work_tab)
  
# Cox PH Models ------------------------------------------------------------

  
  # Model with any work over 11 year period as predictor compared to
  # Model interacting race with any work
  model1 <- coxph(Surv(time = dem_year, event = dementia) ~ cv1age + site + 
                    gender + apo4 + educ + faminc +
                    race+any_work, 
                  data = df2)
  model2 <- coxph(Surv(time = dem_year, event = dementia) ~ cv1age + site + 
                    gender + apo4 + educ + faminc +
                    race*any_work, 
                  data = df2)
  
  
  
  # Compare log likelihood of models
  anova(model1, model2)
  
  mod_summary <- summary(model2)

# Model Outputs -----------------------------------------------------------


  model_df <- data.frame(mod_summary$coefficients)
  model_df <- round(model_df, 3)
  
  save(model2, file = file.path(output_path, "FinalModel.RData"))
  save(df2, file = file.path(output_path, "FinalModelingData.RData"))
    
# Model Diagnostics -------------------------------------------------------
  
  ## Proportional hazards assumption
  
  (xx <- cox.zph(model2))
  
  
  p1 <- ggcoxzph(xx)
  p2 <- ggcoxdiagnostics(model2)
  
  

  
  
  
  
  
  