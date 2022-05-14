#upload packages
install.packages("labelled")
install.packages("tidyverse")
install.packages("haven")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("magrittr")

library(labelled)
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
library(magrittr)

#Read and merge the data from the different years
setwd("C:/Users/rrooks/OneDrive - The University of Colorado Denver/Rooks Work laptop/Professional/MCCFAD grant proposal/MCCFAD Statistics Team work with R Rooks/Health ABC data 10.15.2021/")
temp1 = list.files(path="Year 1",pattern="*.sas7bdat")
temp10 = list.files(path="Year 10",pattern="*.sas7bdat")
temp3 = list.files(path ="Year 3",pattern = "*.sas7bdat")
temp5 = list.files(path="Year 5",pattern="*.sas7bdat")
temp7 = list.files(path ="Year 7",pattern = "*.sas7bdat")
temp8 = list.files(path="Year 8",pattern="*.sas7bdat")
temp9 = list.files(path="Year 9",pattern="*.sas7bdat")
temp11 = list.files(path="Year 11",pattern="*.sas7bdat")
temp12 = list.files(path="Year 12",pattern="*.sas7bdat")
temp13 = list.files(path="Year 13",pattern="*.sas7bdat")
temp16 = list.files(path="Year 16",pattern="*.sas7bdat")
tempoutcm = list.files(path="Outcomes",pattern="*.sas7bdat")
tempsubstdy = list.files(path="Substudies",pattern="*.sas7bdat")
tempgenetics = list.files(path="Genetics",pattern="*.sas7bdat")

#Read the data files from SAS into R
setwd("Year 1")
list2env(lapply(setNames(temp1, make.names(gsub("*.sas7bdat$","", temp1))), read_sas), envir = .GlobalEnv)

setwd("../Year 10")
list2env(lapply(setNames(temp10, make.names(gsub("*.sas7bdat$","", temp10))), read_sas), envir = .GlobalEnv)

setwd("../Year 3")
list2env(lapply(setNames(temp3, make.names(gsub("*.sas7bdat$","", temp3))), read_sas), envir = .GlobalEnv)

setwd("../Year 5")
list2env(lapply(setNames(temp5, make.names(gsub("*.sas7bdat$","", temp5))), read_sas), envir = .GlobalEnv)

setwd("../Year 7")
list2env(lapply(setNames(temp7, make.names(gsub("*.sas7bdat$","", temp7))), read_sas), envir = .GlobalEnv)

setwd("../Year 8")
list2env(lapply(setNames(temp8, make.names(gsub("*.sas7bdat$","", temp8))), read_sas), envir = .GlobalEnv)

setwd("../Year 9")
list2env(lapply(setNames(temp9, make.names(gsub("*.sas7bdat$","", temp9))), read_sas), envir = .GlobalEnv)

setwd("../Year 11")
list2env(lapply(setNames(temp11, make.names(gsub("*.sas7bdat$","", temp11))), read_sas), envir = .GlobalEnv)

setwd("../Year 12")
list2env(lapply(setNames(temp12, make.names(gsub("*.sas7bdat$","", temp12))), read_sas), envir = .GlobalEnv)

setwd("../Year 13")
list2env(lapply(setNames(temp13, make.names(gsub("*.sas7bdat$","", temp13))), read_sas), envir = .GlobalEnv)

setwd("../Year 16")
list2env(lapply(setNames(temp16, make.names(gsub("*.sas7bdat$","", temp16))), read_sas), envir = .GlobalEnv)

setwd("../Outcomes")
list2env(lapply(setNames(tempoutcm, make.names(gsub("*.sas7bdat$","", tempoutcm))), read_sas), envir = .GlobalEnv)

setwd("../Substudies")
list2env(lapply(setNames(tempsubstdy, make.names(gsub("*.sas7bdat$","", tempsubstdy))), read_sas), envir = .GlobalEnv)

setwd("../Genetics")
list2env(lapply(setNames(tempgenetics, make.names(gsub("*.sas7bdat$","", tempgenetics))), read_sas), envir = .GlobalEnv)


# Merge the data for year 1
y1_ds <- left_join(ph, previncdz, by="HABCID") %>%
  left_join(., y1calc, by="HABCID") %>% 
  left_join(., y1clnvis, by="HABCID") %>%
#  left_join(., y1mif, by="HABCID") %>%
#  left_join(., y1mifcod, by="HABCID") %>%
#  left_join(., y1rxcalc, by="HABCID") %>%
#  left_join(., sixmovis, by="HABCID") %>%
#  left_join(., y1read, by="HABCID") %>%
  left_join(., y1screen, by="HABCID") %>%
  left_join(., dna, by="HABCID") %>%
#below line includes DVs, IVs  
  select("HABCID", "MMMSCORE", "MMMFLAG", "DSS", "P4NC", "P4NI", "P4TST", "FPVWCURJ", "FPVWAHWR", "FPVWMOW", "FPVWCURV", "FPVWAHVW", 'FPVWMOV',
#below line includes CVs, APOEHAP for Apolipoprotein-e4 from Genetics folder and DNA sas file
  "RACE", "GENDER", "CV1AGE", "SITE", "HQSSOPIH", "EDUC", "FAMINC", "BMI", "MADATE", "Y1WTK",
  "BQDA12MO", "BQSCCGNO", "BQSCSNOW", "FPAC12MO", "FPHI12MO", "FPMI12MO",
  "HQSSRCLO", "HQSSFCLO", "APOEHAP")
#below line includes all variables to create count of chronic conditions
#  "Y1PCHD1", "Y1PCHD2", "Y1PCHD3", "Y1PCHF", "Y1PPAD", "Y1PDEPR1", "Y1PDIAB1", 
#  "Y1PGALLS", "Y1PHERNI", "Y1PGIBLD", "Y1PULCER", "Y1PHBP1", "Y1POAHIP", "Y1POAHND", "Y1POAKN",
#  "Y1POAOTH", "Y1POSTP1", "Y1PPROST", "Y1PPULCD")

#1/21/22
#Error: Can't subset columns that don't exist.
#x Column `CV1DATE` doesn't exist. 
#Variable exists in PH data file but delete var for now - CV1DATE
#same message with all diseases in previncdz data file so commented them out

view(y1_ds)
dim(ph)

#Above merge data code works now
#fixed problem with Wassim's help - due to noscreen and screen data files not
#having a HABCID merge var

#this code from Wassim allows me to type variable names in lowercase
#bc R is case sensitive otherwise
y1_ds = y1_ds %>% mutate_all(.,.funs=tolower)
#I will need to use above code for each merged year data if I want to only use lower case text

 
mmmscore <- y1_ds[,"mmmscore"]  

summary(mmmscore)
hist(MMMSCORE)


# Merge the data for year 1 + Outcomes habc_dementia data file - NOT working
# I want to merge these just to get some results for my AAIC abstract for the RCMAR symposium Toni's organizing
y1plus <- left_join(y1_ds, habc_dementia, by="HABCID")

view(y1plus)
dim(y1plus)
summary(y1plus)


# Fit the logistic regression model
y1plus <- glm(dementia ~ FPVWCURJ, data = y1plus.data, family = binomial)
# Summarize the model
summary(y1plus)

summary(y1_ds$mmmscore) 
hist(y1_ds$mmmscore)

# Merge the data for year 3
y3_ds <- left_join(dxrean13, sa30mo, by="HABCID") %>%
  left_join(., sa30prox, by="HABCID") %>% 
  left_join(., y3calc, by="HABCID") %>%
  left_join(., y3clnvis, by="HABCID") %>%
  left_join(., y3corehv, by="HABCID") %>%
#  left_join(., y3mif, by="HABCID") %>%
#  left_join(., y3mifcod, by="HABCID") %>%
#  left_join(., y3proxy, by="HABCID") %>% 
#  left_join(., y3read, by="HABCID") %>%
#  left_join(., y3rxcalc, by="HABCID") %>%
  left_join(., y3cogvit, by="HABCID") %>%
#below line includes DVs, IVs 
  select("HABCID", "MMMSCORE", "MMMFLAG", "C4TMM", "REALM", "C2REALM", "C4REALM", "CLOX1", "C4CLOX1", "CLVWCURJ", "CLVWAHWR", "CLVWMOW", "CLVWCURV", "CLVWAHVW", 'CLVWMOV',
#below line includes CVs
       "RACE", "GENDER", "CV3AGE", "SITE", "CMSSOPIH", "CV3DATE", "MADATE", "Y3WTK",
       "BMI", "SMK3", "CMSSFFST", "CMSSFFH", "CLHI12MO", "CLEW12MO", "CKSCFRND", "CKSCREL",
#below line includes all variables to create count of chronic conditions
       "CLHCHBP", "ZCHCHBP", "CLSGDIAB", "ZCSGDIAB", "CLHCHAMI", "CLHCCVA", "CLCHF", "CLCHMGMT")


# Merge the data for year 5
y5_ds <- left_join(dxrean15, sa54mo, by="HABCID") %>%
  left_join(., sa54prox, by="HABCID") %>% 
  left_join(., y5calc, by="HABCID") %>%
  left_join(., y5clnvis, by="HABCID") %>%
  left_join(., y5corehv, by="HABCID") %>%
#  left_join(., y5mif, by="HABCID") %>%
#  left_join(., y5mifcod, by="HABCID") %>%
#  left_join(., y5proxy, by="HABCID") %>% 
#  left_join(., y5read, by="HABCID") %>%
#  left_join(., y5rxcalc, by="HABCID")
  left_join(., y5cogvit, by="HABCID") %>%
#below line includes DVs, IVs 
  select("HABCID", "MMMSCORE", "MMMFLAG", "MMMIMP", "MMMPHONE", "E1TMM", "EITMM", "DSS15", "DSS", "E1DSS", "EIDSS", "CLOX1", "E1CLOX", "EICLOX", "EDVWCURJ", "EDVWAHWR", "EDVWMOW", "EDVWCURV", "EDVWAHVW", 'EDVWMOV',
#below line includes CVs
        "RACE", "GENDER", "FAMINC", "CV5AGE", "SITE", "CV5DATE", "MADATE", "Y5WTK",
        "BMI", "SMK5", "EBSSFFH", "EBSFHDR", "EBSFHDR", "EBSSFFST", "EBSSFFDR", "EBEW12MO", "EBHI12MO",
        "EDSCFRND", "EDSCREL",
#below line includes all variables to create count of chronic conditions
        "EBHCHBP", "EBSGDIAB", "EBLCASTH", "EBLCEMPH", "EBLCCOPD", "EBLCCHBR", "EBHCHAMI", "EBHCCVA", "ZCCHF", "ZCCHMGMT")



#Next steps - 1.20.2022
#1. DID - Updated R to 4.1.2 version and R Studio versions bc warning message after running library(tidyverse) needed update 
#2. IN PROGRESS - Add in select vars to keep for each year as a line added to the merges for each year
#3. Run analyses for sample sizes and descriptive info for 4 DVs across years, especially substudy years

# Merge the data for year 7
y7_ds <- left_join(., sa78mo, by="HABCID") %>% 
  left_join(., sa78prox, by="HABCID") %>% 
  left_join(., y7calc, by="HABCID") %>%
  left_join(., y7phone, by="HABCID") %>%
  left_join(., y7proxy, by="HABCID")

# Merge the data for year 8
y8_ds <- left_join(., dxrean18, by="HABCID") %>%
  left_join(., sa90mo, by="HABCID") %>% 
  left_join(., sa90prox, by="HABCID") %>% 
  left_join(., y8calc, by="HABCID") %>%
  left_join(., y8mif, by="HABCID") %>%
  left_join(., y8mifcod, by="HABCID") %>%
  left_join(., y8proxy, by="HABCID") %>% 
  left_join(., y8read, by="HABCID") %>%
  left_join(., y8rxcalc, by="HABCID") %>%
  left_join(., y8visit, by="HABCID")

# Merge the data for year 9
y9_ds <- left_join(., sa102mo, by="HABCID") %>% 
  left_join(., sa102prox, by="HABCID") %>% 
  left_join(., y9calc, by="HABCID") %>%
  left_join(., y9phone, by="HABCID") %>%
  left_join(., y9proxy, by="HABCID")

# Merge the data for year 10
y10_ds <- left_join(., dxrean110, by="HABCID") %>%
  left_join(., sa114mo, by="HABCID") %>% 
  left_join(., sa114prox, by="HABCID") %>% 
  left_join(., y10calc, by="HABCID") %>%
  left_join(., y10mif, by="HABCID") %>%
  left_join(., y10mifcod, by="HABCID") %>%
  left_join(., y10proxy, by="HABCID") %>% 
  left_join(., y10read, by="HABCID") %>%
  left_join(., y10rxcalc, by="HABCID") %>%
  left_join(., y10visit, by="HABCID")

# Merge the data for year 11
y11_ds <- left_join(., sa126mo, by="HABCID") %>% 
  left_join(., sa126prox, by="HABCID") %>% 
  left_join(., y11calc, by="HABCID") %>%
  left_join(., y11mif, by="HABCID") %>%
  left_join(., y11mifcod, by="HABCID") %>%
  left_join(., y11proxy, by="HABCID") %>% 
  left_join(., y11read, by="HABCID") %>%
  left_join(., y11rxcalc, by="HABCID") %>%
  left_join(., y11visit, by="HABCID")

# Merge the data for year 12
y12_ds <- left_join(., sa138mo, by="HABCID") %>% 
  left_join(., sa138prox, by="HABCID") %>% 
  left_join(., y12calc, by="HABCID") %>%
  left_join(., y12phone, by="HABCID") %>%
  left_join(., y12proxy, by="HABCID")

# Merge the data for year 13
y13_ds <- left_join(., sa150mo, by="HABCID") %>% 
  left_join(., sa150prox, by="HABCID") %>% 
  left_join(., y13calc, by="HABCID") %>%
  left_join(., y13phone, by="HABCID") %>%
  left_join(., y13proxy, by="HABCID")

# Merge the data for year 16
y16_ds <- left_join(., dxrean116, by="HABCID") %>% 
  left_join(., y16cvcalc, by="HABCID") %>%
  left_join(., y16hipaccellog, by="HABCID" %>%
  left_join(., y16hipaccelsumppt, by="HABCID" %>%
  left_join(., y16hipaccelsumpptday, by="HABCID" %>%
  left_join(., y16mif, by="HABCID") %>%
  left_join(., y16mifcod, by="HABCID") %>%
  left_join(., y16q1_calc, by="HABCID") %>% 
  left_join(., y16q2_calc, by="HABCID") %>% 
  left_join(., y16q3_calc, by="HABCID") %>% 
  left_join(., y16q4_calc, by="HABCID") %>% 
  left_join(., y16q1_ppt, by="HABCID") %>% 
  left_join(., y16q2_ppt, by="HABCID") %>%
  left_join(., y16q3_ppt, by="HABCID") %>%
  left_join(., y16q4_ppt, by="HABCID") %>%
  left_join(., y16q1_proxy, by="HABCID") %>% 
  left_join(., y16q2_proxy, by="HABCID") %>%
  left_join(., y16q3_proxy, by="HABCID") %>%
  left_join(., y16q4_proxy, by="HABCID") %>%
  left_join(., y16read, by="HABCID") %>%
  left_join(., y16rxcalc, by="HABCID") %>%
  left_join(., y16workbooks, by="HABCID")


#upload package for Linear and nonlinear mixed effects models
install.packages("nlme")

library(nlme)



