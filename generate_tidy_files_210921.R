library(forcats)
library(janitor)
library(mice)
library(psych)
library(tidyverse)

source('merge.mids.R')
source('roc.R')
source('prediction_functions_DMnotes.R')

load(file = "DAACS-WGU.rda")
load(file = "DAACS-EC.rda")
wgu_raw <- daacs.wgu[,c(1,3,4,6,11:14,18,29,39,44,56,58,60,61,64,67:76,78:87)]
ec_raw <- daacs.ec[,c(1,4,5,8:14,16,17,21:23,25:27,29,30,32,37:40,44,55,65,82,84:91)]

# EC

#Drop students who attempted 0 credits in term 1 and 0 credits in term 2
ec_raw <- ec_raw[!(ec_raw$CreditsAttempted_Term1 == 0 & ec_raw$CreditsAttempted_Term2 == 0),]

#Drop students who completed no DAACS
ec_raw <- ec_raw[!(is.na(ec_raw$srlTotal) & is.na(ec_raw$mathTotal) & is.na(ec_raw$readTotal) & is.na(ec_raw$writeTotal)),]

#Simplify employment_lvl_code because some categories have very few members

ec_raw$EMPLOYMENT_LVL_CODE[ec_raw$EMPLOYMENT_LVL_CODE == "1"] <- "not-employed"
ec_raw$EMPLOYMENT_LVL_CODE[ec_raw$EMPLOYMENT_LVL_CODE %in% c("2", "3", "4")] <- "employed"

table(ec_raw$EMPLOYMENT_LVL_CODE)

no.college <- c("01-HS_DNF", "02-HS_GRAD", "03-SOMECOL")
ec_raw$First_Generation <- ec_raw$HIGHEST_ED_LVL_CODE_FATHER %in% no.college & ec_raw$HIGHEST_ED_LVL_CODE_MOTHER %in% no.college

ec_raw$ETHNICITY <- fct_collapse(ec_raw$ETHNICITY, White = "White", Black = "Black or African American", Hispanic = "Hispanic", Asian = "Asian", Other = c("American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander", "Two or more races", "Unknown"))
ec_raw$ETHNICITY <- relevel(ec_raw$ETHNICITY, "White")

ec_raw$ACTIVE_MIL_STUDENT[ec_raw$MVET] <- FALSE

ec_raw$Initial_TRANSFER_CREDITS_EARNED[is.na(ec_raw$Initial_TRANSFER_CREDITS_EARNED)] <- 0

ec_raw$Retained <- ec_raw$CreditsAttempted_Term2 > 0

#SET VECTOR TYPE FOR CATEGORICAL VARIABLES INCOME_RANGE_CODE, EMPLOYMENT_LVL_CODE, SCHOOL_CODE.

ec_raw$INCOME_RANGE_CODE <- factor(ec_raw$INCOME_RANGE_CODE, levels = c("01-LT25K", "02-LT35K", "03-LT45K", "04-LT55K", "05-LT70K", "06-LT85K", "07-LT100K", "08-LT120K", "09-GE120K"), ordered = TRUE)

ec_raw$EMPLOYMENT_LVL_CODE <- factor(ec_raw$EMPLOYMENT_LVL_CODE, levels = c("not-employed", "employed"), ordered = TRUE)

ec_raw$SCHOOL_CODE <- factor(ec_raw$SCHOOL_CODE, levels = c("SBT", "SHS", "SLA", "SON", "SPS"), ordered = FALSE)

#Impute with MICE

ec_mice <- mice(ec_raw[c("INCOME_RANGE_CODE", "EMPLOYMENT_LVL_CODE", "ENGLISH_LANGUAGE_NATIVE", "ETHNICITY", "GENDER", "ACTIVE_MIL_STUDENT", "MVET", "SCHOOL_CODE", "DIVISION_CODE", "DEGREE_CODE", "Age")], m = 5)

ec_imp <- merge.mids(ec_mice, ec_raw, shadow.matrix = TRUE)

write_csv(ec_imp[,c(1:39)], file = "ec_tidy.csv")

#WGU

#Drop students who attempted 0 credits in term 1 and 0 credits in term 2
wgu_raw <- wgu_raw[!(wgu_raw$CreditsAttempted_Term1 == 0 & wgu_raw$CreditsAttempted_Term2 == 0),]

#drop students who did not complete DAACS
wgu_raw <- wgu_raw[!(is.na(wgu_raw$srlTotal) | is.na(wgu_raw$mathTotal) | is.na(wgu_raw$readTotal) | is.na(wgu_raw$writeTotal)),]

hifreq_program_codes <- c('BAISK8', 'BSAC', 'BSBUHCM', 'BSBUHR', 'BSBUITM', 'BSDMDA', 'BSHIM', 'BSIT', 'BSITNW', 'BSITSEC', 'BSITSW', 'BSMG', 'BSMK', 'BSNOS')

wgu_raw$CURRENT_PROGRAM_CODE <- fct_other(wgu_raw$CURRENT_PROGRAM_CODE, keep = hifreq_program_codes, other_level = 'other')

wgu_raw$ETHNICITY2 <- fct_collapse(wgu_raw$ETHNICITY2, White = "White", Black = "Black", Hispanic = "Hispanic", Asian = "Asian", Other = c("Am. Indian or Alaskan Native", "Multiple", "Native Hawaiian"))
wgu_raw$ETHNICITY2 <- relevel(wgu_raw$ETHNICITY2, "White")

#Convert back to character in order to update values. Then later convert back to factor.

wgu_raw$CITIZENSHIP_STATUS <- as.character(wgu_raw$CITIZENSHIP_STATUS)
wgu_raw$CITIZENSHIP_STATUS[wgu_raw$CITIZENSHIP_STATUS == "Non-Resident Alien"] <- "Non-Resident-Alien"
wgu_raw$CITIZENSHIP_STATUS[wgu_raw$CITIZENSHIP_STATUS == "U.S. Citizen"] <- "US-Citizen"

wgu_raw$TRANSFER_CREDITS[is.na(wgu_raw$TRANSFER_CREDITS)] <- 0

wgu_raw$Retained <- wgu_raw$CreditsAttempted_Term2 > 0

#SET VECTOR TYPE FOR CATEGORICAL VARIABLES CITIZENSHIP_STATUS, EMPLOYMENT_STATUS, GENDER, HOUSEHOLD_INCOME

wgu_raw$CITIZENSHIP_STATUS <- factor(wgu_raw$CITIZENSHIP_STATUS, levels = c("Non-Citizen", "Non-Resident-Alien", "US-Citizen"), ordered = FALSE)
wgu_raw$EMPLOYMENT_STATUS <- factor(wgu_raw$EMPLOYMENT_STATUS, levels = c("Unemployed", "Part-Time", "Full-Time"), ordered = TRUE)
wgu_raw$GENDER <- factor(wgu_raw$GENDER, levels = c("Female", "Male"), ordered = FALSE)
wgu_raw$HOUSEHOLD_INCOME <- factor(wgu_raw$HOUSEHOLD_INCOME, levels = c("<16k", "16-25k", "25-35k", "35-45k", "45-65k", ">65k"), ordered = TRUE)

#Impute with MICE

wgu_mice <- mice(wgu_raw[c("CURRENT_PROGRAM_CODE", "Age", "MilitaryStudent", "CITIZENSHIP_STATUS", "EMPLOYMENT_STATUS", "FIRST_GEN_STUDENT", "GENDER", "HOUSEHOLD_INCOME", "ETHNICITY2", "Hispanic")], m = 5)

wgu_imp <- merge.mids(wgu_mice, wgu_raw, shadow.matrix = TRUE)

write_csv(wgu_imp[,c(1:38)], file = "wgu_tidy.csv")