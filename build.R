#!/usr/bin/env R

library("dplyr")
library("stringr")
library("readr")
library("readxl")
library("magrittr")

## Get Raw Data

files <- read_csv("data/files.csv")

for(i in 1:nrow(files)){
  if(!file.exists(paste0("data/", files$csv[i]))){
    download.file(files$url[i], paste0("data/", files$zip[i]))
    unzip( paste0("data/",files$zip[i]), exdir="data")
    file.remove(paste0("data/",files$zip[i]))
  }
}

retitle <- function(orig, titles){
  new <- orig
  colnames(new) <- titles
  new
}

## Outpatient Data

Outpatient11 <- read_csv("data/Medicare_Provider_Charge_Outpatient_APC30_CY2011_v2.csv") %>%
  mutate(year = 2011)

OutpatientColumns <- colnames(Outpatient11)

Outpatient12 <- read_csv("data/Medicare_Provider_Charge_Outpatient_APC30_CY2012.csv")    %>%
  mutate(year = 2012) %>%
  retitle(OutpatientColumns)

Outpatient13 <- read_csv("data/Medicare_Provider_Charge_Outpatient_APC30_CY2013.csv") %>%
  mutate(year = 2013) %>%
  retitle(OutpatientColumns)

Outpatient14 <- read_csv("data/Medicare_Provider_Charge_Outpatient_APC32_CY2014.csv") %>%
  mutate(year = 2014) %>%
  retitle(OutpatientColumns)

#TODO: Fix Schema to Match Outpatient13
Outpatient15 <- read_csv("data/Medicare_OPPS_CY2015_Provider_APC.csv") %>%
	mutate(year = 2015)

Outpatient16 <- read_csv("data/Medicare_OPPS_CY2016_Provider_APC.csv") %>%
	mutate(year = 2016)

Outpatient17 <- read_xlsx("data/MUP_OHP_R19_P04_V10_D17_APC_Provider.xlsx", skip=5) %>%
	mutate(year = 2017) %>%
	select(-Beneficiaries)

Outpatient <- rbind(Outpatient11, Outpatient12, Outpatient13, Outpatient14) %>%
  mutate(
    `Provider Id` = str_pad(`Provider Id`, 6, "left", "0"),
    `Provider Zip Code` = str_pad(`Provider Zip Code`, 5, "left", "0"),
    code = substr(APC, 0, 4),
    procedure = sub("(Level [[:alnum:]]*) (.*)", "\\2: \\1", substring(APC, 8), perl = TRUE) %>% 
      str_to_title() %>%
      str_replace_all(c(
        " & "   = " and ",
        " W "   = " with ",
        " W/O " = " without ",
        " Cc"   = " Complication or Co-morbidity",
        " Mcc"  = " Major Complication or Co-morbidity")),
  	`Average  Estimated Submitted Charges` = round(`Average  Estimated Submitted Charges`, 2),
  	`Average Total Payments` = round(`Average Total Payments`, 2)
  ) %>%
  rename(
    definition = APC,
    performed = `Outpatient Services`
  )

OutpatientData <- Outpatient %>%
  select(`Provider Id`, year, code, performed:`Average Total Payments`) %>%
  distinct %T>%
  write_csv("data/OutpatientProcedures.csv")

OutpatientCodes <- Outpatient %>%
  select(code, procedure) %>%
  distinct %T>%
  write_csv("data/OutpatientCodes.csv")

## Inpatient Data

Inpatient11 <- read_csv("data/Medicare_Provider_Charge_Inpatient_DRG100_FY2011.csv") %>%
  mutate(year = 2011)

Inpatient12 <- read_csv("data/Medicare_Provider_Charge_Inpatient_DRG100_FY2012.csv") %>%
  mutate(year = 2012)

Inpatient13 <- read_csv("data/Medicare_Provider_Charge_Inpatient_DRG100_FY2013.csv") %>%
  mutate(year = 2013)

Inpatient14 <- read_csv("data/Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv") %>%
  mutate(year = 2014)

Inpatient15 <- read_csv("data/Medicare_Provider_Charge_Inpatient_DRGALL_FY2015.csv") %>%
	mutate(year = 2015)

Inpatient16 <- read_csv("data/Medicare_Provider_Charge_Inpatient_DRGALL_FY2016.csv", col_types="cccccccccccc") %>%
	mutate(
		`Average Covered Charges`: parse_number(`Average Covered Charges`),
		`Average Total Payments`: parse_number(`Average Total Payments`),
		`Average Medicare Payments`: parse_number(`Average Medicare Payments`),
		year = 2016
	)

Inpatient17 <- read_csv("data/MEDICARE_PROVIDER_CHARGE_INPATIENT_DRGALL_FY2017.CSV") %>%
	mutate(year = 2017)

Inpatient <- rbind(Inpatient11, Inpatient12, Inpatient13, Inpatient14, Inpatient15, Inpatient17) %>%
  mutate(
    `Provider Id` = str_pad(`Provider Id`, 6, "left", "0"),
    `Provider Zip Code` = str_pad(`Provider Zip Code`, 5, "left", "0"),
    DRG = substring(`DRG Definition`, 1, 3),
    Procedure = substring(`DRG Definition`, 7) %>%
      str_to_title() %>%
      str_replace_all(c(
        " & "   = " and ",
        " W "   = " with ",
        " W/O " = " without ",
        " Cc"   = " Complication or Co-morbidity",
        " Mcc"  = " Major Complication or Co-morbidity")),
    `Average Covered Charges` = round(`Average Covered Charges`, 2),
  	`Average Total Payments`  = round(`Average Total Payments`, 2),
  	`Average Medicare Payments` = round(`Average Medicare Payments`, 2)) %>%
  rename(
    definition = `DRG Definition`,
    performed = `Total Discharges`
  )

InpatientData <- Inpatient %>%
  select(`Provider Id`, year, Procedure, performed:`Average Medicare Payments`) %>%
  distinct %T>%
  write_csv("data/InpatientProcedures.csv")

InpatientCodes <- Inpatient %>%
  select(DRG, Procedure) %>%
  distinct %>%
  arrange(DRG) %T>%
  write_csv("data/InpatientCodes.csv")

## Providers

Addresses <- read_csv("data/geoCoded.csv")

OutpatientProviders <- Outpatient %>%
  select(starts_with("Provider"), hrr=`Hospital Referral Region (HRR) Description`)

InpatientProviders <- Inpatient %>%
  select(starts_with("Provider"), hrr=`Hospital Referral Region (HRR) Description`)

Providers <- rbind(InpatientProviders, OutpatientProviders) %>%
  distinct(`Provider Id`, .keep_all = TRUE) %>%
  mutate(
    `Provider Zip Code` = str_pad(`Provider Zip Code`, 5, "left", "0"),
    address = paste(`Provider Street Address`, `Provider City`, `Provider State`, sep = ", "),
    `Provider Name` = str_to_title(`Provider Name`)
  ) %>%
  left_join(Addresses) %>%
  select(-address) %T>%
  write_csv("data/Providers.csv")

## Clean up! Clean up! Everybody, Everywhere!

remove(Addresses, files, retitle,
       Inpatient, InpatientProviders,
			 Inpatient17, Inpatient15,
       Inpatient14, Inpatient13,
       Inpatient12, Inpatient11,
       Outpatient, OutpatientProviders,
			 Outpatient16, Outpatient15,
       Outpatient14, Outpatient13,
       Outpatient12, Outpatient11)

StateCentroids <- read_csv("data/StateCentroids.csv")

save.image("data/Medicare_Data.rdata")
