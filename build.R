#!/usr/bin/env R

source("deps.R")

## Get Raw Data

files <- read_csv("data/files.csv")

for(i in 1:nrow(files)){
  if(!file.exists(paste0("data/", files$csv[i]))){
    download.file(files$url[i], paste0("data/", files$zip[i]))
    unzip( paste0("data/",files$zip[i]), files = files$csv[i], exdir = "data/")
    unlink(paste0("data/",files$zip[i]))
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

Outpatient12 <- read_csv("data/Medicare_Provider_Charge_Outpatient_APC30_CY2012.csv")    %>%
  mutate(year = 2012) %>%
  retitle(colnames(Outpatient11))

Outpatient13 <- read_csv("data/Medicare_Provider_Charge_Outpatient_APC30_CY2013.csv") %>%
  mutate(year = 2013) %>%
  retitle(colnames(Outpatient12))

Outpatient14 <- read_csv("data/Medicare_Provider_Charge_Outpatient_APC32_CY2014.csv") %>%
  mutate(year = 2014) %>%
  retitle(colnames(Outpatient13))

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

Inpatient <- rbind(Inpatient11, Inpatient12, Inpatient13, Inpatient14) %>%
  mutate(
    `Provider Id` = str_pad(`Provider Id`, 6, "left", "0"),
    `Provider Zip Code` = str_pad(`Provider Zip Code`, 5, "left", "0"),
    DRG = substring(`DRG Definition`, 1, 3),
    Procedure = substring(`DRG Definition`, 7) %>%
      str_to_title() %>%
      str_replace_all(list(
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
  select(`Provider Id`, year, DRG, performed:`Average Medicare Payments`) %>%
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
       Inpatient14, Inpatient13,
       Inpatient12, Inpatient11,
       Outpatient, OutpatientProviders,
       Outpatient14, Outpatient13,
       Outpatient12, Outpatient11)

StateCentroids <- read_csv("data/StateCentroids.csv")

save.image("data/Medicare_Data.rdata")
