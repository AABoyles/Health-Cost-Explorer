library(readr)
library(magrittr)
library(dplyr)
library(stringr)
library(RPostgreSQL)

source("creds.R")

con <- dbConnect(
  dbDriver("PostgreSQL"),
  host=server,
  port=port,
  user=username,
  password=password,
  dbname="postgres"
)

Outpatient13 <- read_csv("data/orig/Medicare_Provider_Charge_Outpatient_APC30_CY2013_v2.csv") %>%
  mutate(year=2013)
Outpatient12 <- read_csv("data/orig/Medicare_Provider_Charge_Outpatient_APC30_CY2012.csv")    %>%
  mutate(year=2012)
Outpatient11 <- read_csv("data/orig/Medicare_Provider_Charge_Outpatient_APC30_CY2011_v2.csv") %>%
  mutate(year=2011)

OutpatientData <- rbind(Outpatient11, Outpatient12, Outpatient13) %>%
  mutate(
    code = substr(APC, 0, 4),
    procedure = sub("(Level [[:alnum:]]*) (.*)", "\\2: \\1", substring(APC, 8), perl = TRUE) %>% str_replace("&", "and"),
    patience = "Outpatient") %>%
  rename(
    definition = APC,
    performed = `Outpatient Services`
  )

dbWriteTable(con,"Outpatient",data.frame(OutpatientData))

OutpatientData %<>% select(-`Average  Estimated Submitted Charges`)

Inpatient13 <- read_csv("data/orig/Medicare_Provider_Charge_Inpatient_DRG100_FY2013.csv") %>%
  mutate(year=2013)
Inpatient12 <- read_csv("data/orig/Medicare_Provider_Charge_Inpatient_DRG100_FY2012.csv") %>%
  mutate(year=2012)
Inpatient11 <- read_csv("data/orig/Medicare_Provider_Charge_Inpatient_DRG100_FY2011.csv") %>%
  mutate(`Average Medicare Payments`=NA, year=2011) %>%
  rename(
    `Hospital Referral Region (HRR) Description` = `Hospital Referral Region Description`,
    `Total Discharges`=` Total Discharges `,
    `Average Covered Charges`=` Average Covered Charges `,
    `Average Total Payments` = ` Average Total Payments `
  )

InpatientData <- rbind(Inpatient11, Inpatient12, Inpatient13) %>%
  mutate(
    code = substring(`DRG Definition`, 1, 3),
    procedure = substring(`DRG Definition`, 7) %>%
      str_to_title() %>%
#       str_replace("Cc", "Complication or co-morbidity") %>%
#       str_replace("Mcc", "Major Complication or co-morbidity") %>%
      str_replace_all(c(" W "=" with ", " W/O "=" without ", "&"="and")),
    patience = "Inpatient") %>%
  rename(
    definition = `DRG Definition`,
    performed = `Total Discharges`
  )

dbWriteTable(con,"Inpatient",data.frame(InpatientData))

InpatientData %<>% select(-`Average Covered Charges`, -`Average Medicare Payments`)

MedicareData <- rbind(OutpatientData, InpatientData) %>%
  rename(
    pid=`Provider Id`,
    atp=`Average Total Payments`
  )

Addresses <- read_csv("data/geoCoded.csv")

Providers <- MedicareData %>%
  select(pid, starts_with("Provider"), hrr=`Hospital Referral Region (HRR) Description`) %>%
  distinct %>%
  mutate(
    address = paste(`Provider Street Address`, `Provider City`, `Provider State`, sep = ", "),
    hospital = str_to_title(`Provider Name`)
  ) %>%
  left_join(Addresses) %T>%
  write_csv("data/Providers.csv")

dbWriteTable(con,"Providers",data.frame(Providers))

Procedures <- MedicareData %>%
  select(procedure, code) %>%
  distinct %T>%
  write_csv("data/Procedures.csv")

dbWriteTable(con,"Procedures",data.frame(Procedures))

MedicareData %<>%
  select(pid, performed, year, code, atp) %T>%
  write_csv("data/MedicareData.csv")

dbWriteTable(con,"MedicareData",data.frame(MedicareData))
