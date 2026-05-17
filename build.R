#!/usr/bin/env R

library("dplyr")
library("stringr")
library("readr")
library("readxl")
library("magrittr")
library("jsonlite")

## ── Helpers ───────────────────────────────────────────────────────────────────

retitle <- function(orig, titles) { colnames(orig) <- titles; orig }

# Flexibly find a column by regex patterns; returns the first match's vector
pick_col <- function(df, ...) {
  for (p in c(...)) {
    m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
    if (length(m)) return(df[[m[1]]])
  }
  NA_character_
}

# Map new-format CMS inpatient columns (2017 xlsx / 2018+) to legacy names
normalize_inpatient <- function(df) {
  if (!"Rndrng_Prvdr_CCN" %in% names(df)) return(df)
  transmute(df,
    `Provider Id`          = as.character(Rndrng_Prvdr_CCN),
    `Provider Name`        = Rndrng_Prvdr_Org_Name,
    `Provider Street Address` = Rndrng_Prvdr_St,
    `Provider City`        = Rndrng_Prvdr_City,
    `Provider State`       = Rndrng_Prvdr_State_Abrvtn,
    `Provider Zip Code`    = as.character(Rndrng_Prvdr_Zip5),
    `Hospital Referral Region (HRR) Description` = NA_character_,
    `DRG Definition`       = paste0(str_pad(as.character(DRG_Cd), 3, "left", "0"),
                                    " - ", DRG_Desc),
    `Total Discharges`           = as.numeric(Tot_Dschrgs),
    `Average Covered Charges`    = as.numeric(Avg_Submtd_Cvrd_Chrg),
    `Average Total Payments`     = as.numeric(Avg_Tot_Pymt_Amt),
    `Average Medicare Payments`  = as.numeric(Avg_Mdcr_Pymt_Amt)
  )
}

# Map new-format CMS outpatient columns to legacy names
normalize_outpatient <- function(df) {
  if (!"Rndrng_Prvdr_CCN" %in% names(df)) return(df)
  transmute(df,
    `Provider Id`          = as.character(Rndrng_Prvdr_CCN),
    `Provider Name`        = Rndrng_Prvdr_Org_Name,
    `Provider Street Address` = pick_col(df, "^Rndrng_Prvdr_St$", "Street"),
    `Provider City`        = Rndrng_Prvdr_City,
    `Provider State`       = pick_col(df, "State_Abrvtn", "State_Cd"),
    `Provider Zip Code`    = as.character(pick_col(df, "Zip5", "Zip_Cd")),
    `Hospital Referral Region (HRR) Description` = NA_character_,
    APC                    = paste0(str_pad(as.character(APC_Cd), 4, "left", "0"),
                                    " - ", APC_Desc),
    `Outpatient Services`  = as.numeric(pick_col(df, "Outptnt_Svcs", "Svcs")),
    `Average  Estimated Submitted Charges` = as.numeric(
                               pick_col(df, "Submtd_Cvrd_Chrg", "Submitted")),
    `Average Total Payments` = as.numeric(pick_col(df, "Tot_Pymt_Amt", "Total_Pay"))
  )
}

## ── Download: Legacy CMS ZIPs (2011-2017) ────────────────────────────────────

files <- read_csv("data/files.csv", show_col_types = FALSE)

for (i in seq_len(nrow(files))) {
  csvpath <- paste0("data/", files$csv[i])
  if (!file.exists(csvpath)) {
    message("Downloading: ", files$csv[i])
    zippath <- paste0("data/", files$zip[i])
    download.file(files$url[i], zippath, quiet = TRUE)
    unzip(zippath, exdir = "data")
    file.remove(zippath)
  }
}

## ── Download: Modern CMS data.cms.gov (2018+) ────────────────────────────────

# Query the CMS DKAN catalog to discover per-year CSV download URLs.
# Results are cached locally for 7 days to avoid re-fetching the large catalog.
discover_cms_urls <- function() {
  cache_file <- "data/.cms_url_cache.rds"
  cache_days <- 7

  if (file.exists(cache_file)) {
    age <- as.numeric(difftime(Sys.time(), file.info(cache_file)$mtime, units = "days"))
    if (age < cache_days) {
      message("Using cached CMS dataset catalog (", round(age, 1), " days old).")
      return(readRDS(cache_file))
    }
  }

  message("Fetching CMS data catalog to discover 2018+ download URLs...")
  message("(First run only; results cached for ", cache_days, " days)")

  catalog <- tryCatch(
    fromJSON("https://data.cms.gov/data.json", flatten = TRUE),
    error = function(e) { message("  Cannot reach data.cms.gov: ", e$message); NULL }
  )

  if (is.null(catalog) || is.null(catalog$dataset)) {
    message("  Catalog unavailable — 2018+ data will be skipped.")
    return(NULL)
  }

  ds <- catalog$dataset
  inp_pat <- "Medicare Inpatient Hospitals.*by Provider and Service"
  out_pat <- "Medicare Outpatient Hospitals.*by Provider and Service"

  rows <- list()
  for (i in seq_len(nrow(ds))) {
    title  <- ds$title[i]
    is_inp <- grepl(inp_pat, title, ignore.case = TRUE)
    is_out <- grepl(out_pat, title, ignore.case = TRUE)
    if (!is_inp && !is_out) next

    io <- if (is_inp) "Inpatient" else "Outpatient"

    # Year from temporal field, fall back to title
    temporal <- ds$temporal[i]
    year <- NA_integer_
    if (!is.null(temporal) && !is.na(temporal)) {
      m <- regmatches(temporal, regexpr("\\d{4}", temporal))
      if (length(m)) year <- as.integer(m)
    }
    if (is.na(year)) {
      m <- regmatches(title, regexpr("\\d{4}", title))
      if (length(m)) year <- as.integer(m)
    }

    # CSV downloadURL from distribution list
    dists   <- ds$distribution[[i]]
    csv_url <- NA_character_
    if (is.data.frame(dists)) {
      csv_row <- dists[tolower(dists$mediaType) == "text/csv", ]
      if (nrow(csv_row) > 0 && !is.na(csv_row$downloadURL[1]))
        csv_url <- csv_row$downloadURL[1]
    }

    if (!is.na(year) && year >= 2018 && !is.na(csv_url))
      rows[[length(rows) + 1]] <- data.frame(year = year, io = io,
                                              url = csv_url, stringsAsFactors = FALSE)
  }

  if (!length(rows)) {
    message("  No matching 2018+ datasets found.")
    return(NULL)
  }

  result <- bind_rows(rows) %>% arrange(io, year)
  saveRDS(result, cache_file)
  message("  Found ", nrow(result), " modern datasets (",
          paste(sort(unique(result$year)), collapse = ", "), ").")
  result
}

modern_urls <- discover_cms_urls()

modern_inpatient_files  <- list()
modern_outpatient_files <- list()

if (!is.null(modern_urls)) {
  for (i in seq_len(nrow(modern_urls))) {
    row   <- modern_urls[i, ]
    ext   <- if (grepl("\\.xlsx?$", row$url, ignore.case = TRUE)) "xlsx" else "csv"
    fname <- sprintf("cms_%s_%d.%s", tolower(row$io), row$year, ext)
    fpath <- paste0("data/", fname)

    if (!file.exists(fpath)) {
      message(sprintf("Downloading %s %d...", row$io, row$year))
      ok <- tryCatch({
        download.file(row$url, fpath, quiet = TRUE); TRUE
      }, error = function(e) { message("  Failed: ", e$message); FALSE })
      if (!ok) next
    } else {
      message("Cached: ", fname)
    }

    info <- list(path = fpath, year = row$year)
    if (row$io == "Inpatient")
      modern_inpatient_files[[as.character(row$year)]]  <- info
    else
      modern_outpatient_files[[as.character(row$year)]] <- info
  }
}

## ── Outpatient: read legacy years ────────────────────────────────────────────

Outpatient11 <- read_csv("data/Medicare_Provider_Charge_Outpatient_APC30_CY2011_v2.csv",
                          show_col_types = FALSE) %>% mutate(year = 2011)
OutpatientColumns <- colnames(Outpatient11)

Outpatient12 <- read_csv("data/Medicare_Provider_Charge_Outpatient_APC30_CY2012.csv",
                          show_col_types = FALSE) %>%
  mutate(year = 2012) %>% retitle(OutpatientColumns)

Outpatient13 <- read_csv("data/Medicare_Provider_Charge_Outpatient_APC30_CY2013.csv",
                          show_col_types = FALSE) %>%
  mutate(year = 2013) %>% retitle(OutpatientColumns)

Outpatient14 <- read_csv("data/Medicare_Provider_Charge_Outpatient_APC32_CY2014.csv",
                          show_col_types = FALSE) %>%
  mutate(year = 2014) %>% retitle(OutpatientColumns)

# 2015/2016: detect schema (old positional or new MUP) and normalize
read_outpatient_flexible <- function(path, year) {
  tryCatch({
    df <- read_csv(path, show_col_types = FALSE)
    if ("Rndrng_Prvdr_CCN" %in% names(df)) {
      normalize_outpatient(df) %>% mutate(year = year)
    } else {
      ref <- head(OutpatientColumns, -1)  # exclude "year" column
      if (ncol(df) >= length(ref)) df <- df[, seq_len(length(ref))]
      colnames(df) <- ref[seq_len(ncol(df))]
      df %>% mutate(year = year)
    }
  }, error = function(e) {
    message(sprintf("  Warning: could not read %d outpatient: %s", year, e$message))
    NULL
  })
}

Outpatient15 <- read_outpatient_flexible(
  "data/Medicare_OPPS_CY2015_Provider_APC.csv", 2015)
Outpatient16 <- read_outpatient_flexible(
  "data/Medicare_OPPS_CY2016_Provider_APC.csv", 2016)

Outpatient17 <- tryCatch({
  df <- read_xlsx("data/MUP_OHP_R19_P04_V10_D17_APC_Provider.xlsx", skip = 5)
  if ("Beneficiaries" %in% names(df)) df <- select(df, -Beneficiaries)
  normalize_outpatient(df) %>% mutate(year = 2017)
}, error = function(e) {
  message("  Warning: could not read 2017 outpatient: ", e$message); NULL
})

## Outpatient: read modern years (2018+)
modern_outpatient_dfs <- lapply(modern_outpatient_files, function(f) {
  tryCatch({
    df <- if (grepl("\\.xlsx?$", f$path, ignore.case = TRUE)) read_xlsx(f$path)
          else read_csv(f$path, show_col_types = FALSE)
    normalize_outpatient(df) %>% mutate(year = f$year)
  }, error = function(e) {
    message(sprintf("  Warning: could not process %d outpatient: %s", f$year, e$message))
    NULL
  })
})

## Combine all outpatient years
all_outpatient <- Filter(Negate(is.null),
  c(list(Outpatient11, Outpatient12, Outpatient13, Outpatient14,
         Outpatient15, Outpatient16, Outpatient17),
    modern_outpatient_dfs))

Outpatient <- bind_rows(all_outpatient) %>%
  mutate(
    `Provider Id`       = str_pad(as.character(`Provider Id`), 6, "left", "0"),
    `Provider Zip Code` = str_pad(as.character(`Provider Zip Code`), 5, "left", "0"),
    code      = substr(APC, 1, 4),
    procedure = sub("(Level [[:alnum:]]*) (.*)", "\\2: \\1",
                    substring(APC, 8), perl = TRUE) %>%
                str_to_title() %>%
                str_replace_all(c(" & " = " and ", " W " = " with ",
                                  " W/O " = " without ",
                                  " Cc"  = " Complication or Co-morbidity",
                                  " Mcc" = " Major Complication or Co-morbidity")),
    `Average  Estimated Submitted Charges` =
      round(`Average  Estimated Submitted Charges`, 2),
    `Average Total Payments` = round(`Average Total Payments`, 2)
  ) %>%
  rename(definition = APC, performed = `Outpatient Services`)

OutpatientData <- Outpatient %>%
  select(`Provider Id`, year, code, performed:`Average Total Payments`) %>%
  distinct() %T>%
  write_csv("data/OutpatientProcedures.csv")

OutpatientCodes <- Outpatient %>%
  select(code, procedure) %>%
  distinct() %T>%
  write_csv("data/OutpatientCodes.csv")

## ── Inpatient: read legacy years ─────────────────────────────────────────────

Inpatient11 <- read_csv("data/Medicare_Provider_Charge_Inpatient_DRG100_FY2011.csv",
                         show_col_types = FALSE) %>% mutate(year = 2011)
Inpatient12 <- read_csv("data/Medicare_Provider_Charge_Inpatient_DRG100_FY2012.csv",
                         show_col_types = FALSE) %>% mutate(year = 2012)
Inpatient13 <- read_csv("data/Medicare_Provider_Charge_Inpatient_DRG100_FY2013.csv",
                         show_col_types = FALSE) %>% mutate(year = 2013)
Inpatient14 <- read_csv("data/Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv",
                         show_col_types = FALSE) %>% mutate(year = 2014)
Inpatient15 <- read_csv("data/Medicare_Provider_Charge_Inpatient_DRGALL_FY2015.csv",
                         show_col_types = FALSE) %>% mutate(year = 2015)

# 2016 had comma-formatted numbers; original code also had a syntax error (`:` vs `=`)
Inpatient16 <- read_csv("data/Medicare_Provider_Charge_Inpatient_DRGALL_FY2016.csv",
                         col_types = cols(.default = "c"), show_col_types = FALSE) %>%
  mutate(
    `Average Covered Charges`   = parse_number(`Average Covered Charges`),
    `Average Total Payments`    = parse_number(`Average Total Payments`),
    `Average Medicare Payments` = parse_number(`Average Medicare Payments`),
    year = 2016
  )

Inpatient17 <- read_csv("data/MEDICARE_PROVIDER_CHARGE_INPATIENT_DRGALL_FY2017.CSV",
                         show_col_types = FALSE) %>% mutate(year = 2017)

## Inpatient: read modern years (2018+)
modern_inpatient_dfs <- lapply(modern_inpatient_files, function(f) {
  tryCatch({
    df <- if (grepl("\\.xlsx?$", f$path, ignore.case = TRUE)) read_xlsx(f$path)
          else read_csv(f$path, show_col_types = FALSE)
    normalize_inpatient(df) %>% mutate(year = f$year)
  }, error = function(e) {
    message(sprintf("  Warning: could not process %d inpatient: %s", f$year, e$message))
    NULL
  })
})

## Combine all inpatient years (includes 2016 — was missing from original script)
all_inpatient <- Filter(Negate(is.null),
  c(list(Inpatient11, Inpatient12, Inpatient13, Inpatient14,
         Inpatient15, Inpatient16, Inpatient17),
    modern_inpatient_dfs))

Inpatient <- bind_rows(all_inpatient) %>%
  mutate(
    `Provider Id`       = str_pad(as.character(`Provider Id`), 6, "left", "0"),
    `Provider Zip Code` = str_pad(as.character(`Provider Zip Code`), 5, "left", "0"),
    DRG       = substring(`DRG Definition`, 1, 3),
    Procedure = substring(`DRG Definition`, 7) %>%
                str_to_title() %>%
                str_replace_all(c(" & " = " and ", " W " = " with ",
                                  " W/O " = " without ",
                                  " Cc"  = " Complication or Co-morbidity",
                                  " Mcc" = " Major Complication or Co-morbidity")),
    `Average Covered Charges`   = round(`Average Covered Charges`, 2),
    `Average Total Payments`    = round(`Average Total Payments`, 2),
    `Average Medicare Payments` = round(`Average Medicare Payments`, 2)
  ) %>%
  rename(definition = `DRG Definition`, performed = `Total Discharges`)

InpatientData <- Inpatient %>%
  select(`Provider Id`, year, Procedure, performed:`Average Medicare Payments`) %>%
  distinct() %T>%
  write_csv("data/InpatientProcedures.csv")

InpatientCodes <- Inpatient %>%
  select(DRG, Procedure) %>%
  distinct() %>%
  arrange(DRG) %T>%
  write_csv("data/InpatientCodes.csv")

## ── Providers ─────────────────────────────────────────────────────────────────

Addresses <- read_csv("data/geoCoded.csv", show_col_types = FALSE)

OutpatientProviders <- Outpatient %>%
  select(starts_with("Provider"),
         hrr = `Hospital Referral Region (HRR) Description`)

InpatientProviders <- Inpatient %>%
  select(starts_with("Provider"),
         hrr = `Hospital Referral Region (HRR) Description`)

Providers <- bind_rows(InpatientProviders, OutpatientProviders) %>%
  distinct(`Provider Id`, .keep_all = TRUE) %>%
  mutate(
    `Provider Zip Code` = str_pad(as.character(`Provider Zip Code`), 5, "left", "0"),
    address = paste(`Provider Street Address`, `Provider City`, `Provider State`,
                    sep = ", "),
    `Provider Name` = str_to_title(`Provider Name`)
  ) %>%
  left_join(Addresses, by = "address") %>%
  select(-address) %T>%
  write_csv("data/Providers.csv")

## ── Cleanup and save ──────────────────────────────────────────────────────────

remove(Addresses, files, modern_urls,
       modern_inpatient_files, modern_outpatient_files,
       modern_inpatient_dfs, modern_outpatient_dfs,
       all_inpatient, all_outpatient,
       InpatientProviders, OutpatientProviders,
       Inpatient, Outpatient,
       Inpatient11, Inpatient12, Inpatient13, Inpatient14,
       Inpatient15, Inpatient16, Inpatient17,
       Outpatient11, Outpatient12, Outpatient13, Outpatient14,
       Outpatient15, Outpatient16, Outpatient17)

StateCentroids <- read_csv("data/StateCentroids.csv", show_col_types = FALSE)

save.image("data/Medicare_Data.rdata")
