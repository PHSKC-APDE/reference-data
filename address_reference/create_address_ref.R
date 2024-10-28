#' @title Address Reference Table
#'
#' @description Create an address reference table for Medicaid and HMIS data
#' that lists type of organization(s) at a given address
#'
#' @details
#' Place of Service codes:
#' https://www.cms.gov/medicare/medicare-fee-for-service-payment/physicianfeesched/downloads/website-pos-database.pdf
#'
#' @TODO
#' Long-term version of HMIS file iteration
#' Investigate why some pharmacies are not listed as 01 code
#' Further ways to break out "Office" category, maybe using txnmy
#'

## Clear memory, load packages, set up constants ----
rm(list = ls())
pacman::p_load(data.table,
               iotools,
               kcgeocode,
               lubridate,
               openxlsx,
               rads,
               rads.data,
               odbc,
               stringr)
devtools::source_url(
  "https://raw.githubusercontent.com/PHSKC-APDE/apde/main/R/create_db_connection.R"
)

# Constants
recode_mcaid_addresses <- F  # run if new mcaid servicing/billing provider addresses come in
recode_all_mcaid <- F  # If you need a full recode of all mcaid addresses instead of just new ones, which is unlikely
recode_hmis_addresses <- F

# Change this to your shortcut if needed
crosswalk_location <- "C:/Users/kfukutaki.KC/OneDrive - King County/Shared Documents - HHSAW Users/Ref schema/address_reference_crosswalk.xlsx"

## CONNECT SERVERS ----
db_hhsaw <- create_db_connection("hhsaw", interactive = F, prod = T)

## DEFINE FUNCTIONS ----
submit_ads_to_geocoder <- function(conn,
                                   ads,
                                   schema_name = c("claims", "hmis"),
                                   table_name = c("raw_billing_provider_address",
                                                  "raw_servicing_provider_address")) {
  cleaned_ads = lapply(ads, function(a) {
    x = kc_singleline(a, unparsed = T)
    if (is.null(x$candidates)) {
      r = (data.table(input = a))
    } else if (((length(x$candidates) == 0))) {
      r = (data.table(input = a))
    } else{
      r = as.data.table(x$candidates[[1]]$attributes)[, c(1:28)]
      r[, input := a]
    }
    return(r)
  })
  cleaned_ads = rbindlist(cleaned_ads, fill = TRUE)
  # Write intermediate results
  DBI::dbWriteTable(
    conn = conn,
    name = DBI::Id(schema = schema_name, table = table_name),
    value = cleaned_ads,
    append = T
  )
  cleaned_ads <- cleaned_ads[, c("StAddr", "City", "County", "State", "ZIP", "input")]
  setnames(
    cleaned_ads,
    c("StAddr", "City", "ZIP", "County", "State"),
    c(
      "geo_add1_raw",
      "geo_city_raw",
      "geo_zip_raw",
      "geo_county_raw",
      "geo_state_raw"
    )
  )
  cleaned_ads <- cleaned_ads[, `:=` (geo_add2_raw = NA, geo_add3_raw = NA)]
  uped = submit_ads_for_cleaning(cleaned_ads, con = db_hhsaw)
  return(cleaned_ads)
}

fetch_and_save_ads <- function(conn,
                               ads_to_fetch,
                               schema_name = c("claims", "hmis"),
                               final_table_name = c("final_servicing_provider_address",
                                                    "final_billing_provider_address")) {
  a1 = fetch_addresses(
    ads_to_fetch,
    input_type = 'raw',
    geocode = TRUE,
    con = db_hhsaw,
    deduplicate = T
  )
  ads_geocoded <- cbind(ads_to_fetch, a1)
  ads_geocoded[, geocode_success := ifelse(!is.na(geo_address_geocoded), 1, 0)]
  ads_geocoded <- ads_geocoded[, c("input",
                                   "geo_hash_raw",
                                   "geo_hash_clean",
                                   "geocode_success")]
  
  DBI::dbWriteTable(
    conn = db_hhsaw,
    name = DBI::Id(schema = schema_name, table = final_table_name),
    value = ads_geocoded,
    append = T
  )
  return(ads_geocoded)
}

## READ DATA ----
# Read address_reference_crosswalk table
crosswalk <- setDT(openxlsx::read.xlsx(crosswalk_location))
crosswalk <- rads::string_clean(crosswalk)

# Read in HMIS table
hmis_schema <- "hmis"
hmis_tbl <- "HMISFacilityReference"
hmis <- rads::string_clean(setDT(DBI::dbGetQuery(
  db_hhsaw,
  glue::glue_sql(
    "SELECT Agency
      ,Program
      ,ProjectType
      ,Address
      ,City
      ,Zip
      ,OperatingStartDate
      ,OperatingEndDate
      ,HousingType
      ,LastUpdated
   FROM {`hmis_schema`}.{`hmis_tbl`}
  ",
    .con = db_hhsaw
  )
)))
setnames(
  hmis,
  c("Address", "City", "Zip"),
  c("geo_add1_raw", "geo_city_raw", "geo_zip_raw")
)
hmis$OperatingStartDate <- as.Date(hmis$OperatingStartDate)
hmis$OperatingEndDate <- as.Date(hmis$OperatingEndDate)
rads::string_clean(hmis)
# Optional code to subset end dates to no more than current date
# hmis[, OperatingEndDate:=ifelse(OperatingEndDate > Sys.Date(), Sys.Date(), OperatingEndDate)]


# KC zip codes
kc_zips <- as.character(rads.data::spatial_zip_to_hra20_geog[]$ZIP)

# Read in stage_mcaid_claim with unique combos of PRVDR_LAST_NAME, PRVDR_FIRST_NAME, TXNMY_NAME, BILLING_PRVDR_ADDRESS, SERVICING_PRVDR_ADDRESS, FCLTY_TYPE_CODE
mcaid_schema <- "claims"
mcaid_tbl_name <- "stage_mcaid_claim"
mcaid <- rads::string_clean(setDT(DBI::dbGetQuery(
  db_hhsaw,
  glue::glue_sql(
    "SELECT DISTINCT PRVDR_LAST_NAME, PRVDR_FIRST_NAME, TXNMY_NAME,
     BILLING_PRVDR_ADDRESS, SERVICING_PRVDR_ADDRESS AS SERVICING_PRVDR_ADDRESS, FCLTY_TYPE_CODE,
     SYSTEM_IN_DATE
   FROM {`mcaid_schema`}.{`mcaid_tbl_name`}
  ",
    .con = db_hhsaw
  )
)))
mcaid[, SYSTEM_IN_DATE := max(SYSTEM_IN_DATE)]
mcaid$SERVICING_PRVDR_ADDRESS <- toupper(mcaid$SERVICING_PRVDR_ADDRESS)
mcaid$BILLING_PRVDR_ADDRESS <- toupper(mcaid$BILLING_PRVDR_ADDRESS)

# Remove non-KC zip codes
mcaid$service_zip <- stringr::str_sub(mcaid$SERVICING_PRVDR_ADDRESS, -5, -1)
mcaid$billing_zip <- stringr::str_sub(mcaid$BILLING_PRVDR_ADDRESS, -5, -1)
mcaid <- mcaid[(mcaid$service_zip %in% kc_zips) |
                 (mcaid$billing_zip %in% kc_zips), ]
mcaid[, service_zip := NULL]
mcaid[, billing_zip := NULL]

# from/to date info
srvc_newest <- rads::string_clean(setDT(DBI::dbGetQuery(
  db_hhsaw,
  glue::glue_sql(
    "SELECT SERVICING_PRVDR_ADDRESS, MAX(TO_SRVC_DATE) AS operating_latest_date
    FROM {`mcaid_schema`}.{`mcaid_tbl_name`}
    GROUP BY SERVICING_PRVDR_ADDRESS",
    .con = db_hhsaw
  )
)))
srvc_oldest <- rads::string_clean(setDT(DBI::dbGetQuery(
  db_hhsaw,
  glue::glue_sql(
    "SELECT SERVICING_PRVDR_ADDRESS, MIN(FROM_SRVC_DATE) AS operating_earliest_date
    FROM {`mcaid_schema`}.{`mcaid_tbl_name`}
    GROUP BY SERVICING_PRVDR_ADDRESS",
    .con = db_hhsaw
  )
)))
bllng_newest <- rads::string_clean(setDT(DBI::dbGetQuery(
  db_hhsaw,
  glue::glue_sql(
    "SELECT BILLING_PRVDR_ADDRESS, MAX(TO_SRVC_DATE) AS operating_latest_date
    FROM {`mcaid_schema`}.{`mcaid_tbl_name`}
    GROUP BY BILLING_PRVDR_ADDRESS",
    .con = db_hhsaw
  )
)))
bllng_oldest <- rads::string_clean(setDT(DBI::dbGetQuery(
  db_hhsaw,
  glue::glue_sql(
    "SELECT BILLING_PRVDR_ADDRESS, MIN(FROM_SRVC_DATE) AS operating_earliest_date
    FROM {`mcaid_schema`}.{`mcaid_tbl_name`}
    GROUP BY BILLING_PRVDR_ADDRESS",
    .con = db_hhsaw
  )
)))
srvc_newest$SERVICING_PRVDR_ADDRESS <- toupper(srvc_newest$SERVICING_PRVDR_ADDRESS)
srvc_oldest$SERVICING_PRVDR_ADDRESS <- toupper(srvc_oldest$SERVICING_PRVDR_ADDRESS)
bllng_newest$BILLING_PRVDR_ADDRESS <- toupper(bllng_newest$BILLING_PRVDR_ADDRESS)
bllng_oldest$BILLING_PRVDR_ADDRESS <- toupper(bllng_oldest$BILLING_PRVDR_ADDRESS)

# if latest date is > today, change it to today (fifelse preserves date type)
srvc_newest[, operating_latest_date := fifelse(operating_latest_date > Sys.Date(),
                                               Sys.Date(),
                                               operating_latest_date)]
bllng_newest[, operating_latest_date := fifelse(operating_latest_date > Sys.Date(),
                                                Sys.Date(),
                                                operating_latest_date)]

srvc_dates <- merge(mcaid, srvc_oldest, by = "SERVICING_PRVDR_ADDRESS", incomparables = NA)
srvc_dates <- merge(srvc_dates,
                    srvc_newest,
                    by = "SERVICING_PRVDR_ADDRESS",
                    incomparables = NA)
bllng_dates <- merge(mcaid, bllng_oldest, by = "BILLING_PRVDR_ADDRESS", incomparables = NA)
bllng_dates <- merge(bllng_dates,
                     bllng_newest,
                     by = "BILLING_PRVDR_ADDRESS",
                     incomparables = NA)
mcaid <- rbind(srvc_dates, bllng_dates)
mcaid <- unique(mcaid)
rm(srvc_dates, bllng_dates)

# Do a little cleaning to help the geocoder
mcaid$SERVICING_PRVDR_ADDRESS <- gsub("(ID ONLY)", "", mcaid$SERVICING_PRVDR_ADDRESS)
mcaid$SERVICING_PRVDR_ADDRESS <- gsub("ID ONLY", "", mcaid$SERVICING_PRVDR_ADDRESS)
mcaid$BILLING_PRVDR_ADDRESS <- gsub("(ID ONLY)", "", mcaid$BILLING_PRVDR_ADDRESS)
mcaid$BILLING_PRVDR_ADDRESS <- gsub("ID ONLY", "", mcaid$BILLING_PRVDR_ADDRESS)
# Removes all text before the first number OR po box appears
mcaid$SERVICING_PRVDR_ADDRESS <- gsub("^*(\\D+ | PO | P O)", "", mcaid$SERVICING_PRVDR_ADDRESS)
mcaid$BILLING_PRVDR_ADDRESS <- gsub("^*(\\D+ | PO | P O)", "", mcaid$BILLING_PRVDR_ADDRESS)

# Manually change some facility codes based on names
mcaid[, FCLTY_TYPE_CODE := ifelse((like(toupper(PRVDR_LAST_NAME), "PHARMACY") &
                                     is.na(FCLTY_TYPE_CODE)), "01", FCLTY_TYPE_CODE)]

# Create lists of unique mcaid addresses for cleaning
mcaid_srvc <- unique(mcaid$SERVICING_PRVDR_ADDRESS)
mcaid_bllng <- unique(mcaid$BILLING_PRVDR_ADDRESS)


## HMIS Geocoder ----
if (recode_hmis_addresses) {
  hmis_ads <- unique(hmis[, c("geo_add1_raw", "geo_city_raw", "geo_zip_raw")])
  hmis_ads <- hmis_ads[, `:=` (
    geo_add2_raw = NA,
    geo_add3_raw = NA,
    geo_state_raw = "WA"
  )]
  uped = submit_ads_for_cleaning(hmis_ads, con = db_hhsaw)
  # wait a bit before continuing!
  a1 = fetch_addresses(
    hmis_ads,
    input_type = 'raw',
    geocode = TRUE,
    con = db_hhsaw,
    deduplicate = T
  )
  hmis_cleaned <- cbind(hmis_ads, a1)
  hmis_cleaned[, geocode_success := ifelse(!is.na(geo_address_geocoded), 1, 0)]
  hmis_geocoded <- hmis_cleaned[, c(
    "geo_add1_raw",
    "geo_add2_raw",
    "geo_city_raw",
    "geo_zip_raw",
    "geo_hash_raw",
    "geo_hash_clean",
    "geocode_success"
  )]
  DBI::dbWriteTable(
    conn = db_hhsaw,
    name = DBI::Id(schema = "kfukutaki", table = "hmis_provider_geocoded"),
    value = hmis_geocoded,
    overwrite = T
  )
  print("HMIS geocoding done!")
}

## Load data ----
# Load HMIS if not already loaded from geocoder section
if (!exists("hmis_geocoded")) {
  hmis_geocoded <- setDT(DBI::dbGetQuery(
    db_hhsaw,
    glue::glue_sql(
      "SELECT geo_add1_raw, geo_city_raw, geo_zip_raw,
      geo_hash_raw, geo_hash_clean, geocode_success
     FROM kfukutaki.hmis_provider_geocoded",
      .con = db_hhsaw
    )
  ))
  hmis_geocoded$addr_type <- "HMIS"
} else{
  hmis_geocoded <- hmis_geocoded[, c(
    "geo_add1_raw",
    "geo_city_raw",
    "geo_zip_raw",
    "geo_hash_raw",
    "geo_hash_clean",
    "geocode_success"
  )]
  hmis_geocoded$addr_type <- "HMIS"
}
# Load mcaid data
srvc_geocoded <- setDT(DBI::dbGetQuery(
  db_hhsaw,
  glue::glue_sql(
    "SELECT input, geo_hash_raw, geo_hash_clean, geocode_success
   FROM claims.final_servicing_provider_address",
    .con = db_hhsaw
  )
))

bllng_geocoded <- setDT(DBI::dbGetQuery(
  db_hhsaw,
  glue::glue_sql(
    "SELECT input, geo_hash_raw, geo_hash_clean, geocode_success
   FROM claims.final_billing_provider_address",
    .con = db_hhsaw
  )
))

## GEOCODE NEW MCAID ADDRESSES IF NEEDED ---
# Check for new mcaid addresses - if there are any, change recode_mcaid_addresses to T
# and run the if/else section below manually pausing in between the sections
srvc_diff <- setdiff(mcaid_srvc, srvc_geocoded$input)
bllng_diff <- setdiff(mcaid_bllng, bllng_geocoded$input)

if (recode_mcaid_addresses == T) {
  if (recode_all_mcaid == T) {
    srvc_fetch_ads <- submit_ads_to_geocoder(
      conn = db_hhsaw,
      ads = mcaid_srvc,
      schema_name = "claims",
      table_name = "raw_servicing_provider_address"
    )
    bllng_fetch_ads <- submit_ads_to_geocoder(
      conn = db_hhsaw,
      ads = mcaid_bllng,
      schema_name = "claims",
      table_name = "raw_billing_provider_address"
    )
  } else{
    srvc_fetch_ads <- submit_ads_to_geocoder(
      conn = db_hhsaw,
      ads = srvc_diff,
      schema_name = "claims",
      table_name = "raw_servicing_provider_address"
    )
    bllng_fetch_ads <- submit_ads_to_geocoder(
      conn = db_hhsaw,
      ads = bllng_diff,
      schema_name = "claims",
      table_name = "raw_billing_provider_address"
    )
  }
}
# WAIT A BIT and then run this section
if (recode_mcaid_addresses == T) {
  if (recode_all_mcaid == T) {
    srvc_geocoded <- fetch_and_save_ads(
      conn,
      srvc_fetch_ads,
      schema_name = "claims",
      final_table_name = c(
        "final_servicing_provider_address",
        "final_billing_provider_address"
      )
    )
    bllng_geocoded <- fetch_and_save_ads(
      conn,
      bllng_fetch_ads,
      schema_name = "claims",
      final_table_name = c(
        "final_servicing_provider_address",
        "final_billing_provider_address"
      )
    )
  } else{
    srvc_geocoded <- fetch_and_save_ads(
      conn,
      srvc_fetch_ads,
      schema_name = "claims",
      final_table_name = c(
        "final_servicing_provider_address",
        "final_billing_provider_address"
      )
    )
    bllng_geocoded <- fetch_and_save_ads(
      conn,
      bllng_fetch_ads,
      schema_name = "claims",
      final_table_name = c(
        "final_servicing_provider_address",
        "final_billing_provider_address"
      )
    )
  }
}

## Merge cleaned addresses for mcaid ----
# Rename columns
setnames(srvc_geocoded, c("input"), c("SERVICING_PRVDR_ADDRESS"))
setnames(bllng_geocoded, c("input"), c("BILLING_PRVDR_ADDRESS"))

# Add on the geocode information
all_srvc <- merge(
  mcaid,
  srvc_geocoded,
  by = "SERVICING_PRVDR_ADDRESS",
  all = F,
  incomparables = NA
)
all_bllng <- merge(
  mcaid,
  bllng_geocoded,
  by = "BILLING_PRVDR_ADDRESS",
  all = F,
  incomparables = NA
)

# Remove any null addresses (shouldn't be many, if any)
all_srvc <- all_srvc[!is.na(SERVICING_PRVDR_ADDRESS), ]
all_bllng <- all_bllng[!is.na(BILLING_PRVDR_ADDRESS), ]

# Subset to the desired columns
all_srvc <- all_srvc[, c(
  "SERVICING_PRVDR_ADDRESS",
  "PRVDR_LAST_NAME",
  "PRVDR_FIRST_NAME",
  "TXNMY_NAME",
  "FCLTY_TYPE_CODE",
  "geo_hash_clean",
  "geocode_success",
  "SYSTEM_IN_DATE",
  "operating_earliest_date",
  "operating_latest_date"
)]
all_bllng <- all_bllng[, c(
  "BILLING_PRVDR_ADDRESS",
  "PRVDR_LAST_NAME",
  "PRVDR_FIRST_NAME",
  "TXNMY_NAME",
  "FCLTY_TYPE_CODE",
  "geo_hash_clean",
  "geocode_success",
  "SYSTEM_IN_DATE",
  "operating_earliest_date",
  "operating_latest_date"
)]

# Add a flag for whether it's a billing or service provider
all_srvc[, bllng_or_srvc := "S"]
all_bllng[, bllng_or_srvc := "B"]

setnames(
  all_srvc,
  c("SERVICING_PRVDR_ADDRESS", "SYSTEM_IN_DATE"),
  c("Address", "source_last_updated")
)
setnames(
  all_bllng,
  c("BILLING_PRVDR_ADDRESS", "SYSTEM_IN_DATE"),
  c("Address", "source_last_updated")
)

# Bind service and billing ads together
all_addr <- rbindlist(list(all_srvc, all_bllng),
                      use.names = T,
                      fill = T)
all_addr[, code_source := "Medicaid - Place of Service"]

## Rbind HMIS ----
all_hmis <- merge(hmis,
                  hmis_geocoded,
                  by = c("geo_add1_raw", "geo_city_raw", "geo_zip_raw"))
all_hmis <- setDT(tidyr::unite(all_hmis, Address, geo_add1_raw:geo_zip_raw, sep = ' '))
setnames(
  all_hmis,
  c(
    "Agency",
    "Program",
    "ProjectType",
    "Address",
    "geo_hash_raw",
    "OperatingStartDate",
    "OperatingEndDate",
    "HousingType",
    "addr_type"
  ),
  c(
    "PRVDR_FIRST_NAME",
    "PRVDR_LAST_NAME",
    "FCLTY_TYPE_CODE",
    "Address",
    "geo_hash_raw",
    "operating_earliest_date",
    "operating_latest_date",
    "TXNMY_NAME",
    "code_source"
  )
)
all_hmis[, bllng_or_srvc := "S"]  # HMIS counts as service provider, not billing
all_addr <- rbindlist(list(all_addr, all_hmis),
                      use.names = T,
                      fill = T)
all_addr <- rads::string_clean(all_addr)
all_addr <- unique(all_addr,
                   by = c("Address", "code_source", "bllng_or_srvc", "FCLTY_TYPE_CODE"))

## Rollup logic ----
# Merge on code and code_source
all_addr <- merge(
  all_addr,
  crosswalk,
  by.x = c("FCLTY_TYPE_CODE", "code_source"),
  by.y = c("code", "code_source"),
  all.x = T
)

# Add unknown codes for those who have no code
all_addr[, health := ifelse(code_source == "Medicaid - Place of Service", 1, 0)]
all_addr[, housing := ifelse(code_source == "HMIS", 1, 0)]
all_addr[, address_midlevel_desc := ifelse((code_source == "Medicaid - Place of Service") &
                                             is.na(FCLTY_TYPE_CODE),
                                           "Unknown Health Facility",
                                           address_midlevel_desc
)]
all_addr[, address_midlevel_desc := ifelse((code_source == "HMIS") &
                                             is.na(FCLTY_TYPE_CODE),
                                           "Unknown Housing Assistance",
                                           address_midlevel_desc
)]

## Final column cleaning ----
setnames(
  all_addr,
  c(
    "FCLTY_TYPE_CODE",
    "code_source",
    "Address",
    "PRVDR_LAST_NAME",
    "PRVDR_FIRST_NAME",
    "TXNMY_NAME",
    "geo_hash_clean",
    "bllng_or_srvc",
    "operating_latest_date",
    "operating_earliest_date",
    "health",
    "housing",
    "address_midlevel_desc",
    "address_detail_desc"
  ),
  c(
    "fclty_type_code",
    "code_source",
    "address_orig",
    "provider_name",
    "provider_first_name",
    "txnmy_name",
    "geo_hash_clean",
    "bllng_or_srvc",
    "operating_latest_date",
    "operating_earliest_date",
    "health",
    "housing",
    "address_midlevel_desc",
    "address_detail_desc"
  )
)
all_addr <- all_addr[, c(
  "geo_hash_clean",
  "geocode_success",
  "address_orig",
  "provider_name",
  "provider_first_name",
  "code_source",
  "source_last_updated",
  "bllng_or_srvc",
  "health",
  "housing",
  "operating_earliest_date",
  "operating_latest_date",
  "address_midlevel_desc",
  "address_detail_desc",
  "fclty_type_code",
  "txnmy_name"
)]
setcolorder(
  all_addr,
  c(
    "geo_hash_clean",
    "geocode_success",
    "address_orig",
    "provider_name",
    "provider_first_name",
    "code_source",
    "source_last_updated",
    "bllng_or_srvc",
    "health",
    "housing",
    "operating_earliest_date",
    "operating_latest_date",
    "address_midlevel_desc",
    "address_detail_desc",
    "fclty_type_code",
    "txnmy_name"
  )
)
all_addr[, last_run := Sys.time()]


## Upload ----
DBI::dbWriteTable(
  conn = db_hhsaw,
  name = DBI::Id(schema = "ref", table = "address_reference"),
  value = all_addr,
  overwrite = T
)
