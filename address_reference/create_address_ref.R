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
rm(list=ls())
pacman::p_load(data.table, lubridate, kcgeocode, stringr, openxlsx, rads, rads.data, readr, iotools, odbc, sqldf, stringr)
devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/apde/main/R/create_db_connection.R")

# Constants
recode_mcaid_addresses <- F  # run if new mcaid servicing/billing provider addresses come in
recode_hmis_addresses <- F

crosswalk_location <- "C:/Users/kfukutaki.KC/OneDrive - King County/Videos/Shared Documents - HHSAW Users/Ref schema/address_reference_crosswalk.xlsx"
hmis_location <- "C:/Users/kfukutaki.KC/OneDrive - King County/Documents/Data/HMIS Facility Information June 2024.xlsx"
hmis_date <- as.Date("2024-06-01")

## Prevent scientific notation except for huge numbers ----
options("scipen"=999) # turn off scientific notation

## CONNECT SERVERS ----
db_hhsaw <- create_db_connection("hhsaw", interactive = F, prod = T)

## READ DATA ----
# Read address_reference_crosswalk table
crosswalk <- setDT(openxlsx::read.xlsx(crosswalk_location))
crosswalk <- rads::string_clean(crosswalk)

# Read in HMIS table
hmis <- setDT(openxlsx::read.xlsx(hmis_location, detectDates = T))
setnames(hmis,
         c("Programs.Address", "Programs.City", "Programs.ZIP.Code"),
         c("geo_add1_raw", "geo_city_raw", "geo_zip_raw"))
rads::string_clean(hmis)
hmis[, source_last_updated := hmis_date]
# Optional code to subset end dates to no more than current date
# hmis[, Programs.Operating.End.Date:=ifelse(Programs.Operating.End.Date > Sys.Date(), Sys.Date(), Programs.Operating.End.Date)]


# KC zip codes
kc_zips <- as.character(rads.data::spatial_zip_to_hra20_geog[]$ZIP)

# Read in stage_mcaid_claim with unique combos of PRVDR_LAST_NAME, PRVDR_FIRST_NAME, TXNMY_NAME, BILLING_PRVDR_ADDRESS, SERVICING_PRVDR_ADDRESS, FCLTY_TYPE_CODE
mcaid_schema <- "claims"
mcaid_tbl_name <- "stage_mcaid_claim"
mcaid <- setDT(DBI::dbGetQuery(db_hhsaw, glue::glue_sql(
  "SELECT DISTINCT PRVDR_LAST_NAME, PRVDR_FIRST_NAME, TXNMY_NAME,
     BILLING_PRVDR_ADDRESS, SERVICING_PRVDR_ADDRESS, FCLTY_TYPE_CODE,
     SYSTEM_IN_DATE
   FROM {`mcaid_schema`}.{`mcaid_tbl_name`}",
  .con = db_hhsaw)))
mcaid <- rads::string_clean(mcaid)
mcaid[, SYSTEM_IN_DATE := max(SYSTEM_IN_DATE)]

# Remove non-KC zip codes
mcaid$service_zip <- str_sub(mcaid$SERVICING_PRVDR_ADDRESS, - 5, - 1)
mcaid$billing_zip <- str_sub(mcaid$BILLING_PRVDR_ADDRESS, - 5, - 1)
mcaid <- mcaid[(mcaid$service_zip %in% kc_zips) | (mcaid$billing_zip %in% kc_zips),]
mcaid[,service_zip:=NULL]
mcaid[,billing_zip:=NULL]

# from/to date info
srvc_newest <- rads::string_clean(setDT(DBI::dbGetQuery(db_hhsaw, glue::glue_sql(
  "SELECT SERVICING_PRVDR_ADDRESS, MAX(TO_SRVC_DATE) AS operating_latest_date
    FROM {`mcaid_schema`}.{`mcaid_tbl_name`}
    GROUP BY SERVICING_PRVDR_ADDRESS",
  .con = db_hhsaw))))
srvc_oldest <- rads::string_clean(setDT(DBI::dbGetQuery(db_hhsaw, glue::glue_sql(
  "SELECT SERVICING_PRVDR_ADDRESS, MIN(TO_SRVC_DATE) AS operating_earliest_date
    FROM {`mcaid_schema`}.{`mcaid_tbl_name`}
    GROUP BY SERVICING_PRVDR_ADDRESS",
  .con = db_hhsaw))))
bllng_newest <- rads::string_clean(setDT(DBI::dbGetQuery(db_hhsaw, glue::glue_sql(
  "SELECT BILLING_PRVDR_ADDRESS, MAX(TO_SRVC_DATE) AS operating_latest_date
    FROM {`mcaid_schema`}.{`mcaid_tbl_name`}
    GROUP BY BILLING_PRVDR_ADDRESS",
  .con = db_hhsaw))))
bllng_oldest <- rads::string_clean(setDT(DBI::dbGetQuery(db_hhsaw, glue::glue_sql(
  "SELECT BILLING_PRVDR_ADDRESS, MIN(TO_SRVC_DATE) AS operating_earliest_date
    FROM {`mcaid_schema`}.{`mcaid_tbl_name`}
    GROUP BY BILLING_PRVDR_ADDRESS",
  .con = db_hhsaw))))

# Do a little cleaning to help the geocoder
mcaid$SERVICING_PRVDR_ADDRESS <- toupper(mcaid$SERVICING_PRVDR_ADDRESS)
mcaid$BILLING_PRVDR_ADDRESS <- toupper(mcaid$BILLING_PRVDR_ADDRESS)
mcaid$SERVICING_PRVDR_ADDRESS <- gsub("(ID ONLY)","", mcaid$SERVICING_PRVDR_ADDRESS)
mcaid$SERVICING_PRVDR_ADDRESS <- gsub("ID ONLY","", mcaid$SERVICING_PRVDR_ADDRESS)
mcaid$BILLING_PRVDR_ADDRESS <- gsub("(ID ONLY)","", mcaid$BILLING_PRVDR_ADDRESS)
mcaid$BILLING_PRVDR_ADDRESS <- gsub("ID ONLY","", mcaid$BILLING_PRVDR_ADDRESS)
# Removes all text before the first number OR po box appears
mcaid$SERVICING_PRVDR_ADDRESS <- gsub("^*(\\D+ | PO | P O)", "", mcaid$SERVICING_PRVDR_ADDRESS)
mcaid$BILLING_PRVDR_ADDRESS <- gsub("^*(\\D+ | PO | P O)", "", mcaid$BILLING_PRVDR_ADDRESS)

# Manually change some facility codes based on names
mcaid[, FCLTY_TYPE_CODE := ifelse((like(toupper(PRVDR_LAST_NAME),"PHARMACY") & is.na(FCLTY_TYPE_CODE)),
                                  "01",
                                  FCLTY_TYPE_CODE)]

# Create lists of unique mcaid addresses for cleaning
mcaid_srvc <- unique(mcaid$SERVICING_PRVDR_ADDRESS)
mcaid_bllng <- unique(mcaid$BILLING_PRVDR_ADDRESS)


## Geocoder address cleaning ----
if (recode_hmis_addresses) {
  # HMIS geocoding
  hmis_ads <- unique(hmis[, c("geo_add1_raw", "geo_city_raw", "geo_zip_raw")])
  hmis_ads <- hmis_ads[, `:=` (geo_add2_raw=NA, geo_add3_raw=NA, geo_state_raw="WA")]
  uped = submit_ads_for_cleaning(hmis_ads, con = db_hhsaw)
  # wait a bit before continuing!
  a1 = fetch_addresses(hmis_ads, input_type = 'raw', geocode = TRUE, con = db_hhsaw, deduplicate = T)
  hmis_cleaned <- cbind(hmis_ads, a1)
  hmis_cleaned[, geocode_success:=ifelse(!is.na(geo_address_geocoded), 1, 0)]
  hmis_geocoded <- hmis_cleaned[,c("geo_add1_raw", "geo_add2_raw", "geo_city_raw",
                                   "geo_zip_raw", "geo_hash_raw", "geo_hash_clean",
                                   "geocode_success")]
  DBI::dbWriteTable(conn = db_hhsaw,
                    name = DBI::Id(schema = "kfukutaki", table = "hmis_provider_geocoded"),
                    value = hmis_geocoded,
                    overwrite = T)
  print("HMIS geocoding done!")
}

if (recode_mcaid_addresses){
  # Separate city and zip with kc_singleline
  srvc_cleaned = lapply(mcaid_srvc, function(a){
    x = kc_singleline(a, unparsed = T)
    if (is.null(x$candidates)){
      r = (data.table(input = a))
    } else if (((length(x$candidates)==0))){
      r = (data.table(input=a))
    } else{
      r = as.data.table(x$candidates[[1]]$attributes)[,c(1:28)]
      r[, input := a]
    }
    return(r)
  })
  srvc_cleaned = rbindlist(srvc_cleaned, fill=TRUE)
  DBI::dbWriteTable(conn = db_hhsaw, 
                    name = DBI::Id(schema = "claims",
                                   table = "raw_servicing_provider_address"),
                    value = srvc_cleaned,
                    overwrite = T)
  # srvc_cleaned <- setDT(DBI::dbGetQuery(db_hhsaw, glue::glue_sql(
  #   "SELECT * FROM claims.raw_servicing_provider_address",
  #   .con = db_hhsaw)))
  srvc_cleaned <- srvc_cleaned[,c("StAddr", "City", "County", "State", "ZIP", "input")]
  setnames(srvc_cleaned,
           c("StAddr", "City", "ZIP", "County", "State"),
           c("geo_add1_raw","geo_city_raw", "geo_zip_raw", "geo_county_raw", "geo_state_raw"))
  srvc_cleaned <- srvc_cleaned[, `:=` (geo_add2_raw=NA, geo_add3_raw=NA)]
  uped = submit_ads_for_cleaning(srvc_cleaned, con = db_hhsaw)
  # wait a bit before continuing!
  a1 = fetch_addresses(srvc_cleaned, input_type = 'raw', geocode = TRUE, con = db_hhsaw, deduplicate = T)
  srvc_geocoded <- cbind(srvc_cleaned, a1)
  srvc_geocoded[, geocode_success:=ifelse(!is.na(geo_address_geocoded), 1, 0)]
  srvc_geocoded <- srvc_geocoded[,c("input", "geo_hash_raw", "geo_hash_clean", "geocode_success")]
  
  DBI::dbWriteTable(conn = db_hhsaw,
                    name = DBI::Id(schema = "claims", table = "final_servicing_provider_address"),
                    value = srvc_geocoded,
                    overwrite = T)
  print("Service geocoding done!")
  
  bllng_cleaned = lapply(mcaid_bllng, function(a){
    x = kc_singleline(a, unparsed = T)
    if (is.null(x$candidates)){
      r = (data.table(input = a))
    } else if (((length(x$candidates)==0))){
      r = (data.table(input=a))
    } else{
      r = as.data.table(x$candidates[[1]]$attributes)[,c(1:28)]
      r[, input := a]
    }
    return(r)
  })
  bllng_cleaned = rbindlist(bllng_cleaned, fill=TRUE)
  DBI::dbWriteTable(conn = db_hhsaw, 
                    name = DBI::Id(schema = "claims", table = "raw_billing_provider_address"),
                    value = bllng_cleaned,
                    overwrite = T)
  # bllng_cleaned <- setDT(DBI::dbGetQuery(db_hhsaw, glue::glue_sql(
  #   "SELECT * FROM claims.raw_billing_provider_address",
  #   .con = db_hhsaw)))
  bllng_cleaned <- bllng_cleaned[,c("StAddr", "City", "County", "State", "ZIP", "input")]
  setnames(bllng_cleaned,
           c("StAddr", "City", "ZIP", "County", "State"),
           c("geo_add1_raw","geo_city_raw", "geo_zip_raw", "geo_county_raw", "geo_state_raw"))
  bllng_cleaned <- bllng_cleaned[, `:=` (geo_add2_raw=NA, geo_add3_raw=NA)]
  uped = submit_ads_for_cleaning(bllng_cleaned, con = db_hhsaw)
  # wait a bit before continuing!
  a1 = fetch_addresses(bllng_cleaned, input_type = 'raw', geocode = TRUE, con = db_hhsaw, deduplicate = T)
  bllng_geocoded <- cbind(bllng_cleaned, a1)
  bllng_geocoded[, geocode_success:=ifelse(!is.na(geo_address_geocoded), 1, 0)]
  bllng_geocoded <- bllng_geocoded[,c("input", "geo_hash_raw", "geo_hash_clean", "geocode_success")]
  
  DBI::dbWriteTable(conn = db_hhsaw,
                    name = DBI::Id(schema = "claims", table = "final_billing_provider_address"),
                    value = bllng_geocoded,
                    overwrite = T)
  print("Billing geocoding done!")
}

## Load data ----
# Load if not already loaded
if (!exists("srvc_geocoded")){
  srvc_geocoded <- setDT(DBI::dbGetQuery(db_hhsaw, glue::glue_sql(
    "SELECT input, geo_hash_raw, geo_hash_clean, geocode_success
     FROM claims.final_servicing_provider_address",
    .con = db_hhsaw)))
} else{
  srvc_geocoded <- srvc_geocoded[,c("input", "geo_hash_raw", "geo_hash_clean",
                                    "geocode_success")]
  srvc_geocoded$addr_type <- "servicing_provider"
}
if (!exists("bllng_geocoded")){
  bllng_geocoded <- setDT(DBI::dbGetQuery(db_hhsaw, glue::glue_sql(
    "SELECT input, geo_hash_raw, geo_hash_clean, geocode_success
     FROM claims.final_billing_provider_address",
    .con = db_hhsaw)))
} else{
  bllng_geocoded <- bllng_geocoded[,c("input", "geo_hash_raw", "geo_hash_clean",
                                      "geocode_success")]
  bllng_geocoded$addr_type <- "billing_provider"
}
if (!exists("hmis_geocoded")){
  hmis_geocoded <- setDT(DBI::dbGetQuery(db_hhsaw, glue::glue_sql(
    "SELECT geo_add1_raw, geo_city_raw, geo_zip_raw,
      geo_hash_raw, geo_hash_clean, geocode_success
     FROM kfukutaki.hmis_provider_geocoded",
    .con = db_hhsaw)))
  hmis_geocoded$addr_type <- "HMIS"
} else{
  hmis_geocoded <- hmis_geocoded[,c("geo_add1_raw", "geo_city_raw",
                                  "geo_zip_raw", "geo_hash_raw", "geo_hash_clean",
                                  "geocode_success")]
  hmis_geocoded$addr_type <- "HMIS"
}

# Check for new addresses - if there are any, change recode_mcaid_addresses to T
# and go back to rerun that section
setdiff(mcaid_srvc, srvc_geocoded$input)
setdiff(mcaid_bllng, bllng_geocoded$input)

# Rename columns
setnames(srvc_geocoded,
         c("input"),
         c("SERVICING_PRVDR_ADDRESS"))
setnames(bllng_geocoded,
         c("input"),
         c("BILLING_PRVDR_ADDRESS"))

## Merge cleaned addresses for mcaid ----
all_srvc <- merge(mcaid, srvc_geocoded, by="SERVICING_PRVDR_ADDRESS", all = F)
all_bllng <- merge(mcaid, bllng_geocoded, by="BILLING_PRVDR_ADDRESS", all = F)

all_srvc <- all_srvc[!is.na(SERVICING_PRVDR_ADDRESS),]
all_bllng <- all_bllng[!is.na(BILLING_PRVDR_ADDRESS),]

all_srvc <- all_srvc[,c("SERVICING_PRVDR_ADDRESS", "PRVDR_LAST_NAME", "PRVDR_FIRST_NAME",
                        "TXNMY_NAME", "FCLTY_TYPE_CODE", "geo_hash_clean", "geocode_success",
                        "SYSTEM_IN_DATE")]
all_bllng <- all_bllng[,c("BILLING_PRVDR_ADDRESS", "PRVDR_LAST_NAME", "PRVDR_FIRST_NAME",
                          "TXNMY_NAME", "FCLTY_TYPE_CODE", "geo_hash_clean", "geocode_success",
                          "SYSTEM_IN_DATE")]

all_srvc[, bllng_or_srvc:="S"]
all_bllng[, bllng_or_srvc:="B"]

all_srvc <- merge(all_srvc, srvc_newest, by = "SERVICING_PRVDR_ADDRESS", all.x = T)
all_srvc <- merge(all_srvc, srvc_oldest, by = "SERVICING_PRVDR_ADDRESS", all.x = T)
all_bllng <- merge(all_bllng, bllng_newest, by = "BILLING_PRVDR_ADDRESS", all.x = T)
all_bllng <- merge(all_bllng, bllng_oldest, by = "BILLING_PRVDR_ADDRESS", all.x = T)

setnames(all_srvc,
         c("SERVICING_PRVDR_ADDRESS", "SYSTEM_IN_DATE"),
         c("Address", "source_last_updated"))
setnames(all_bllng,
         c("BILLING_PRVDR_ADDRESS", "SYSTEM_IN_DATE"),
         c("Address", "source_last_updated"))

all_addr <- rbindlist(list(all_srvc, all_bllng), use.names=T, fill=T)

all_addr[, code_source:="Medicaid - Place of Service"]
all_addr$bed_type_emergency_shelters <- NA

## Rbind HMIS ----
all_hmis <- merge(hmis, hmis_geocoded, by=c("geo_add1_raw", "geo_city_raw", "geo_zip_raw"))
all_hmis <- setDT(tidyr::unite(all_hmis, Address, geo_add1_raw:geo_zip_raw, sep = ' '))
setnames(all_hmis,
         c("Agency.Name", "Program.Name", "Project.Type", "Address", "geo_hash_raw",
           "Programs.Operating.Start.Date", "Programs.Operating.End.Date",
           "Programs.Housing.Type", "Bed.Type.(Emergency.Shelters.Only)",
           "addr_type"),
         c("PRVDR_FIRST_NAME", "PRVDR_LAST_NAME", "FCLTY_TYPE_CODE", "Address", "geo_hash_raw",
           "operating_earliest_date", "operating_latest_date",
           "TXNMY_NAME", "bed_type_emergency_shelters",
           "code_source"))
all_hmis[, bllng_or_srvc:="S"]

all_addr <- rbindlist(list(all_addr, all_hmis), use.names=T, fill=T)
all_addr <- rads::string_clean(all_addr)
all_addr <- unique(all_addr,
                   by = c("Address", "code_source", "bllng_or_srvc", "FCLTY_TYPE_CODE")
)

## Rollup logic ----
# Merge on code and code_source
all_addr <- merge(all_addr, crosswalk, by.x=c("FCLTY_TYPE_CODE", "code_source"), by.y=c("code", "code_source"), all.x=T)

# Add unknown codes for those who have no code
all_addr[, health := ifelse(code_source == "Medicaid - Place of Service", 1, 0)]
all_addr[, housing := ifelse(code_source == "HMIS", 1, 0)]
all_addr[, address_midlevel_desc := ifelse((code_source == "Medicaid - Place of Service") & is.na(FCLTY_TYPE_CODE), "Unknown Health Facility", address_midlevel_desc)]
all_addr[, address_midlevel_desc := ifelse((code_source == "HMIS") & is.na(FCLTY_TYPE_CODE), "Unknown Housing Assistance", address_midlevel_desc)]

## Final column cleaning ----
setnames(all_addr,
         c("FCLTY_TYPE_CODE", "code_source", "Address", "PRVDR_LAST_NAME",
           "PRVDR_FIRST_NAME", "TXNMY_NAME", "geo_hash_clean",
           "bllng_or_srvc", "operating_latest_date", "operating_earliest_date", "bed_type_emergency_shelters", "health", "housing",
           "address_midlevel_desc", "address_detail_desc"),
         c("fclty_type_code", "code_source", "address_orig", "provider_name",
           "provider_first_name", "txnmy_name", "geo_hash_clean",
           "bllng_or_srvc", "operating_latest_date", "operating_earliest_date",
           "bed_type_emergency_shelters", "health", "housing",
           "address_midlevel_desc", "address_detail_desc")
         )
all_addr <- all_addr[,c("geo_hash_clean", "geocode_success", "address_orig",
                        "provider_name", "address_midlevel_desc",
                        "address_detail_desc", "fclty_type_code", "code_source",
                        "bed_type_emergency_shelters", "health", "housing",
                        "txnmy_name", "bllng_or_srvc", "operating_latest_date",
                        "operating_earliest_date", "source_last_updated"
                       )]
setcolorder(all_addr, c("geo_hash_clean", "geocode_success", "address_orig",
                        "provider_name", "address_midlevel_desc",
                        "address_detail_desc", "fclty_type_code", "code_source",
                        "bed_type_emergency_shelters", "health", "housing",
                        "txnmy_name", "bllng_or_srvc", "operating_latest_date",
                        "operating_earliest_date", "source_last_updated"
                        ))
all_addr[, last_r_run := Sys.Date()]


## Upload ----
DBI::dbWriteTable(conn = db_hhsaw, 
                  name = DBI::Id(schema = "ref", table = "address_reference"),
                  value = all_addr,
                  overwrite = T)

