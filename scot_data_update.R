### SCOTLAND DATA UPDATE

# This script transforms and combines multiple data zone level 
# pieces of open government data in Scotland to scottish data zone
# shape files. 

# Scotland Data update

library(tidyverse)
library(sf)
library(janitor)
library(haven)
library(httr)
library(rvest)
library(SPARQL)

# Shapefiles

scot_sf <- read_sf("scot_data/SG_DataZoneBdry_2011") %>%
  st_simplify(dTolerance = 30) %>%
  st_transform(crs = 4326) %>%
  st_cast("MULTIPOLYGON")

scot_sf %>%
  slice(1:10) %>%
  ggplot() +
  geom_sf()

# Lookup DZ to HSCPName

scot_lu <- read_csv("http://statistics.gov.scot/downloads/file?id=5a9bf61e-7571-45e8-a307-7c1218d5f6b5%2FDatazone2011Lookup.csv") %>%
  select(DataZone, Council)


scot_calu <- read_csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/967937c4-8d67-4f39-974f-fd58c4acfda5/download/ca11_ca19.csv") %>%
  select(CA, CAName, HSCP, HSCPName)

dz_hscp_lookup <- left_join(scot_lu, scot_calu, by = c("Council" = "CA")) %>%
  select(-Council, -CAName, -HSCP) %>%
  rename(data_zone = DataZone, LA_name = HSCPName)

dz_hscp_lookup

# DZ level data
# Census data - can't find a neat version

# Ethnicity of HRFs percentage by data zone
scot_eth <- read_csv("scot_data/LC1201SC.csv") %>%
  rename(data_zone = X1, householdtype = X2) %>%
  clean_names() %>%
  filter(householdtype == "All households") %>%
  mutate_at(vars(white_total:other_ethnic_groups), ~(./all_hr_ps)*100) %>%
  select(-householdtype, -all_hr_ps)

# Add labels (manually) !!!

scot_eth_labels <- read_csv("scot_data/LC1201SC.csv") %>%
  rename(datazone = X1, householdtype = X2) %>%
  select(-datazone, -householdtype, -`All HRPs`) %>%
  names(.) %>%
  str_remove_all(" \\(1\\)") %>%
  paste("(Scottish Census 2011)", ., "(Household Reference Person Percent)")


scot_eth_varnames <- names(scot_eth)[2:length(scot_eth)] %>%
  make_clean_names(., "big_camel") %>%
  abbreviate(., minlength = 11, strict = FALSE, named = FALSE) %>%
  paste0("SCOT_", ., "_CEN")

names(scot_eth)[2:length(scot_eth)] <- scot_eth_varnames

scot_eth
scot_eth_varname_lookup <- tibble(varnames = scot_eth_varnames, varlabel = scot_eth_labels)


#IMD

scot_imd <- tempfile()

download.file(scot_imd, url = "https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-index-of-multiple-deprivation")

# Labels
scot_imd_labels <- read_csv(scot_imd) %>%
  filter(DateCode == "2020" & Measurement == "Rank") %>%
  clean_names() %>%
  mutate(measure_name = paste0("(Scottish IMD 2020) ", simd_domain, " (", units, " Lower = Least Deprived)")) %>%
  .$measure_name %>%
  unique(.)

# Data in tidy format - reverse IMD ranks
scot_imd_data <- read_csv(scot_imd) %>%
  filter(DateCode == "2020" & Measurement == "Rank") %>%
  clean_names() %>%
  mutate(measure_name = paste0(simd_domain, " (", units, ")")) %>%
  select(data_zone = feature_code, value, measure_name) %>%
  pivot_wider(names_from = measure_name, values_from = value) %>%
  clean_names() %>%
  mutate_at(vars(employment_rank:education_skills_and_training_rank), ~max(.)-.)

# variable names
scot_imd_varnames <- names(scot_imd_data)[2:length(scot_imd_data)] %>%
  make_clean_names(., "big_camel") %>%
  abbreviate(., minlength = 11, strict = FALSE, named = FALSE) %>%
  paste0("SCOT_", ., "_SIMD")

names(scot_imd_data)[2:length(scot_imd_data)] <- scot_imd_varnames

scot_imd_data

# Variable name and description lookup
scot_imd_varname_lookup <- tibble(varnames = scot_imd_varnames, varlabel = scot_imd_labels)



# Scot data crime
# 
# scot_crime_ep <- "http://statistics.gov.scot/data/scottish-index-of-multiple-deprivation---crime-indicators"
# scot_crime_ep <- "https://statistics.gov.scot/sparql"
# 
# scot_crime_query <- "
# PREFIX dcat: <http://www.w3.org/ns/dcat#>
# PREFIX dcterms: <http://purl.org/dc/terms/>
# PREFIX owl: <http://www.w3.org/2002/07/owl#>
# PREFIX qb: <http://purl.org/linked-data/cube#>
# PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
# PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
# PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/concept#>
# PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
# PREFIX void: <http://rdfs.org/ns/void#>
# PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
# 
# SELECT ?areacode ?periodname ?ratio 
# WHERE { ?obs <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/scottish-index-of-multiple-deprivation---crime-indicators> .
#         ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?areacode .
#         ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?perioduri . 
#         ?obs <http://statistics.gov.scot/def/measure-properties/ratio> ?ratio .
#         ?perioduri rdfs:label ?periodname .
# }
#         LIMIT 100
#         
# "
# 
# 
# 
# SPARQL(scot_crime_ep, scot_crime_query)

# Crime rates

scot_crime <- tempfile()

download.file(destfile = scot_crime, 
              url = "https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-index-of-multiple-deprivation---crime-indicators")

# Labels
scot_crime_labels <- read_csv(scot_crime) %>%
  mutate(
    measure_name = paste0("(Scottish IMD Crime Domain Data ", DateCode, ") ", Units, " (", Measurement, ")")
  ) %>%
  select(data_zone = FeatureCode, measure_name, value = Value) %>%
  .$measure_name %>%
  unique(.)

# data
scot_crime_data <- read_csv(scot_crime) %>%
  mutate(
    measure_name = paste(Units, Measurement, DateCode)
  ) %>%
  select(data_zone = FeatureCode, measure_name, value = Value) %>%
  pivot_wider(names_from = measure_name, values_from = value) %>%
  clean_names()

# varnames
scot_crime_varnames <- names(scot_crime_data)[2:length(scot_crime_data)] %>%
  make_clean_names(., "big_camel") %>%
  abbreviate(., minlength = 11, strict = FALSE, named = FALSE) %>%
  paste0("SCOT_", ., "_CRI")

names(scot_crime_data)[2:length(scot_crime_data)] <- scot_crime_varnames

# Lookup
scot_crime_data_varnames <- tibble(varnames = scot_crime_varnames, varlabel = scot_crime_labels)

scot_crime_data
scot_crime_data_varnames

# Access to services data
# - Not currently responding

scot_ats <- tempfile()

download.file(destfile = scot_ats, 
              url = "https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-index-of-multiple-deprivation---geographic-access-to-services-indicators")

# labels
scot_ats_labels <- read_csv(scot_ats) %>%
  filter(DateCode == 2018) %>%
  clean_names() %>%
  mutate(
    measure_name = paste0("(Scottish IMD Access to Services 2018) ", destination, " by ", method_of_travel, " (Minutes)")
  ) %>%
  .$measure_name %>%
  unique(.)

# data
scot_ats_data <- read_csv(scot_ats) %>%
  filter(DateCode == 2018) %>%
  clean_names() %>%
  mutate(
    measure_name = paste0(destination, " by ", method_of_travel)
  ) %>%
  select(data_zone = feature_code, measure_name, value) %>%
  pivot_wider(names_from = measure_name, values_from = value) %>%
  clean_names()

# varnames
scot_ats_varnames <- names(scot_ats_data)[2:length(scot_ats_data)] %>%
  make_clean_names(., "big_camel") %>%
  abbreviate(., minlength = 12, strict = FALSE, named = FALSE) %>%
  paste0("SCOT_", ., "_ATS")

names(scot_ats_data)[2:length(scot_ats_data)] <- scot_ats_varnames

#lookup
scot_ats_data_varnames <- tibble(varnames = scot_ats_varnames, varlabel = scot_ats_labels)

scot_ats_data
scot_ats_data_varnames

# Education indicators

scot_education <- tempfile()

download.file(scot_education,
              url = "https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-index-of-multiple-deprivation---education-indicators")

scot_education_labels <- read_csv(scot_education, col_types = list(col_character(), col_character(), col_character(), col_character(), col_double(), col_character())) %>%
  filter(Measurement %in% c("Percent", "Score", "Index") & DateCode == "2015/16-2017/18") %>%
  clean_names() %>%
  mutate(measure_name = paste0("(Scottish Education Indicators 2015/16-2016/17) ", simd_education_indicator, " (", units, ")")) %>%
  .$measure_name %>%
  unique(.)

scot_education_data <- read_csv(scot_education, col_types = list(col_character(), col_character(), col_character(), col_character(), col_double(), col_character())) %>%
  filter(Measurement %in% c("Percent", "Score", "Index") & DateCode == "2015/16-2017/18") %>%
  clean_names() %>%
  mutate(measure_name = paste0(simd_education_indicator, " (", units, ")")) %>%
  select(-date_code, -measurement, -units, -simd_education_indicator) %>%
  pivot_wider(names_from = measure_name, values_from = value) %>%
  clean_names() %>%
  rename(data_zone = feature_code)

scot_education_varnames <- names(scot_education_data)[2:length(scot_education_data)] %>%
  make_clean_names(., "big_camel") %>%
  abbreviate(., minlength = 11, strict = FALSE, named = FALSE) %>%
  paste0("SCOT_", ., "_EDU")

names(scot_education_data)[2:length(scot_education_data)] <- scot_education_varnames


scot_education_data_varnames <- tibble(varnames = scot_education_varnames, varlabel = scot_education_labels)


scot_education_data
scot_education_data_varnames


# Scottish Health indicators
scot_health <- tempfile()
download.file(scot_health,
              url = "https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-index-of-multiple-deprivation---health-indicators")

# Scot health labels
scot_health_labels <- read_csv(scot_health, col_types = list(col_character(), col_character(), col_character(), col_character(), col_double(), col_character())) %>%
  filter(DateCode == "2017/2018" | DateCode == "2017" | DateCode == "2014/15-2017/18" | DateCode == "2015") %>%
  clean_names() %>%
  mutate(
    measure_name = paste0("(Scottish Health Indicators ", date_code, ") ", simd_health_indicator, " (", units, ")")
  ) %>%
  select(-date_code, -measurement, -units, -simd_health_indicator) %>%
  .$measure_name %>%
  unique(.)

# Scot health data
scot_health_data <- read_csv(scot_health, col_types = list(col_character(), col_character(), col_character(), col_character(), col_double(), col_character())) %>%
  filter(DateCode == "2017/2018" | DateCode == "2017" | DateCode == "2014/15-2017/18" | DateCode == "2015") %>%
  clean_names() %>%
  mutate(
    measure_name = paste0(simd_health_indicator, " (", units, date_code, ")")
  ) %>%
  select(-date_code, -measurement, -units, -simd_health_indicator) %>%
  pivot_wider(names_from = measure_name, values_from = value) %>%
  clean_names() %>%
  rename(data_zone = feature_code)

# Create shorter variable names
scot_health_varnames <- names(scot_health_data)[2:length(scot_health_data)] %>%
  make_clean_names(., "big_camel") %>%
  abbreviate(., minlength = 11, strict = FALSE, named = FALSE) %>%
  paste0("SCOT_", ., "_HEL")

names(scot_health_data)[2:length(scot_health_data)] <- scot_health_varnames

# scot health varnames and descriptions lookup 

scot_health_data_varnames <- tibble(varnames = scot_health_varnames, varlabel = scot_health_labels)

scot_health_data
scot_health_data_varnames

# Scotland Employment Indicators
scot_emp <- tempfile()
download.file(scot_emp,
              url = "https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-index-of-multiple-deprivation---employment-indicators")

scot_emp_labels <- read_csv(scot_emp) %>%
  clean_names() %>%
  filter(date_code == 2017) %>%
  mutate(measure_name = paste0("(Scottish Employment 2017) Employment Deprived", " (", measurement, ")")) %>%
  .$measure_name %>%
  unique(.)


scot_emp_data <- read_csv(scot_emp) %>%
  clean_names() %>%
  filter(date_code == 2017) %>%
  mutate(measure_name = paste0("Employment Deprived", measurement)) %>%
  select(data_zone = feature_code, measure_name, value) %>%
  pivot_wider(names_from = measure_name, values_from = value) %>%
  clean_names()

scot_emp_varnames <- names(scot_emp_data)[2:length(scot_emp_data)] %>%
  make_clean_names(., "big_camel") %>%
  abbreviate(., minlength = 11, strict = FALSE, named = FALSE) %>%
  paste0("SCOT_", ., "_EMP")

names(scot_emp_data)[2:length(scot_emp_data)] <- scot_emp_varnames

scot_emp_data_varnames <- tibble(varnames = scot_emp_varnames, varlabel = scot_emp_labels)

scot_emp_data
scot_emp_data_varnames


# Scotland Broadband Access
scot_bb <- tempfile()
download.file(scot_bb, url = "https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-index-of-multiple-deprivation---broadband-access-indicator")

scot_bb_labels <- read_csv(scot_bb) %>%
  clean_names %>%
  mutate(
    measure_name = paste0("(Scottish IMD Broadband Access Indicator 2019) ", units)
  ) %>%
  select(data_zone = feature_code, value, measure_name) %>%
  .$measure_name %>%
  unique(.)

scot_bb_data <- read_csv(scot_bb) %>%
  clean_names %>%
  mutate(
    measure_name = paste0(units)
  ) %>%
  select(data_zone = feature_code, value, measure_name) %>%
  pivot_wider(names_from = measure_name, values_from = value) %>%
  clean_names()

scot_bb_varnames <- names(scot_bb_data)[2:length(scot_bb_data)] %>%
  make_clean_names(., "big_camel") %>%
  abbreviate(., minlength = 11, strict = FALSE, named = FALSE) %>%
  paste0("SCOT_", ., "_BB")

names(scot_bb_data)[2:length(scot_bb_data)] <- scot_bb_varnames

scot_bb_data_varnames <- tibble(varnames = scot_bb_varnames, varlabel = scot_bb_labels)

scot_bb_data
scot_bb_data_varnames

# Scottish Index of Multiple Deprivation - Housing Indicators

scot_hous <- tempfile()
download.file(scot_hous,
              url = "https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-index-of-multiple-deprivation---housing-indicators")


scot_hous_labels <- read_csv(scot_hous) %>%
  clean_names() %>%
  mutate(
    measure_name = paste0("(Scottish IMD Housing Indicators 2011) ", simd_housing_indicator, " (", measurement, ")")
  ) %>%
  .$measure_name %>%
  unique(.)

scot_hous_data <- read_csv(scot_hous) %>%
  clean_names() %>%
  mutate(
    measure_name = paste(simd_housing_indicator, measurement)
  ) %>%
  select(data_zone = feature_code, measure_name, value) %>%
  pivot_wider(names_from = measure_name, values_from = value) %>%
  clean_names() 

scot_hous_varnames <- names(scot_hous_data)[2:length(scot_hous_data)] %>%
  make_clean_names(., "big_camel") %>%
  abbreviate(., minlength = 11, strict = FALSE, named = FALSE) %>%
  paste0("SCOT_", ., "_HOU")

names(scot_hous_data)[2:length(scot_hous_data)] <- scot_hous_varnames
scot_hous_data_varnames <- tibble(varnames = scot_hous_varnames, varlabel = scot_hous_labels)

scot_hous_data
scot_hous_data_varnames

# School attendance rate

scot_schat <- tempfile()
download.file(scot_schat,
              url = "https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fschool-attendance-rate")

scot_schat_labels <- read_csv(scot_schat) %>%
  clean_names() %>%
  filter(date_code == "2016/2017") %>%
  mutate(measure_name = paste0("(Scottish School Attendance ", date_code, ") School attendance rate, ", school_type, " (", units, ")")) %>%
  .$measure_name %>%
  unique(.)

scot_schat_data <- read_csv(scot_schat) %>%
  clean_names() %>%
  filter(date_code == "2016/2017") %>%
  mutate(measure_name = paste("School attendance rate,", school_type, units)) %>%
  select(data_zone = feature_code, measure_name, value) %>%
  pivot_wider(names_from = measure_name, values_from = value) %>%
  clean_names()

scot_schat_varnames <- names(scot_schat_data)[2:length(scot_schat_data)] %>%
  make_clean_names(., "big_camel") %>%
  abbreviate(., minlength = 11, strict = FALSE, named = FALSE) %>%
  paste0("SCOT_", ., "_SCH")

names(scot_schat_data)[2:length(scot_schat_data)] <- scot_schat_varnames
scot_schat_data_varnames <- tibble(varnames = scot_schat_varnames, varlabel = scot_schat_labels)

scot_schat_data
scot_schat_data_varnames


# Combine data

scot_data_combined <- dz_hscp_lookup %>% 
  left_join(., scot_eth, "data_zone") %>%
  left_join(., scot_imd_data, "data_zone") %>%
  left_join(., scot_crime_data, "data_zone") %>%
  left_join(., scot_ats_data, "data_zone") %>%
  left_join(., scot_education_data, "data_zone") %>%
  left_join(., scot_health_data, "data_zone") %>%
  left_join(., scot_emp_data, "data_zone") %>%
  left_join(., scot_bb_data, "data_zone") %>%
  left_join(., scot_hous_data, "data_zone") %>%
  left_join(., scot_schat_data, "data_zone")



scot_data_labels <- bind_rows(scot_eth_varname_lookup,
                              scot_imd_varname_lookup,
                              scot_crime_data_varnames,
                              scot_ats_data_varnames,
                              scot_education_data_varnames,
                              scot_health_data_varnames,
                              scot_emp_data_varnames,
                              scot_bb_data_varnames,
                              scot_hous_data_varnames,
                              scot_schat_data_varnames)

unique(scot_data_labels$varnames)


# Add to geographic data

scot_sf_data_combined <- left_join(scot_sf %>% rename(data_zone = DataZone, msoa11hclnm = Name), scot_data_combined, by = "data_zone") %>%
  rename(LSOA11CD = data_zone)

# Add Scottish Data and Scottish data labels to all data 

lsoa_data_spatial <- read_rds("data/mog_datav3.Rds")
labels_lsoa_data <- read_csv("data/lsoa_dataset_labelsv3.csv")

lsoa_data_spatial

temp <- st_join(lsoa_data_spatial, scot_sf_data_combined)
temp2 <- st_join(scot_sf_data_combined, lsoa_data_spatial)

combined_sf <- do.call(rbind, list(temp, temp2))

names(combined_sf)

combined_sf <- combined_sf %>%
  rename(LSOA11CD = LSOA11CD.x, LA_name = LA_name.x, msoa11hclnm = msoa11hclnm.x) %>%
  select(-LSOA11CD.y, -LA_name.y, -msoa11hclnm.y)

combined_sf
unique(names(combined_sf))

# Combined labels

combined_labels <- bind_rows(labels_lsoa_data, scot_data_labels %>% rename(var_name = varnames, label = varlabel))


# 49 Data Zone level variables for Scottish HSCBs
write_rds(combined_sf, path = "data/mog_datav4.Rds")
write_csv(combined_sf, path = "data/mog_datav4.csv")
write_csv(combined_labels, "data/lsoa_dataset_labelsv4.csv")




