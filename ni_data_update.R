library(tidyverse)
library(sf)
library(janitor)

# Import and tidy NI data

# Shapefiles for super output areas

ni_sf <- read_sf("ni_data/SOA2011_Esri_Shapefile_0")

ni_sf

ni_sf <- st_simplify(ni_sf, dTolerance = 30)

ni_sf <- st_transform(ni_sf, crs = 4326) %>%
  st_cast("MULTIPOLYGON")

ni_sf %>%
  slice(1:10) %>%
  ggplot() +
  geom_sf()


# Add SOA to LGD lookup

SOA_lookup <- readODS::read_ods("ni_data/lgd_lookup.ods", skip = 4)

SOA_lookup <- as_tibble(SOA_lookup)

# remove dupes
SOA_lookup <- SOA_lookup %>%
  select(SOA, HSCT) %>%
  filter(!duplicated(SOA))

SOA_lookup <- SOA_lookup %>%
  janitor::clean_names(case = "snake")


# Add SOA name lookup

soa_clnm_lu <- read_csv("ni_data/soaname_lookup.csv") %>%
  select(LSOA11CD, LSOA11NM) %>%
  filter(!duplicated(LSOA11CD)) %>%
  rename(soa_code = LSOA11CD, soa_name = LSOA11NM)


# Add Population data

# Ethnicity %

ni_ethnicity <- read_csv("ni_data/ni_ethnicity.csv", skip = 2) %>%
  select(-X3) %>%
  clean_names() %>%
  select(soa, ethnic_group_white_percent:ethnic_group_other_percent) %>%
  slice(-1) %>%
  left_join(., soa_clnm_lu, by = c("soa" = "soa_name")) %>%
  select(soa_name = soa, soa_code, everything()) 

# National Identity %

ni_nationalid <- read_csv("ni_data/ni_identity.csv", skip = 2) %>%
  select(-X3) %>%
  clean_names() %>%
  select(soa, national_identity_british_percent:national_identity_other_percent) %>%
  slice(-1) %>%
  left_join(., soa_clnm_lu, by = c("soa" = "soa_name")) %>%
  select(soa_name = soa, soa_code, everything()) 
#  filter(is.na(soa_code))


# Religion or Religion Brought Up In %

ni_religion <- read_csv("ni_data/ni_religion.csv", skip = 2) %>%
  select(-X3) %>%
  clean_names() %>%
#  names(.)
  select(soa, religion_or_religion_brought_up_in_catholic_percent:religion_or_religion_brought_up_in_none_percent) %>%
  slice(-1) %>%
  left_join(., soa_clnm_lu, by = c("soa" = "soa_name")) %>%
  select(soa_name = soa, soa_code, everything()) 
#  filter(is.na(soa_code))


# Add IMD data
imd_keeplist <- read_csv("ni_data/nimd/nimdm2017-variables.csv") %>%
  filter(VarName == "MDM_rank" | str_detect(Format, "Rate") | str_detect(Format, "Percentage"))


ni_imd_data <- read_csv("ni_data/nimd/nimdm2017-soa.csv") %>%
  select(SOA2001, SOA2001name, imd_keeplist$VarName)


# join all data 
ni_data <- left_join(soa_clnm_lu, SOA_lookup, by = c("soa_code" = "soa")) %>%
  left_join(., ni_ethnicity, by = c("soa_code", "soa_name")) %>%
  left_join(., ni_nationalid, by = c("soa_code", "soa_name")) %>%
  left_join(., ni_religion, by = c("soa_code", "soa_name")) %>%
  left_join(., ni_imd_data, by = c("soa_code" = "SOA2001")) %>%
  select(-SOA2001name)

# create variable name list
names(ni_data)



ni_vardescs <- c(
  "(Northern Ireland Census 2011) Ethnic Group White Percent (%)",
  "(Northern Ireland Census 2011) Ethnic Group Chinese Percent (%)",
  "(Northern Ireland Census 2011) Ethnic Group Irish Traveller Percent (%)",
  "(Northern Ireland Census 2011) Ethnic Group Indian Percent (%)",
  "(Northern Ireland Census 2011) Ethnic Group Pakistani Percent (%)",
  "(Northern Ireland Census 2011) Ethnic Group Bangladeshi Percent (%)",
  "(Northern Ireland Census 2011) Ethnic Group Other Asian Percent (%)",
  "(Northern Ireland Census 2011) Ethnic Group Black Caribbean Percent (%)",
  "(Northern Ireland Census 2011) Ethnic Group Black African Percent (%)",
  "(Northern Ireland Census 2011) Ethnic Group Black Other Percent (%)",
  "(Northern Ireland Census 2011) Ethnic Group Mixed Percent (%)",
  "(Northern Ireland Census 2011) Ethnic Group Other Percent (%)",
  "(Northern Ireland Census 2011) National Identity British Percent (%)",
  "(Northern Ireland Census 2011) National Identity Irish Percent (%)",
  "(Northern Ireland Census 2011) National Identity Northern Irish Percent (%)",
  "(Northern Ireland Census 2011) National Identity English Percent (%)",
  "(Northern Ireland Census 2011) National Identity Scottish Percent (%)",
  "(Northern Ireland Census 2011) National Identity Welsh Percent (%)",
  "(Northern Ireland Census 2011) National Identity Other Percent (%)",
  "(Northern Ireland Census 2011) Religion or Religion Brought Up In Catholic Percent (%)",
  "(Northern Ireland Census 2011) Religion or Religion Brought Up In Protestant & Other Christian Percent (%)",
  "(Northern Ireland Census 2011) Religion or Religion Brought Up In Other Religions Percent (%)",
  "(Northern Ireland Census 2011) Religion or Religion Brought Up In None Percent (%)",
  paste("(Northern Ireland IMD 2017)", imd_keeplist$Variable)
  )


ni_varnames <- c(
  "NI_ETHWHPC",
  "NI_ETHCHPC",
  "NI_ETHITPC",
  "NI_ETHINDPC",
  "NI_ETHPAKPC",
  "NI_ETHBANPC",
  "NI_ETHOTHASPC",
  "NI_ETHBLCPC",
  "NI_ETHBLAPC",
  "NI_ETHBLOTHPC",
  "NI_ETHMIXPC",
  "NI_ETHOTHPC",
  "NI_NIBRITPC",
  "NI_NIIRISHPC",
  "NI_NINORIRIPC",
  "NI_NIENGPC",
  "NI_NISCOTPC",
  "NI_NIWELPC",
  "NI_NIOTHPC",
  "NI_RELCATHPC",
  "NI_RELPROT",
  "NI_RELOTH",
  "NI_RELNONE",
  toupper(paste0("NI_",  imd_keeplist$VarName))
)


# Join to new rows for variable names lookup file

ni_varlabels <- tibble(var_name = ni_varnames, label = ni_vardescs)


# rename cols for parity

names(ni_data) <- c("LSOA11CD", "LSOA11NM", "LA_name", ni_varlabels$var_name)

ni_data


# join to sf
names(ni_sf) <- c("LSOA11CD", "LSOA11NM", "geometry")

ni_data_sf <- left_join(ni_sf %>% select(-LSOA11NM), ni_data, by = "LSOA11CD")


# add to MOG data and MOG data variable labels
ni_data_sf

ni_varlabels

lsoa_data_spatial <- read_rds("data/mog_datav2.Rds")
labels_lsoa_data <- read_csv("data/lsoa_dataset_labelsv2.csv")

temp <- st_join(lsoa_data_spatial, ni_data_sf)
temp2 <- st_join(ni_data_sf, lsoa_data_spatial)

combined_sf <- do.call(rbind, list(temp, temp2))

names(combined_sf)

combined_sf <- combined_sf %>%
  rename(LSOA11CD = LSOA11CD.x, LSOA11NM = LSOA11NM.x, LA_name = LA_name.x) %>%
  select(-LSOA11CD.y, -LSOA11NM.y, -LA_name.y, -LsoaName)



# extend labels

combined_labels <- bind_rows(labels_lsoa_data, ni_varlabels)

# Save new versions of data

write_rds(combined_sf, path = "data/mog_datav3.Rds")
write_csv(combined_sf, path = "data/mog_datav3.csv")
write_csv(combined_labels, "data/lsoa_dataset_labelsv3.csv")














