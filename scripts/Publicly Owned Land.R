library(tidyverse)
library(sf)
library(CPALtools)
library(googlesheets4)

libDB <- "C:/Users/micha/CPAL Dropbox/" #dropbox directory
libGH <- "C:/Users/micha/Documents/GitHub/" #github directory

#libDB <- "E:/CPAL Dropbox/" #dropbox directory
#libGH <- "C:/Users/Michael Lopez/Documents/GitHub/" #github directory

##### Import Parcel Account Data
dallas <- st_read(paste0(libDB, "Data Library/Data.gdb"), layer = "Dallas_Simple")
#st_crs(dallas)

accounts <- rio::import(paste0(libDB, "Data Library/Parcel Data/DCAD/2021 Certified/account_info.csv"))
parcels <- st_read(paste0(libDB, "Data Library/Parcel Data/DCAD/2021 Certified/PARCEL2021/PARCEL2021.shp")) %>%
  st_transform(crs = 6584)

# separate column of just owner names and count how many accounts are owned by each owner
# using this to search through owner names for all possible publicly ownerd land
owners <- accounts %>%
  group_by(OWNER_NAME1) %>%
  summarize(count = n())

# data frame containing all possible owner names that fit necessary conditions
pubOwners <- c("DALLAS AREA RAPID TRANSIT", "DART AREA RAPID TRANSIT", "CITY OF", "COUNTY OF", "DALLAS AREA RAPID",
               "DALLAS C ITY OF", "DALLAS CITY", "DALLAS COUNTY", "DALLAS CTY", "DALLAS DIST OFFICE US",
               "DALLAS HOUSING", "DALLAS INDEPENDENT SCHOOL DISTRICT", "DALLAS ISD", "DALLAS POLICE",
               "DALLAS PUBLIC LIBRARY", "DALLAS TIF", "DALLAS TRANSIT SYSTEM", "DALLAS WIC",
               "HOUSING AUTHORITY", "INDEPENDENT SCHOOL", " ISD", "TIF#", "TIF #", "PARKLAND",
               "DALLAS COLLEGE")

#index that will be used to conduct a partial string match against all possible owner names previously identified
index <- grepl(paste0(pubOwners, collapse = "|"), accounts$OWNER_NAME1, ignore.case = TRUE)

# Subset of all accounts that fit identified conditions
pubAccounts <- accounts[index,]  

pubTot <- pubAccounts %>%
  group_by(GIS_PARCEL_ID) %>%
  summarize(count = n())

# Select only Parcels that have been identified
pubParcels <- parcels %>%
  filter(Acct %in% pubTot$GIS_PARCEL_ID) %>%
  .[dallas, ]

plot(pubParcels["geometry"])

#st_write(pubParcels, "Data/Publicly Owned Parcels.gpkg", layer = "DCAD Publicly Owned Parcels")

##### Base Zoning
st_layers("C:/Users/micha/CPAL Dropbox/Data Library/City of Dallas/02_Boundaries and Features/Zoning/Data/CityofDallas_Zoning.gpkg")
zoning <- st_read("C:/Users/micha/CPAL Dropbox/Data Library/City of Dallas/02_Boundaries and Features/Zoning/Data/CityofDallas_Zoning.gpkg", layer = "Base_Zoning")
st_crs(zoning)
sum(zoning$sqmi)
test <- zoning %>%
  filter(zone_dist == "PD")

sum(test$sqmi)
sum(test$sqmi)/sum(zoning$sqmi)

# blah
cleanOwners <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1_mMM9Smz4LndqW-fFx0O5VuIAyvIG3oLxvUdtMbv9Ys/", sheet = "pubDallas")

countOWners <- cleanOwners %>%
  group_by(OWNERSHIP_GROUP) %>%
  summarize(TOT_ACCOUNTS = sum(TOT_ACCOUNTS))
