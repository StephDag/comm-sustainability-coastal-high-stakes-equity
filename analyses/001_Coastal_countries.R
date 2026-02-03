#***********************************************************************
# List of coastal countries and eez and iso-a3 codes, etc.
# --------------------------------------------------------------
# INPUTS:
# - eez territories
# - countries' iso codes + regions
# 
# OUTPUTS
# - coastal.ctr object with coastal country list and codes
#***********************************************************************
# Creation : Stéphanie D'Agata
# Email :stephanie.dagata@ird.fr
# ORCID : https://orcid.org/0000-0001-6941-8489
# Institution : Institut de Recherche pour le Developpement
#***********************************************************************

# load eez
eez <- read.csv(here("data","raw-data","List_territory","eez_territory_region.csv"))
head(eez)
dim(eez) # 221

# list of countries with regions, etc.
ctr.region <- read.csv(here("data","raw-data","ISO-3166-Countries-with-Regional-Codes-master","all","all.csv"))
head(ctr.region)

# coastal countries
ctr <- read.csv(here("data","raw-data","country-coastline-distance-master","coastlines.csv"))
head(ctr)
ctr[which(ctr$iso2 == ""),]

# modify iso-a2 codes of Namibia and Netherlands Antilles
ctr[which(ctr$iso2 == ""),"iso2"] <- c("NA","AN","PS")
ctr[!ctr$iso2%in%ctr.region$alpha.2,]

# filter coastal countries
coastal.ctr <- ctr %>% filter(coastline_wf >0)

# create full country database
coastal.ctr <- left_join(coastal.ctr,ctr.region,by=c("country"="name"))
head(coastal.ctr);tail(coastal.ctr)
coastal.ctr[which(coastal.ctr$iso2 == ""),"iso2"] <- c("NA","AN")

# check
coastal.ctr %>% filter(country == "Namibia")
ctr.region  %>% filter(name == "Namibia")
