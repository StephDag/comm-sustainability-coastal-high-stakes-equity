#***********************************************************************
# Generate the raster stack of the contextual inequity variables
# --------------------------------------------------------------
# INPUTS:
# - sources data for each of the 14 variables
# 
# OUTPUTS
# - Raster stacks for socio-ecological vulnerability, governance and inequity data
#***********************************************************************
# Creation : Stéphanie D'Agata
# Email :stephanie.dagata@ird.fr
# ORCID : https://orcid.org/0000-0001-6941-8489
# Institution : Institut de Recherche pour le Développement
#***********************************************************************

#################################################

library(here)
source(here::here("analyses","00_setup.R"))

# source coastal countries script
source(here::here("analyses","001_Coastal_countries.R"),echo=T)

# coastal countries shapefile
# Get the country boundaries data - sf dataframe
countries <- ne_countries(returnclass = "sf",scale = 10) # 258 countries and territories

# filter by coastal countries
countries.shp.coastal <- countries %>%
  filter(iso_a2 %in% coastal.ctr$iso2)
dim(coastal.ctr)
dim(countries.shp.coastal)
head(countries.shp.coastal)

# check with small countries are present
countries.shp.coastal %>%
  filter(name_en == "Comoros")

# Define projection : mollweide
crs<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# ##########################################
# #               Population               #
# ##########################################
#
 pop.world.nc <- terra::rast(here::here("data","raw-data","Word Population count  SEDAC 5km",
                                        "gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev11_totpop_2pt5_min_nc",
                                        "gpw_v4_population_count_adjusted_rev11_2pt5_min.nc"))

# # # the 4th raster is the count population for 2015: 4	Population Count, v4.11 (2015) (see doc)
 pop.world <- pop.world.nc[[4]]
# # writeRaster(pop.world,here("data","derived-data","Spatial rasters","pop.world.tif"))
# #
# #  pop.world <- rast(here("data","derived-data","Spatial rasters","pop.world.tif"))
# #  plot(pop.world)
# # # # 
# # # # # mollweide projection
# #  #pop.world.proj <- terra::project(pop.world,y="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs",method="bilinear",res=4500)
# #  pop.world.proj <- terra::project(pop.world,y=crs,method="bilinear",res=4500)
# #  writeRaster(pop.world.proj,here("data","derived-data","Spatial rasters","pop.world.proj.tif"),overwrite=TRUE)
# 
# # plot(pop.world.proj)
pop.world.proj <- rast(here("data","derived-data","Spatial rasters","pop.world.proj.tif"))

# ##########################################
# #               Biodiversity             #
# ##########################################
# # 
# # specie.grav <- terra::rast(here("data","derived-data","Spatial rasters","sp.count.rast.ter.grav.tif"))
# # plot(specie.grav)
# # 
# # # # project to mollweide
# # specie.grav.proj <- project(specie.grav,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs",method="bilinear",res=4500)
# # # plot(specie.grav.proj)
# # writeRaster(specie.grav.proj,here("data","derived-data","Spatial rasters","specie.grav.proj.tif"),overwrite=TRUE)
# # 
# # # # resample species grav to match population raster
# #  specie.grav.proj <- rast(here("data","derived-data","Spatial rasters","specie.grav.proj.tif"))
# # #
# #  specie.grav.resample <- terra::resample(specie.grav.proj, pop.world.proj, method="bilinear")
# # plot(specie.grav.resample)
# #  writeRaster(specie.grav.resample,here("data","derived-data","Spatial rasters","specie.grav.resample.tif"),overwrite=TRUE)
# 
 specie.grav.resample <- rast(here("data","derived-data","Spatial rasters","specie.grav.resample.tif"))
# 
# # check crs
 crs(specie.grav.resample) == crs(pop.world.proj)
# 
# ##########################################
# #      Coastal Population Crop           #
# ##########################################
# 
# # crop to species gravity/coastal
# pop.world.proj.coastal <- crop(pop.world.proj, specie.grav.resample,mask=T)
# plot(pop.world.proj.coastal)
# writeRaster(pop.world.proj.coastal,here("data","derived-data","Spatial rasters","pop.world.proj.coastal.tif"),overwrite=TRUE)
 pop.world.proj.coastal <- rast(here("data","derived-data","Spatial rasters","pop.world.proj.coastal.tif"))
 crs(specie.grav.resample) == crs(pop.world.proj.coastal)
# # # test to match raster
test <- c(specie.grav.resample,pop.world.proj.coastal) # ok, raster match
# # 
# # # # # total coastal pop by country based on coastal pop raster used in the study
# df.risk.stack.sc.ctry.full <- readRDS(here("data","derived-data","df.cont.inequity.compo.coastal.rds"))
# df.risk.stack.sc.ctry.full <- df.risk.stack.sc.ctry.full %>%
#   dplyr::select(ID:region_wb)
# names(df.risk.stack.sc.ctry.full); gc()

# # # rm(df.pop.world.proj.coastal)
# df.pop.world.proj.coastal <- terra::as.data.frame(pop.world.proj.coastal,xy=T)
# names(df.pop.world.proj.coastal)[3] <- "pop.world.proj.coastal"
# df.pop.world.proj.coastal <- df.pop.world.proj.coastal %>%
# mutate(ID = rownames(df.pop.world.proj.coastal)); gc()
# names(pop.world.proj.coastal)
# head(df.pop.world.proj.coastal)
# # # 
# # # # left join with country level information
# df.risk.stack.sc.ctry.full.pop <- df.risk.stack.sc.ctry.full %>%
# left_join(df.pop.world.proj.coastal,by="ID")
# # # 
# # # # # summarize by country
#  total.coastal.pop <- df.risk.stack.sc.ctry.full.pop %>%
#    group_by(iso_a3) %>%
#    summarize(total.coast.pop = sum(pop.world.proj.coastal))
# #
# # # # rasterize total coastal pop
#  countries.shp.coastal <-  countries.shp.coastal %>%
#    left_join(total.coastal.pop,by="iso_a3") %>%
#    mutate(perc.coastal.pop.est = round(total.coast.pop/pop_est,2))

# ##########################################
# #                 SLR  risk                  #
# ##########################################
# # 
#    low.lying.SLR <- rast(here("data","raw-data","lecz-urban-rural-population-land-area-estimates-v3-merit-leczs-geotiff","lecz_v3_spatial_data","data","merit_leczs.tif"))
#    plot(low.lying.SLR)
# #  #
# #  # # # mollweide
# low.lying.SLR.proj <- terra::project(low.lying.SLR,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# crs(low.lying.SLR.proj) == crs(specie.grav.resample)
# writeRaster(low.lying.SLR.proj,here("data","derived-data","Spatial rasters","low.lying.SLR.proj.2.tif"))
#  low.lying.SLR.proj <- rast(here("data","derived-data","Spatial rasters","low.lying.SLR.proj.2.tif"))
# 
# #  # aggregate
#  r2 <- aggregate(low.lying.SLR.proj, fact =8 ,fun="min",cores=10)
#  writeRaster(r2,here("data","derived-data","Spatial rasters","r2.1km.tif"))
# 
# #  r4 <- aggregate(r2, fact =4 ,fun="min",cores=10)
# #  writeRaster(r4,here("data","derived-data","Spatial rasters","r2.4km.tif"))
# #
# # resample
#  low.lying.SLR.resample <- terra::resample(r2,pop.world.proj, method="min")
#  plot(low.lying.SLR.resample)
#  writeRaster(low.lying.SLR.resample,here("data","derived-data","Spatial rasters","low.lying.SLR.resample.tif"),overwrite=TRUE)
#  low.lying.SLR.resample <- rast(here("data","derived-data","Spatial rasters","low.lying.SLR.resample.tif"))
# 
#   #crs(low.lying.SLR.resample) == crs(pop.world.proj)
# # # crop to species gravity/coastal
# low.lying.SLR.proj.resample.coastal <- crop(low.lying.SLR.resample, specie.grav.resample,mask=T)
# plot(low.lying.SLR.proj.resample.coastal)
# ext(low.lying.SLR.proj.resample.coastal) == ext(specie.grav.resample)
# 
# terra::writeRaster(low.lying.SLR.proj.resample.coastal,here("data","derived-data","Spatial rasters","low.lying.SLR.proj.resample.coastal.tif"),overwrite=TRUE)
 low.lying.SLR.proj.resample.coastal <- rast(here("data","derived-data","Spatial rasters","low.lying.SLR.proj.resample.coastal.tif"))
# 
# # # filter values <= 10
  low.lying.SLR.proj.resample.coastal.10 <- clamp(low.lying.SLR.proj.resample.coastal, upper=10)
# #  plot(low.lying.SLR.proj.resample.coastal.10)
# #  
# #  # crop human population with 10m LLA
  pop.lla.10m <- crop(pop.world.proj.coastal,low.lying.SLR.proj.resample.coastal.10,mask=T)
  plot(pop.lla.10m)
# # # 
# #   # country level - from ND Gain
# # SLR.score <- read.csv(here("data","raw-data","nd_gain_country_index_2023","resources","indicators","id_infr_02","score.csv"),
# #                            header=T,sep=",")
# # 
# # SLR.score_NA <- SLR.score %>%
# #   filter(!is.na(X2015))
# # dim(SLR.score_NA) # 151 countries
# # head(SLR.score_NA) # 151 countries
# # 
# # # change names
# # names(SLR.score_NA)[3:29] <- paste("SLR",names(SLR.score_NA)[3:29],sep="_")
# # 
# # # in countries directly
# # countries.shp.coastal <-  countries.shp.coastal %>%
# #   left_join(SLR.score_NA,by=c("iso_a3" = "ISO3"))
# # dim(countries.shp.coastal) # 179 countries
# 
# # #### project
# # SLR.score_NA.sf.proj <- st_transform(SLR.score_NA,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# # crs(SLR.score_NA.sf.proj) == crs(pop.world.proj)
# # 
# # #### 
# # SLR.score_NA.sf.proj.2015 <- rasterize(SLR.score_NA.sf.proj,pop.world.proj, field="X2015")
# # plot(SLR.score_NA.sf.proj.2015)
# # 
# # SLR.score_coastal <- crop(SLR.score_NA.sf.proj.2015, specie.grav.resample, mask=T)
# # plot(SLR.score_coastal)
# 
# ##########################################
# #             Poverty                    #
# # ##########################################
# depriv <- terra::rast(here("data","raw-data","povmap-grdi-v1-geotiff","povmap-grdi-v1.tif"))
#   crs(depriv) <- "EPSG:4326"
# # # plot(depriv$`povmap-grdi-v1`)#,col=wes_palette("Zissou1", 10, type = "continuous"))
# # # 
# # # # # # projected
# #   target_crs <- "ESRI:54009"  # Mollweide projection
# #   res_orig <- terra::res(depriv)
# #   r <- depriv$`povmap-grdi-v1`
# #   terraOptions(threads = 4)
# #   res_m <- c(463, 463)
# #   target_crs <- "ESRI:54009"  # Mollweide
# #   
# #   depriv.proj <- terra::project(
# #     r,
# #     target_crs,
# #     method = "cubic",
# #     res = res_m,
# #     filename = here("data", "derived-data", "Spatial rasters", "depriv.proj.tif"),
# #     overwrite = TRUE
# #   )
# #   depriv.proj <- terra::project(, target_crs, method = "near", res = res_orig)
# #   writeRaster(depriv.proj,here("data","derived-data","Spatial rasters","depriv.proj.mollweide.tif"),overwrite=T)
# 
#   rm(depriv.proj)
#   depriv.proj <- terra::rast(here("data","derived-data","Spatial rasters","depriv.proj.mollweide.Qgis.tif"))
#   crs(depriv.proj) <- crs(specie.grav.resample) 
#   
# plot(depriv.proj)
# 
# # # # # # resample species grav to match population raster
# depriv.proj.resample <- terra::resample(depriv.proj, pop.world.proj, method="bilinear")
# writeRaster(depriv.proj.resample,here("data","derived-data","Spatial rasters","depriv.proj.resample.tif"),overwrite=T)
# ext(specie.grav.resample) == ext(depriv.proj.resample)
# crs(specie.grav.resample) == crs(depriv.proj.resample)
#  plot(depriv.proj.resample,col=wes_palette("Zissou1", 10, type = "continuous"))
# # # 
# # # # # # # crop to species gravity/coastal
# depriv.proj.resample.coastal <- crop(depriv.proj.resample, specie.grav.resample,mask=T)
# plot(depriv.proj.resample.coastal,col=wes_palette("Zissou1", 10, type = "continuous"))
# # # #
# # # # # # let's save this raster so we don't have to do this again:
# terra::writeRaster(depriv.proj.resample.coastal, "data/derived-data/Spatial rasters/depriv.proj.resample.coastal.tif",overwrite=T)
# # 
# # now simply load raster:
depriv.proj.resample.coastal <- terra::rast("data/derived-data/Spatial rasters/depriv.proj.resample.coastal.tif")
plot(depriv.proj.resample.coastal)
summary(values(depriv.proj.resample.coastal))

# #############################################
# #.          Enabling conditions             #
# #############################################
# 
# #enabling_CM <- read.csv(here("data","raw-data","Cisneiros Montemayor 2021","Enabling_conditions_equitable_sustainable_Blue_Economy_-_Cisneros-Montemayor_et_al_2021.csv"))
# # 
# # # load data
 gender_ineq <- read.csv(here("data","raw-data","WID_Data_Metadata","WID_Data_02052025-144622.csv"),sep=";")
# #gender_ineq <- read.delim(here("data","raw-data","HDR23-24_Composite_indices_complete_time_series.csv"),sep="")
# 
# #   # extract country iso_code_a2
gender_ineq_2015 <- gender_ineq %>%
  mutate(iso_a2 = substr(gender_ineq[,2], 21, 22))

gender_ineq_2015 %>% filter(iso_a2 =="KM")

# # add iso_as
gender_ineq_2015 <- left_join(gender_ineq_2015,countries %>% dplyr::select(iso_a2,iso_a3,name),by=c("iso_a2" = "iso_a2"))

# # all other
#   # from WID
# #wealth_ineq_indicators_ctry.coastal <- read.csv(here("data","derived-data","wealth_ineq_indicators_ctry.coastal.csv"))
 ineq.db <- read.csv(here("data","raw-data","human_development_inequality_indices_time_series.csv"))
#
# # compute tendencies for income inequality and gender inequality from 2010 to 2022
ineq.db <- ineq.db %>%
  mutate(rate.ineq.inc = round((ineq_inc_2010-ineq_inc_2022)/ineq_inc_2022,2),
         rate.ineq.edu = round((ineq_edu_2010-ineq_edu_2022)/ineq_edu_2022,2),
         rate.ineq.le = round((ineq_le_2010-ineq_le_2022)/ineq_le_2022,2))

 ineq.db %>% filter(is.na(rate.ineq.edu)) %>% dplyr::select(iso3)
# 
# #   # subset
ineq.db.subset <- ineq.db %>%
  dplyr::select("iso3","avg_se_m","avg_pr_f","avg_pr_m","avg_lfpr_f","avg_lfpr_m","avg_gii","avg_ineq_le",
                "avg_ineq_edu","avg_ineq_inc","avg_ihdi","rate.ineq.inc","rate.ineq.edu","rate.ineq.le")
# # add gender
wealth_ineq_indicators_ctry.coastal <- left_join(gender_ineq_2015,ineq.db.subset,by=c("iso_a3" = "iso3"))
head(wealth_ineq_indicators_ctry.coastal)

# # # test countries
# wealth_ineq_indicators_ctry.coastal %>% filter(!is.na(avg_ineq_edu)) %>% dplyr::select(iso_a2)

countries.shp.coastal <-  countries.shp.coastal %>%
  left_join(wealth_ineq_indicators_ctry.coastal %>% dplyr::select(-Country),by=c("iso_a2" = "iso_a2","iso_a3"="iso_a3"))
dim(countries.shp.coastal) # 179 countries

# countries.shp.coastal %>% filter(iso_a2 == "KM")
# 
# # # check missing countries
countries.shp.coastal[which(is.na(countries.shp.coastal$perc_labour_female_p0p100)==T),"name_en"]%>% as.data.frame()
countries.shp.coastal[which(is.na(countries.shp.coastal$avg_ineq_inc)==T),"name_en"]%>% as.data.frame()
countries.shp.coastal[which(is.na(countries.shp.coastal$avg_ineq_edu)==T),"name_en"]%>% as.data.frame()
countries.shp.coastal[which(is.na(countries.shp.coastal$rate.ineq.inc)==T),"name_en"] %>% as.data.frame()

# # ###########################################
# # #     Marine  nutritional dependency      #
# # ###########################################
# # 
marine.dep <- read.csv("https://ourworldindata.org/grapher/fish-and-seafood-consumption-per-capita.csv?v=1&csvType=full&useColumnShortNames=true")
head(marine.dep)
dim(marine.dep)

names(marine.dep)[4] <- "fish_consumpt_capita"
#
marine.dep.NA <- marine.dep %>%
  filter(!is.na(fish_consumpt_capita)) %>%
  filter(Year == 2015)
#
# #   # check countries
 marine.dep.NA[which(marine.dep.NA$Entity %in% countries$name_en== F),"Entity"]
#
 marine.dep.NA <- marine.dep.NA %>%
   mutate(Entity = as.character(Entity)) %>%
       mutate(country.clean=case_when(
         Entity=="Sao Tome and Principe" ~ "São Tomé and Príncipe",
         Entity=="Cote d'Ivoire" ~ "Ivory Coast",
         Entity=="Gambia" ~ "The Gambia",
         Entity=="Congo" ~ "Republic of the Congo",
         Entity=="Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
         Entity=="Micronesia" ~ "Federated States of Micronesia",
         Entity=="China" ~ "People's Republic of China",
         Entity=="Czechia" ~ "Czech Republic",
         Entity=="Bahamas" ~ "The Bahamas",
         Entity=="Macao" ~ "Macau",
         TRUE ~  Entity))

# # check countries
marine.dep.NA[which(marine.dep.NA$country.clean %in% countries$name_en== F),"country.clean"]

# # add to countries
countries.shp.coastal <-  countries.shp.coastal %>%
  left_join(marine.dep.NA,by=c("iso_a3" = "Code"))
dim(countries.shp.coastal)
names(countries.shp.coastal)

countries.shp.coastal %>% filter(is.na(fish_consumpt_capita)) %>% dplyr::select(name_en) %>% as.data.frame()

# ###########################################
# #.          Marine  economic dependency             #
# ###########################################
# 
marine.dep.eco <- read.csv(here("data","raw-data","Selig2019","Selig&al2019_Dependance_national_marine.csv"),sep=";")
head(marine.dep.eco)
dim(marine.dep.eco)

marine.dep.eco.NA <- marine.dep.eco %>%
  filter(!is.na(Economic.dependence))

  # check countries
marine.dep.eco.NA[which(marine.dep.eco.NA$Country %in% countries$name_en== F),"Country"]

marine.dep.eco.NA <- marine.dep.eco.NA %>%
  mutate(Country = as.character(Country)) %>%
      mutate(country.clean=case_when(
        Country=="Virgin Islands (U.S.)" ~ "United States Virgin Islands",
        Country=="Sao Tome and Principe" ~ "São Tomé and Príncipe",
        Country=="Cote D'Ivoire" ~ "Ivory Coast",
        Country=="Gambia, The" ~ "The Gambia",
        Country=="Congo" ~ "Republic of the Congo",
        Country=="Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
        Country=="Curacao" ~ "Curaçao",
        Country=="Wallis and Futuna Islands" ~ "Wallis and Futuna",
        Country=="Timor Leste" ~ "East Timor",
        Country=="French Southern Territories" ~ "French Southern and Antarctic Lands",
        Country=="China" ~ "People's Republic of China",
        Country=="French Guiana" ~ "Guyana",
        Country=="Brunei Darussalam" ~ "Brunei",
        Country=="Great Britain" ~ "United Kingdom",
        Country=="United States" ~ "United States of America",
        Country=="The Former Yugoslav Republic of Macedonia" ~ "Moldova",
        Country=="Swaziland" ~ "Swaziland",
        TRUE ~  Country))

# check countries
marine.dep.eco.NA[which(marine.dep.eco.NA$country.clean %in% countries$name_en== F),"country.clean"]

# add to countries
countries.shp.coastal <-  countries.shp.coastal %>%
  left_join(marine.dep.eco.NA,by=c("name_en" = "Country"))
dim(countries.shp.coastal)

# check
countries.shp.coastal %>% filter(is.na(name_en))

# ###########################################
# # Dependency Andrello                     #
# ###########################################
# 
marine.dep.Andrello <- read.csv(here("data","raw-data","Andrello2017","41467_2017_BFncomms16039_MOESM290_ESM.csv"),sep=";",dec=",")
head(marine.dep.Andrello)
dim(marine.dep.Andrello)

# check countries with difference names
marine.dep.Andrello[which(marine.dep.Andrello$EEZ %in% countries.shp.coastal$name_en == F),"EEZ"]

marine.dep.Andrello <- marine.dep.Andrello %>%
  mutate(EEZ = as.character(EEZ)) %>%
  mutate(EEZ.clean=case_when(
    EEZ=="Virgin Islands of the United States" ~ "United States Virgin Islands",
    EEZ=="Sao Tome and Principe" ~ "São Tomé and Príncipe",
    EEZ=="Gambia" ~ "The Gambia",
    EEZ=="R\xe9publique du Congo" ~ "Republic of the Congo",
    EEZ=="Cura\xe7ao" ~ "Curaçao",
    EEZ=="China" ~ "People's Republic of China",
    EEZ=="French Guiana" ~ "Guyana",
    EEZ=="Great Britain" ~ "United Kingdom",
    EEZ=="United States" ~ "United States of America",
    EEZ=="Micronesia" ~ "Federated States of Micronesia",
    EEZ=="Bahamas" ~ "The Bahamas",
    EEZ=="Comoro Islands" ~ "Comoros",
    TRUE ~  EEZ))
marine.dep.Andrello[which(marine.dep.Andrello$EEZ.clean %in% countries.shp.coastal$name_en == F),"EEZ.clean"]
countries.shp.coastal %>% filter(iso_a3 == "CUW")

# add iso_3
countries.shp.coastal <- countries.shp.coastal %>%
  left_join(marine.dep.Andrello,by=c("name_en"="EEZ.clean"),relationship = "many-to-many")

# ###########################################
# # Subsistence fishing Virdin et al., 2023 #
# ##########################################
# 
subs.fishing <- read.csv(here::here("data","raw-data","Virdin2023","subs_employ_fishing.csv"),sep=";",dec=".",header=T)
head(subs.fishing)

# add iso_3
countries.shp.coastal <- countries.shp.coastal %>%
  left_join(subs.fishing %>% dplyr::select(iso3code,Marine),by=c("iso_a3"="iso3code"))

# ###########################################
# #.              Governance                #
# ###########################################
# 
rm(WB_Gov_2010_2020)
WB_Gov_2010_2020 <- read.csv(here("data","raw-data","P_Data_Extract_From_Worldwide_Governance_Indicators_V2.csv"),sep=";",dec=",")
head(WB_Gov_2010_2020)
# summarize over the 2010 - 2020 decade

WB_Gov_2015 <- WB_Gov_2010_2020 %>%
  gather(key="gov_indicators", value="value",-Time,-Time.Code,-Country.Name,-Country.Code) %>%
  filter(Country.Code != "") %>%
  mutate(value = as.numeric(value)) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::group_by(Country.Code,gov_indicators) %>%
  summarize(value = mean(value,na.rm=T)) %>%
  spread(key=gov_indicators,value=value) %>%
  as.data.frame() # 214 Countries

# WB_gov <- read.csv(here("data","raw-data","WB_GOV_2015.csv"),header=T,sep=";")
# head(WB_gov)
# summary(WB_gov)
WB_Gov_2015 %>% filter(Country.Code == "TLS")
# 
# # add governance indicators
countries.shp.coastal <-  countries.shp.coastal %>%
  left_join(WB_Gov_2015,by=c("iso_a3" = "Country.Code"))

 saveRDS(countries.shp.coastal,here("data","derived-data","indicators_countries.shp.coastal.rds"))

countries.shp.coastal<- readRDS(here("data","derived-data","indicators_countries.shp.coastal.rds"))

######################################################################
#   Create a data.frame for country level information and rasterize  #
######################################################################

#### project
countries.data.sf.proj <- st_transform(countries.shp.coastal,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
crs(countries.data.sf.proj) == crs(pop.world.proj.coastal)

countries.data.sf.proj.vt <- vect(countries.data.sf.proj)

### raster country
world.2 <- countries %>%
  st_transform(crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

world.2.raster <- c(rasterize(world.2,pop.world.proj,touches=T,field="iso_a3"),
                    rasterize(world.2,pop.world.proj,touches=T,field="name_en"),
                    rasterize(world.2,pop.world.proj,touches=T,field="pop_est"),
                    rasterize(world.2,pop.world.proj,touches=T,field="gdp_md"),
                    rasterize(world.2,pop.world.proj,touches=T,field="economy"),
                    rasterize(world.2,pop.world.proj,touches=T,field="income_grp"))
world.2.raster.coastal <- crop(world.2.raster, specie.grav.resample,mask=T)
terra::writeRaster(world.2.raster.coastal,here("data","derived-data","Spatial rasters","world.2.raster.coastal.tif"),overwrite=T)

####### nutritional
  # rasterize polygon
marine.dep.nutri <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="fish_consumpt_capita",touches=T)
plot(marine.dep.nutri)
terra::writeRaster(marine.dep.nutri,here("data","derived-data","Spatial rasters","marine.dep.nutri.tif"),overwrite=T)

# crop to species gravity/coastal  - it works to crop a spatraster with coastal raster
marine.dep.nutri.coastal <- crop(marine.dep.nutri, specie.grav.resample,mask=T)
plot(marine.dep.nutri.coastal)
terra::writeRaster(marine.dep.nutri.coastal,here("data","derived-data","Spatial rasters","marine.dep.nutri.coastal.tif"),overwrite=T)

####### nutritional Andrello
# rasterize polygon
marine.dep.nutri.Andrel <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Fisheries.dependency..food.security",touches=T)
plot(marine.dep.nutri.Andrel)
terra::writeRaster(marine.dep.nutri.Andrel,here("data","derived-data","Spatial rasters","marine.dep.nutri.Andrel.tif"),overwrite=T)

# crop to species gravity/coastal  - it works to crop a spatraster with coastal raster
marine.dep.nutri.coastal.Andrel <- crop(marine.dep.nutri.Andrel, specie.grav.resample,mask=T)
plot(marine.dep.nutri.coastal.Andrel)
terra::writeRaster(marine.dep.nutri.coastal.Andrel,here("data","derived-data","Spatial rasters","marine.dep.nutri.coastal.Andrel.tif"),overwrite=T)

####### economic
marine.dep.econ <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Economic.dependence",touches=T)
# crop to species gravity/coastal 
marine.dep.econ.coastal <- crop(marine.dep.econ, specie.grav.resample,mask=T)
plot(marine.dep.econ.coastal)

####### economic Andrello
marine.dep.econ.andrello <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Fisheries.dependency..economy",touches=T)
# crop to species gravity/coastal                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
marine.dep.econ.coastal.andrello <- crop(marine.dep.econ.andrello, specie.grav.resample,mask=T)
plot(marine.dep.econ.coastal.andrello)

####### Employment Andrello
marine.dep.empl.andrello <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Fisheries.dependency..employment",touches=T)
# crop to species gravity/coastal 
marine.dep.empl.coastal.andrello <- crop(marine.dep.empl.andrello, specie.grav.resample,mask=T)
plot(marine.dep.empl.coastal.andrello)

####### subsistence fishing - Virdin 2023
# marine.subs <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Ratio.of.subsistence.fishing.to.employment.in.harvestng.SSF",touches=T)
# # crop to species gravity/coastal 
# marine.subs.coastal.virdin<- crop(marine.subs, specie.grav.resample,mask=T)
# plot(marine.subs.coastal.virdin)

###### governance
  # voice account
Voice_Account <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Voice_and_Accountability",touches=T)
# crop to species gravity/coastal 
Voice_Account.coastal <- crop(Voice_Account, specie.grav.resample,mask=T)
plot(Voice_Account.coastal)
  # polit stab
Polit_stab <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Political_Stability_and_Absence_of_Violence_Terrorism",touches=T)
# crop to species gravity/coastal 
Polit_stab.coastal <- crop(Polit_stab, specie.grav.resample,mask=T)
plot(Polit_stab.coastal)
  # polit stab
Gov_Effect <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Government_Effectiveness",touches=T)
# crop to species gravity/coastal 
Gov_Effect_coastal <- crop(Gov_Effect, specie.grav.resample,mask=T)
plot(Gov_Effect_coastal)
# Reg_quality_2015
Reg_quality <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Regulatory_Quality",touches=T)
# crop to species gravity/coastal 
Reg_quality_coastal <- crop(Reg_quality, specie.grav.resample,mask=T)
plot(Reg_quality_coastal)

# Reg_quality_2015
Rule_Law <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Rule_of_Law",touches=T)
# crop to species gravity/coastal 
Rule_Law_coastal <- crop(Rule_Law, specie.grav.resample,mask=T)
plot(Rule_Law_coastal)

#Control_Corr_2015
Control_Corr <- terra::rasterize(countries.data.sf.proj.vt,specie.grav.resample, field="Control_of_Corruption",touches=T)

#writeVector(countries.data.sf.proj.vt,here("data","derived-data","Spatial rasters","countries.data.vt.shp"))
# crop to species gravity/coastal 
Control_Corr_coastal <- crop(Control_Corr, specie.grav.resample,mask=T)
plot(Control_Corr_coastal)
#terra::writeRaster(Control_Corr,here("data","derived-data","Spatial rasters","Control_Corr.tif"),overwrite=T)
#terra::writeRaster(pop.world.proj,here("data","derived-data","Spatial rasters","pop.world.proj.tif"),overwrite=T)

# # SLR
# SLR.score <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="SLR_X2015")
# # crop to species gravity/coastal 
# SLR.score_coastal <- crop(SLR.score, specie.grav.resample,mask=T)
# plot(SLR.score_coastal)

# gender ineq
ineq.score <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="p0p100",touches=T)
# crop to species gravity/coastal 
ineq.score_coastal <- crop(ineq.score, specie.grav.resample,mask=T)
plot(ineq.score_coastal)
terra::writeRaster(ineq.score_coastal,here("data","derived-data","Spatial rasters","ineq.score_coastal.tif"),overwrite=T)

# income inequity
income.ineq <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="avg_ineq_inc",touches=T)
# crop to species gravity/coastal 
income.ineq_coastal <- crop(income.ineq, specie.grav.resample,mask=T)
plot(income.ineq_coastal)
terra::writeRaster(income.ineq_coastal,here("data","derived-data","Spatial rasters","income.ineq_coastal.tif"),overwrite=T)

# life expectancy inequity
le.ineq <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="avg_ineq_le",touches=T)
# crop to species gravity/coastal 
le.ineq_coastal <- crop(le.ineq, specie.grav.resample,mask=T)
plot(le.ineq_coastal)
terra::writeRaster(le.ineq_coastal,here("data","derived-data","Spatial rasters","le.ineq_coastal.tif"),overwrite=T)

# income inequity change
income.ineq.change <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="rate.ineq.inc",touches=T)
# crop to species gravity/coastal 
income.ineq.change_coastal <- crop(income.ineq.change, specie.grav.resample,mask=T)
plot(income.ineq.change_coastal)
terra::writeRaster(income.ineq.change_coastal,here("data","derived-data","Spatial rasters","income.ineq.change_coastal.tif"),overwrite=T)

# wealth inequity change
le.ineq.change <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="rate.ineq.le",touches=T)
# crop to species gravity/coastal 
le.ineq.change_coastal <- crop(le.ineq.change, specie.grav.resample,mask=T)
plot(le.ineq.change_coastal)
terra::writeRaster(le.ineq.change_coastal,here("data","derived-data","Spatial rasters","le.ineq.change_coastal.tif"),overwrite=T)

# # total coastal population
# total.coastal.pop <- rasterize(countries.data.sf.proj.vt,pop.world.proj.coastal, field="total.coast.pop",touches=T)
# plot(total.coastal.pop)
# crop to species gravity/coastal
# total.coastal.pop_coast <- crop(total.coastal.pop, specie.grav.resample,mask=T)
# plot(total.coastal.pop_coast)
# terra::writeRaster(total.coastal.pop_coast,here("data","derived-data","Spatial rasters","total.coastal.pop_coast.tif"))
# terra::writeRaster(total.coastal.pop,here("data","derived-data","Spatial rasters","total.coastal.pop.tif"),overwrite=T)

# create a pop.lla.raster
pop.world.proj.coastal.merit <- pop.world.proj.coastal
#c(pop.world.proj.coastal,specie.grav.resample,low.lying.SLR.proj.resample.coastal)
ext(low.lying.SLR.proj.resample.coastal) == ext(pop.world.proj.coastal)
pop.world.proj.coastal.merit[low.lying.SLR.proj.resample.coastal[low.lying.SLR.proj.resample.coastal$merit_leczs > 10,]] <- 0
pop.world.proj.coastal.merit[low.lying.SLR.proj.resample.coastal$merit_leczs > 10,] <- 0
 plot(pop.world.proj.coastal.merit)
terra::writeRaster(pop.world.proj.coastal.merit,here("data","derived-data","Spatial rasters","pop.world.proj.coastal.merit.tif"),overwrite=T)
pop.world.proj.coastal.merit <-rast(here("data","derived-data","Spatial rasters","pop.world.proj.coastal.merit.tif"))

#############################################
#        Stack on all raster                #
#############################################

# create stack raster
risk.stack.SE.risk <- c(world.2.raster.coastal,
                  specie.grav.resample, # species gravity
                depriv.proj.resample.coastal, #deprivation poverty index 
                marine.dep.nutri.coastal, # nutrional dependency to marine resources
                marine.dep.empl.andrello,#, # economic dependency to marine resources
                pop.world.proj.coastal.merit,# pop in low lying areas
                pop.world.proj.coastal, # pop world coastal
                pop.world.proj) # 
names(risk.stack.SE.risk) <- c( "iso_a3","name_en","pop_est","gdp_md","economy","income_grp",
                                "mean.count.grav.V2","povmap.grdi.v1",
                                "Nutritional.dependence","Economic.dependence",
                                "pop.world.coastal.merit.10m","pop.world.coastal","pop.tot")

risk.stack.gov <- c(Voice_Account.coastal, # voice account WB
                    Polit_stab.coastal,# political stab
                    Gov_Effect_coastal,#gov effectiveness
                    Reg_quality_coastal,  #regularoty quality "
                    Rule_Law_coastal, # "rule of law"
                    Control_Corr_coastal) # control corruption
names(risk.stack.gov) <- c( "Voice_account","Political_stab","Gov_effect",
                            "Reg_quality","Rule_law","control_corr")

risk.stack.ineq <- c(ineq.score_coastal,  #gender inequality
                    income.ineq_coastal, # income inequality
                    le.ineq_coastal, # life expectancy
                    income.ineq.change_coastal, # income inequality change
                    le.ineq.change_coastal) # life expectancy change
names(risk.stack.ineq) <- c("gender.ineq","income.ineq","life.exp.ineq",
                            "income.ineq.change","life.exp.ineq.change")

               
# clean space of large raster that are not necessary anymore
rm(pop.world.nc,pop.world,specie.grav,specie.grav.proj,pop.world.nc.change,pop.world.change.proj,
   pop.world.change.proj.resample,pop.world.change.proj.resample.coastal,mean.SLR.change,
   mean.SLR.change.proj,mean.SLR.change.proj.resample,mean.SLR.change.proj.coastal,depriv.proj.resample.coastal,
   dis.prep.score,dis.prep_score_coastal,dis.prep_score_NA.sf.proj.2015,fsi.sf.proj.ineq.gender.coastal,fsi.sf.proj.ineq.gender,
   marine.dep.econ.coastal,marine.dep.nutri.coastal,Reg_quality_coastal,Rule_Law_coastal,SLR.score_coastal,SLR.score_NA.sf.proj.2015,
   Control_Corr,Control_Corr_coastal,countries.data.sf.proj.vt,disaster.prep,disaster.prep_coastal,
   Economic.dependence.log,Gov_Effect,Gov_Effect_coastal,low.lying.SLR,low.lying.SLR.crop,low.lying.SLR.proj,low.lying.SLR.proj.resample,
   low.lying.SLR.proj.resample.coastal,low.lying.SLR.proj.resample.coastal.10,low.lying.SLR.resample,mean.count.grav.V2.log);gc()

# save raster 
terra::writeRaster(risk.stack.SE.risk,here("data","derived-data","Spatial rasters","risk.stack.SE.risk.tif"),overwrite=T)
terra::writeRaster(risk.stack.gov,here("data","derived-data","Spatial rasters","risk.stack.gov.tif"),overwrite=T)
terra::writeRaster(risk.stack.ineq,here("data","derived-data","Spatial rasters","risk.stack.ineq.tif"),overwrite=T)

