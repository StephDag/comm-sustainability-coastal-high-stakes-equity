#***********************************************************************
# Generate a spatial dataframe with contextual inequity variables in column, cells in raws
# --------------------------------------------------------------
# INPUTS:
# - The three raster stack (SE, inequity, governance)
# 
# OUTPUTS
# - spatial dataframe and dataframe of the variables
# - sf_df.risk_stack_ctry.rds
# - pop.coastal.ctry.pixel.rds
# - df.cont.inequity.compo.coastal.rds
#***********************************************************************
# Creation : Stéphanie D'Agata
# Email :stephanie.dagata@ird.fr
# ORCID : https://orcid.org/0000-0001-6941-8489
# Institution : Institut de Recherche pour le Développement
#***********************************************************************

library(here)
source(here::here("analyses","00_setup.R"))
source(here::here("analyses","001_Coastal_countries.R"),echo=T)
source(here("R","NormMinMax.R"))

# Get the country boundaries data - sf dataframe
countries <- ne_countries(returnclass = "sf",scale = 10) # 258 countries and territories

# filter by coastal countries
countries.shp.coastal <- countries %>%
  filter(iso_a2 %in% coastal.ctr$iso2)
dim(countries.shp.coastal)

# check with small countries are present
countries.shp.coastal %>%
  filter(name_en == "Samoa") %>%
  names()

# # # load raster for sc
risk.stack.SE.risk <- terra::rast(here("data","derived-data","Spatial rasters","risk.stack.SE.risk.tif"))
risk.stack.gov <- terra::rast(here("data","derived-data","Spatial rasters","risk.stack.gov.tif"))
risk.stack.ineq <- terra::rast(here("data","derived-data","Spatial rasters","risk.stack.ineq.tif"))
plot(risk.stack.SE.risk$povmap.grdi.v1)

  # risk raster to df
df.risk.stack.sc.temp.risk.1 <- terra::as.data.frame(risk.stack.SE.risk[[c(1,2)]],xy=T); gc()
df.risk.stack.sc.temp.risk.2 <- terra::as.data.frame(risk.stack.SE.risk[[c(3,4)]],xy=T); gc()
df.risk.stack.sc.temp.risk.3 <- terra::as.data.frame(risk.stack.SE.risk[[c(5,6)]],xy=T); gc()
df.risk.stack.sc.temp.risk.4 <- terra::as.data.frame(risk.stack.SE.risk[[c(7,8)]],xy=T); gc()
df.risk.stack.sc.temp.risk.5 <- terra::as.data.frame(risk.stack.SE.risk[[c(9,10)]],xy=T); gc()
df.risk.stack.sc.temp.risk.6 <- terra::as.data.frame(risk.stack.SE.risk[[c(11,12,13)]],xy=T); gc()

  # governance raster to df
df.risk.stack.sc.temp.gov.1 <- terra::as.data.frame(risk.stack.gov[[c(1,2)]],xy=T); gc()
df.risk.stack.sc.temp.gov.2 <- terra::as.data.frame(risk.stack.gov[[c(3,4)]],xy=T); gc()
df.risk.stack.sc.temp.gov.3 <- terra::as.data.frame(risk.stack.gov[[c(5,6)]],xy=T); gc()

# ineq raster to df
df.risk.stack.sc.temp.ineq.1 <- terra::as.data.frame(risk.stack.ineq[[c(1,2)]],xy=T); gc()
df.risk.stack.sc.temp.ineq.2 <- terra::as.data.frame(risk.stack.ineq[[c(3,4,5)]],xy=T); gc()

# # left join with main
 # gravity + poverty
rm(df.risk.stack.sc.ctry.full)
 gc()
 df.risk.stack.sc.temp.risk.1 <- df.risk.stack.sc.temp.risk.1 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.risk.1))
 df.risk.stack.sc.ctry.full <- df.risk.stack.sc.temp.risk.1
 names(df.risk.stack.sc.ctry.full)
#   # 0
 df.risk.stack.sc.temp.risk.2 <- df.risk.stack.sc.temp.risk.2 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.risk.2)); gc()
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.risk.2 %>% dplyr::select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full)

# # 1
 df.risk.stack.sc.temp.risk.3 <- df.risk.stack.sc.temp.risk.3 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.risk.3)); gc()
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.risk.3 %>% dplyr::select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full)
 # df.risk.stack.sc.ctry.full$Voice_account.x <- NULL
 # df.risk.stack.sc.ctry.full$Political_stab.x <- NULL
 # names(df.risk.stack.sc.ctry.full)[177:178] <- gsub(".y","",names(df.risk.stack.sc.ctry.full)[177:178])
 # 
 
# # 2
 df.risk.stack.sc.temp.risk.4 <- df.risk.stack.sc.temp.risk.4 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.risk.4))
  df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.risk.4 %>% dplyr::select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full)
#
# # clean memory
 rm(df.risk.stack.sc.temp.risk.2,df.risk.stack.sc.temp.risk.3,df.risk.stack.sc.temp.risk.4)
#
# # 3
 df.risk.stack.sc.temp.risk.5 <- df.risk.stack.sc.temp.risk.5 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.risk.5))
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.risk.5%>% dplyr::select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full)
 rm(df.risk.stack.sc.temp.risk.5)
# # 4
 df.risk.stack.sc.temp.risk.6 <- df.risk.stack.sc.temp.risk.6 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.risk.6))
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.risk.6 %>% dplyr::select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full) 
 rm(df.risk.stack.sc.temp.risk.6)
 
 # gov
 df.risk.stack.sc.temp.gov.1 <- df.risk.stack.sc.temp.gov.1 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.gov.1))
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.gov.1 %>% dplyr::select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full) 
 rm(df.risk.stack.sc.temp.gov.1)
 
 # # 6
 df.risk.stack.sc.temp.gov.2 <- df.risk.stack.sc.temp.gov.2 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.gov.2))
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.gov.2 %>% dplyr::select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full) 
 rm(df.risk.stack.sc.temp.gov.2)
 
 # # 7
 df.risk.stack.sc.temp.gov.3 <- df.risk.stack.sc.temp.gov.3 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.gov.3))
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.gov.3 %>% dplyr::select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full) 
 rm(df.risk.stack.sc.temp.gov.3)
 
 ### equity
 df.risk.stack.sc.temp.ineq.1 <- df.risk.stack.sc.temp.ineq.1 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.ineq.1))
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.ineq.1 %>% dplyr::select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full) 
 rm(df.risk.stack.sc.temp.ineq.1)
 
 df.risk.stack.sc.temp.ineq.2 <- df.risk.stack.sc.temp.ineq.2 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.ineq.2))
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.ineq.2 %>% dplyr::select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full) 
 rm(df.risk.stack.sc.temp.ineq.2)
 
 # compute perc total pop
 df.risk.stack.sc.ctry.full <- df.risk.stack.sc.ctry.full %>%
   group_by(iso_a3) %>%
   mutate(tot.pop.coastal = sum(pop.world.coastal,na.rm=T)) %>%
   mutate(tot.pop.coastal.merit = sum(pop.world.coastal.merit.10m,na.rm=T)) %>%
   mutate(tot.pop = sum(pop.tot,na.rm=T)) %>%
   mutate(perc.pop.world.coastal.merit.10m = round(100*(pop.world.coastal.merit.10m/tot.pop.coastal.merit),5)) %>%
   ungroup()
 
 summary(df.risk.stack.sc.ctry.full)
 
 # filter coastal countries
 df.risk.stack.sc.ctry.full.coastal <- df.risk.stack.sc.ctry.full %>%
   filter(iso_a3 %in% countries.shp.coastal$iso_a3)
 
 # character to numeric governance indicators
 df.risk.stack.sc.ctry.full.coastal <- df.risk.stack.sc.ctry.full.coastal %>% 
   dplyr::mutate_at(c('Voice_account', 'Political_stab','Gov_effect','Reg_quality','Rule_law','control_corr'), as.character) 
 
 df.risk.stack.sc.ctry.full.coastal <- df.risk.stack.sc.ctry.full.coastal %>% 
   dplyr::mutate_at(c('Voice_account', 'Political_stab','Gov_effect','Reg_quality','Rule_law','control_corr'), as.numeric)
summary(df.risk.stack.sc.ctry.full)

#  # test with Mauritius
#  df.risk.stack.sc.ctry.full %>% filter(iso_a3 == "MUS") %>%
#    dplyr::select(pop_est,tot.pop.coastal,tot.pop.coastal.merit,tot.pop,perc.pop.world.coastal.merit.10m) %>%
#    distinct()
# 
# hist(df.risk.stack.sc.ctry.full.coastal$le.ineq.change.sc)

 ### log1p and rescale
 df.risk.stack.sc.ctry.full.coastal <- df.risk.stack.sc.ctry.full.coastal %>%
   mutate(mean.count.grav.V2.log = log1p(mean.count.grav.V2)) %>%
   mutate(Economic.dependence.log = log1p(Economic.dependence)) %>%
   mutate(Nutritional.dependence.sc = stdize(Nutritional.dependence,na.rm = T)) %>%
   mutate(Voice_account.sc = 1-stdize(Voice_account, na.rm = T)) %>%
   mutate(Political_stab.sc = 1-stdize(Political_stab, na.rm = T)) %>%
   mutate(Gov_effect.sc = 1-stdize(Gov_effect, na.rm = T)) %>%
   mutate(Reg_quality.sc = 1-stdize(Reg_quality, na.rm = T)) %>%
   mutate(Rule_law.sc = 1-stdize(Rule_law, na.rm = T)) %>%
   mutate(control_corr.sc = 1-stdize(control_corr, na.rm = T)) %>%
   mutate(gender.ineq.sc = 1-stdize(gender.ineq, na.rm = T)) %>%
   mutate(perc.pop.world.coastal.merit.10m.log = log1p(perc.pop.world.coastal.merit.10m))%>%
   mutate(income.ineq.sc=stdize(income.ineq, na.rm = T))%>%
   mutate(le.ineq.log=log1p(life.exp.ineq))%>%
   mutate(income.ineq.change.sc=stdize(income.ineq.change,na.rm=T))%>%
   mutate(le.ineq.change.sc=stdize(life.exp.ineq.change,na.rm=T))  
  
 # standaridze the log variables
 df.risk.stack.sc.ctry.full.coastal <- df.risk.stack.sc.ctry.full.coastal %>%
 mutate(Economic.dependence.sc = stdize(Economic.dependence.log, na.rm = T)) %>%
 mutate(mean.count.grav.V2.log.sc = stdize(mean.count.grav.V2.log, na.rm = T)) %>%
 mutate(povmap.grdi.v1.sc = stdize(povmap.grdi.v1, na.rm = T)) %>%
 mutate(perc.pop.world.coastal.merit.10m.log.sc = stdize(perc.pop.world.coastal.merit.10m.log, na.rm = T)) %>%
 mutate(le.ineq.log.sc = stdize(le.ineq.log, na.rm = T))
 
 hist(df.risk.stack.sc.ctry.full.coastal$povmap.grdi.v1)
 
 # save total coastal population by country
 pop.coastal.ctry <- df.risk.stack.sc.ctry.full.coastal %>%
   dplyr::select(iso_a3,pop.world.coastal) %>%
   group_by(iso_a3) %>%
   summarize(pop.coastal.ctry = sum(pop.world.coastal,na.rm=T))
 saveRDS(pop.coastal.ctry,here("data","derived-data","pop.coastal.ctry.rds"))
 
 # save total coastal population by country
 pop.coastal.ctry.pixel <- df.risk.stack.sc.ctry.full.coastal %>%
   dplyr::select(iso_a3,ID,pop.world.coastal,pop.tot)
 saveRDS(pop.coastal.ctry.pixel,here("data","derived-data","pop.coastal.ctry.pixel.rds"))
 
 # save
 saveRDS(df.risk.stack.sc.ctry.full.coastal,here("data","derived-data","sf_df.risk_stack_ctry.rds"))

 # rm spatial
 # # load df ctry
 df.risk.stack.sc.ctry.full.coastal <- readRDS(here("data","derived-data","sf_df.risk_stack_ctry.rds"))
 
 # select only variables, country/eez names, remove all geometry to work on a simplified dataframe for the rest of the computation (and clean the space)
 df.risk.stack.sc.ctry.full.coastal.ind <- df.risk.stack.sc.ctry.full.coastal %>%
   st_drop_geometry(x) %>%
   dplyr::select(ID,name_en,iso_a3,economy,income_grp,
                 mean.count.grav.V2.log.sc,povmap.grdi.v1.sc,perc.pop.world.coastal.merit.10m.log.sc,
                 Nutritional.dependence.sc,Economic.dependence.sc,
                 Voice_account.sc,Political_stab.sc,Gov_effect.sc,Reg_quality.sc ,Rule_law.sc,control_corr.sc,
                 gender.ineq.sc,income.ineq.sc,le.ineq.log.sc,income.ineq.change.sc,le.ineq.change.sc)
 
 # check
 df.risk.stack.sc.ctry.full.coastal.ind %>% filter(iso_a3 == "TLS") %>% summary()
 df.risk.stack.sc.ctry.full.coastal.ind %>% filter(iso_a3 == "COM") %>% summary()
 df.risk.stack.sc.ctry.full.coastal %>% filter(iso_a3 == "FJI") %>% summary()
 df.risk.stack.sc.ctry.full.coastal %>% filter(iso_a3 == "MUS") %>% summary()
 df.risk.stack.sc.ctry.full.coastal %>% filter(iso_a3 == "NRU") %>% summary()
 df.risk.stack.sc.ctry.full.coastal %>% filter(iso_a3 == "SLB") %>% summary()
 df.risk.stack.sc.ctry.full.coastal.ind %>% filter(is.na(gender.ineq.sc)) %>% dplyr::select(iso_a3) %>% 
   unique() %>% as.data.frame()
 
 df.risk.stack.sc.ctry.full.coastal.ind %>%
   filter(iso_a3 == "CAN") %>%
   summary()

 # save clean df
 saveRDS(df.risk.stack.sc.ctry.full.coastal.ind,here("data","derived-data","df.cont.inequity.compo.coastal.rds"))
 
 # check number of countries with data for each variable
 df.risk.stack.sc.ctry.ind.coastal <- readRDS(here("data","derived-data","df.cont.inequity.compo.coastal.rds"))
 head(df.risk.stack.sc.ctry.ind.coastal)
 names(df.risk.stack.sc.ctry.ind.coastal)

 # check number of countries
 # SE risk
 df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(povmap.grdi.v1.sc)) %>%
   dplyr::select(iso_a3) %>% unique() %>% dim() # 172 countries
 
 df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(Nutritional.dependence.sc)) %>%
   dplyr::select(iso_a3) %>% unique() %>% dim() # 139 countries
 
 df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(Economic.dependence.sc)) %>%
   dplyr::select(iso_a3,name_en) %>% unique() %>% as.data.frame() %>% dim() # 137 countries

 df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(perc.pop.world.coastal.merit.10m.log.sc)) %>%
   dplyr::select(iso_a3) %>% unique() %>% dim() # 178 countries
 
 df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(control_corr.sc)) %>%
   dplyr::select(iso_a3) %>% unique() %>% dim() # 163 countries
 
 df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(gender.ineq.sc)) %>%
   dplyr::select(iso_a3) %>% unique() %>% dim() # 165 countries
 
 df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(income.ineq.sc)) %>%
   dplyr::select(iso_a3) %>% unique() %>% dim() # 134 countries
 
 df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(le.ineq.log.sc)) %>%
   dplyr::select(iso_a3) %>% unique() %>% dim() # 150 countries
 
 df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(income.ineq.change.sc)) %>%
   dplyr::select(iso_a3) %>% unique() %>% dim() # 111 countries
 
 df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(le.ineq.change.sc)) %>%
   dplyr::select(iso_a3) %>% unique() %>% dim() # 150 countries

 
 
 