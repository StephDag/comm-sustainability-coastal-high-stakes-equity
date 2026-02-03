#***********************************************************************
# Figure 1A, 1B and 1C
# --------------------------------------------------------------
# INPUTS:
# - CADC temporal database with equity terms
# - Temporal trends coefficients by country
# - CI country: country.summary.composite.scores.rds (Script 07)
# 
# OUTPUTS
# - Preparatory analysis for Figure 3 panel A,B,C
#***********************************************************************
# Creation : Stéphanie D'Agata
# Email :stephanie.dagata@ird.fr & David Gill
# ORCID : https://orcid.org/0000-0001-6941-8489
# Institution : Institut de Recherche pour le Développement
#***********************************************************************

library(here)
source(here::here("analyses","00_setup.R"))
source(here::here("analyses","001_Coastal_countries.R"),echo=T)

### Figure related
# theme of the figures
my_theme <- theme_minimal(base_size = 16, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# list of coastal countries
#coastal.ctr

# built sf object for Figure 3A
# projected world
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)
crs.val.robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # The World Robinson projection (ESRI 54030)
projected.world = st_transform(world, crs.val.robinson)

#### load last version (21/12/2023) of the OECD db with keywords check
# load dataset with potential projects in coastal countries
oecd.dat.sf.ctry <- readRDS(here("data","derived-data","ocean_ODA_CADC_2010_2021_equity_sf.RDS"))
  # check
names(oecd.dat.sf.ctry)
head(oecd.dat.sf.ctry)

# summarize number of projects in total & equity and non equity
oecd.dat.sf.ctry.n.tot <- oecd.dat.sf.ctry %>%
  mutate(cadc.type = as.factor(cadc.type)) %>%
  dplyr::select(cadc.type,total.project) %>%
  st_drop_geometry() %>%
  dplyr::filter(!is.na(cadc.type)) %>%
  droplevels() %>%
  group_by(cadc.type) %>%
  summarize(n=sum(total.project))
oecd.dat.sf.ctry.n.tot$n %>% sum() # 50177 projects - all initiatives
oecd.dat.sf.ctry.n.tot
# cadc projects = 35440, equity = 27%, non equity = 25972/35440 = 73%

##### for figure #### not sure it is used here
# chose color blind scale for figures
# hcl_palettes(plot = TRUE)
# # number of categories
# q3 <- qualitative_hcl(4, palette = "Harmonic")
# q3
# 
# display_carto_pal(8, "Teal")
# my_colors = carto_pal(8, "Teal")
# my_colors_3 = my_colors[c(3,5,8)]
# *********

# load trend investment by country
df.out.trend.ctr <- read.csv(here("outputs","prop_investment_trend_coeff_by_ctry.csv"))
oecd.dat.invest.by.country <- read.csv(here("outputs","investment_ctry_per_capita.csv"))

# for Figure 3B - main indicators is countries, total investment, % equity in project
# create df - 1 raw = 1 country = mean contextual equity, % equity CADC, total investment (cum2021) # oecd.dat.sf.ctry.total.commited
rm(oecd.dat.sf.ctry.equity)
oecd.dat.sf.ctry.equity <- oecd.dat.sf.ctry %>%
  dplyr::filter(cadc.type %in% c("equity CADC","other CADC")) %>%
  st_drop_geometry() %>%
  dplyr::select(iso_a3,name_en,cadc.type,total.project) %>%
  dplyr::group_by(iso_a3) %>%
  dplyr::mutate(tot.project.CADC = sum(total.project)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(iso_a3,cadc.type) %>%
  dplyr::mutate(tot.project.eq=sum(total.project)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(perc.equi = 100*round(tot.project.eq/tot.project.CADC,3)) %>%
  dplyr::select(-total.project) %>%
  distinct() %>%
  as.data.frame()
oecd.dat.sf.ctry.equity

# merge investment per country and trend for Figure 3B
rm(oecd.dat.sf.ctry.equity.commit.trend)
oecd.dat.sf.ctry.equity.commit.trend <- oecd.dat.sf.ctry.equity %>%
  left_join(df.out.trend.ctr %>% dplyr::select(-X),by="iso_a3") # trend

# add per capita investment
rm(oecd.dat.sf.ctry.equity.commit.trend.percap)
oecd.dat.sf.ctry.equity.commit.trend.percap <- oecd.dat.sf.ctry.equity.commit.trend %>%
  left_join(oecd.dat.invest.by.country%>% dplyr::select(-name_en),by=c("iso_a3","cadc.type"),relationship = "many-to-many") # per capito investment

# rm(oecd.dat.sf.ctry.equity.commit.trend.percap)
oecd.dat.sf.ctry.equity.commit.trend.percap <- oecd.dat.sf.ctry.equity.commit.trend %>%
   left_join(oecd.dat.invest.by.country%>% dplyr::select(-name_en,-X),by=c("iso_a3","cadc.type"),relationship = "many-to-many") # per capito investment

#### To estimate the total and quantile of the top 50% and 75% countries for investment
 # cum sum by country 
 rm(oecd.dat.invest.by.country.cumsum)
 oecd.dat.invest.by.country.cumsum <- oecd.dat.sf.ctry %>%
   dplyr::select(iso_a3,name_en,cadc.type,usd.comm,usd.disb) %>%
   st_drop_geometry() %>%
   dplyr::filter(!is.na(cadc.type)) %>%
   filter(cadc.type != "other ocean economy") %>%
   droplevels() %>%
   dplyr::group_by(iso_a3,name_en,cadc.type) %>%
   dplyr::summarise(usd.comm.ctry.cadc=sum(usd.comm,na.rm=T) %>% round(2),
                    usd.disb.ctry.cadc=sum(usd.disb,na.rm=T) %>% round(2)) %>%
   dplyr::arrange(iso_a3,name_en,cadc.type) %>%
   dplyr::group_by(iso_a3) %>%
   dplyr::mutate(tot.usd.comm.ctry=sum(usd.comm.ctry.cadc,na.rm=T)%>% round(2),
                 tot.usd.disb.ctry=sum(usd.disb.ctry.cadc,na.rm=T)%>% round(2)) %>%
   arrange(desc(tot.usd.comm.ctry)) %>%
   dplyr::select(iso_a3,tot.usd.comm.ctry,tot.usd.disb.ctry) %>%
   distinct() %>%
   ungroup() %>%
   dplyr::mutate(cum.usd.comm.ctry=cumsum(as.numeric(tot.usd.comm.ctry))%>% round(2),
                 cum.usd.disb.ctry=cumsum(tot.usd.disb.ctry)%>% round(2)) %>%
   mutate(perc.cumsum.comm = cum.usd.comm.ctry/sum(tot.usd.comm.ctry),
          perc.cumsum.disb = cum.usd.disb.ctry/sum(tot.usd.disb.ctry))

 oecd.dat.invest.by.country.cumsum %>% filter(iso_a3 == "SOM")
 
### add to 
 oecd.dat.sf.ctry.equity.commit.trend.percap.total <- oecd.dat.sf.ctry.equity.commit.trend.percap %>%
   left_join(oecd.dat.invest.by.country.cumsum %>% dplyr::select(iso_a3,cum.usd.comm.ctry,cum.usd.disb.ctry,perc.cumsum.comm,perc.cumsum.disb),by=c("iso_a3"))
 
head(oecd.dat.sf.ctry.equity.commit.trend.percap.total)

# add sf to map
rm(oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf)
oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf <- oecd.dat.sf.ctry.equity.commit.trend.percap.total %>%
  left_join(projected.world %>% dplyr::select(iso_a3,region_un),by="iso_a3")

#### stats for FIGURE 3B
# number of countries receiving 50% of total commitment
oecd.dat.invest.by.country.cumsum %>% filter(perc.cumsum.comm <0.51) %>%
  dplyr::select(iso_a3) %>% distinct()

# total CADC investment - sanity check
sum(oecd.dat.invest.by.country.cumsum$tot.usd.comm.ctry)

# # left join with main ## already in the sf df
# oecd.dat.invest.by.country <- oecd.dat.invest.by.country %>%
#   left_join(oecd.dat.invest.by.country.cumsum %>% dplyr::select(iso_a3,perc.cumsum.comm,perc.cumsum.disb),by="iso_a3")
# oecd.dat.invest.by.country

# # simple
# oecd.dat.invest.by.country.dist <- oecd.dat.invest.by.country %>%
#   dplyr::select(iso_a3,name_en,tot.usd.comm.ctry,perc.cumsum.comm) %>%
#   distinct()

# find the quartile in cumperc to find the countries with top 50% and 75%
oecd.dat.invest.by.country <- oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf %>% 
  mutate(
    closest_quartile = case_when(
      near(perc.cumsum.comm, 0.25, tol = 0.01) ~ "Q1",
      near(perc.cumsum.comm, 0.50, tol = 0.01) ~ "Q2",
      near(perc.cumsum.comm, 0.75, tol = 0.01) ~ "Q3",
      TRUE ~ NA_character_
    ))

######################### FIGURE 1A #############################
# maps of total commited investment by countries
# select only perc committed
### NOT SURE WHY WE NEED IT
# rm(oecd.dat.sf.ctry.total.commited)
# oecd.dat.sf.ctry.total.commited <- oecd.dat.sf.ctry %>%
#   st_drop_geometry() %>%
#   dplyr::filter(cadc.type != "other ocean economy") %>%
#   dplyr::select(iso_a3,year,cadc.type,usd.comm,usd.disb,total.project) %>%
#   dplyr::group_by(iso_a3,year) %>%
#   dplyr::summarise(usd.comm.year=sum(usd.comm,na.rm=T) %>% round(2),
#             usd.disb.year=sum(usd.disb,na.rm=T) %>% round(2),
#             tot.project.year = sum(total.project)) %>%
#   arrange(iso_a3,year) %>%
#   ungroup() %>%
#   complete(iso_a3,year,fill = list(usd.comm.year = 0,usd.disb.year=0)) %>%
#   group_by(iso_a3) %>%
#   mutate(cum.usd.comm=cumsum(usd.comm.year)%>% round(2),
#          cum.usd.disb=cumsum(usd.disb.year)%>% round(2),
#          tot.project.ctry = tot.project.year) %>%
#   ungroup() %>%
#   as.data.frame()
# oecd.dat.sf.ctry.total.commited %>% filter(iso_a3 == "MAR")
# 
# ## map cumulative number of initiatives
# oecd.dat.sf.ctry.total.commited.sf <- projected.world %>%
#   left_join(oecd.dat.sf.ctry.total.commited,by="iso_a3")

######################### FIGURE 1B and 1C #############################

# load composite scores by pixel
df.risk.stack.sc.ctry.ind.coastal <- readRDS(here("data","derived-data","df.cont.inequity.compo.coastal.with.scores.rds"))

# load composite score by country
cont.ineq.ctry <- readRDS(here("data","derived-data","country.summary.composite.scores.rds"))
#head(cont.ineq.ctry)
#dim(cont.ineq.ctry)# 115 countries
#summary(cont.ineq.ctry)

# join world
cont.ineq.ctry.sf <- projected.world %>%
  left_join(cont.ineq.ctry %>% dplyr::select(-name_en),by="iso_a3")
saveRDS(cont.ineq.ctry.sf,here("data","derived-data","context.inequity.commitement.ctry.rds"))
write.csv(cont.ineq.ctry.sf,here("data","derived-data","context.inequity.commitement.ctry.csv"))

######## FIGURE 3A #############
##### FIGURE 3A - prepare a df to list of countries with NAs for contextual inequity, donors, and NA.committed
##### caracterize countries that have CI value but no investment vs investment but no CI values
#####
# NA contextual inequity
rm(coastal.cont.ineq.ctry.NA)
coastal.cont.ineq.ctry.NA <- df.risk.stack.sc.ctry.ind.coastal %>%
  dplyr::group_by(iso_a3) %>%
  dplyr::mutate(cont.eq.score.mean.rank = mean(hierachical.score.rank.ineq,na.rm=T) %>% round(2),
                cont.eq.sd.rank =  sd(hierachical.score.rank.ineq,na.rm=T) %>% round(2)) %>%
  mutate(cont.eq.score.CV.rank = 100*(cont.eq.sd.rank/cont.eq.score.mean.rank) %>% round(4))%>%
  ungroup() %>%
  dplyr::select(iso_a3,name_en,cont.eq.score.mean.rank) %>%
  distinct() %>%
  mutate(cont.eq.score.mean.rank.V2 = ifelse(is.na(cont.eq.score.mean.rank),"CE_NA",cont.eq.score.mean.rank))

# add trend data
cont.ineq.ctry.sf <- cont.ineq.ctry.sf %>%
  left_join(df.out.trend.ctr,by="iso_a3")

# merge with NA contextual inequity
rm(df.NA)
df.NA <- full_join(coastal.cont.ineq.ctry.NA,oecd.dat.invest.by.country.cumsum %>% dplyr::select(iso_a3,cum.usd.comm.ctry),by="iso_a3")

# add list of OECD donors 
OECD_donors <- read.csv(here("data","raw-data","OECD_donors.csv"),sep=",")

# add to df.NA
df.NA <- full_join(df.NA,OECD_donors,by="iso_a3")
dim(df.NA)

# code countries based on NA and DAC
# df.NA <- df.NA %>%
#   mutate(Donors = as.factor(Donors)) %>%
#   filter(cont.eq.score.mean.rank.V2 != "CE_NA")
# summary(df.NA)

### 
# rm SWZ
df.NA <- df.NA %>% filter(iso_a3 != "SWZ")
df.NA$legend.ctry <- NA
# CE but 0 investment
df.NA[which(df.NA$cont.eq.score.mean.rank.V2 > 0 & is.na(df.NA$cum.usd.comm.ctry) & df.NA$cont.eq.score.mean.rank.V2 != "CE_NA"),"legend.ctry"] = "CE_NO_USD" #- 9 countries
# CE + USD
df.NA[which(df.NA$cont.eq.score.mean.rank.V2 > 0 & df.NA$cum.usd.comm.ctry > 0 & df.NA$cont.eq.score.mean.rank.V2 != "CE_NA"),"legend.ctry"] = "CE_USD" # 78 countries
# NA CE but investment
df.NA[which(df.NA$cont.eq.score.mean.rank.V2 == "CE_NA" & df.NA$cum.usd.comm.ctry > 0),"legend.ctry"] = "TRUE_CE_NA_USD"
# 
# donors
df.NA[which(!is.na(df.NA$Donors) & is.na(df.NA$cum.usd.comm.ctry)),"legend.ctry"] = "DAC_donors_only" # 38
# no data at all: CE_NA_USD_NA
df.NA[which(is.na(df.NA$legend.ctry)),"legend.ctry"] = "CE_NA_USD_NA" # 38

summary(as.factor(df.NA$legend.ctry))

# check if coding filter with the criteria
df.NA %>% filter(legend.ctry == "CE_NO_USD") %>% as.data.frame() # OK - costal, non receiving, CE ok 8 countries
df.NA %>% filter(legend.ctry == "CE_USD") %>% as.data.frame() # CE + USD : 84 countries
df.NA %>% filter(legend.ctry == "TRUE_CE_NA_USD") %>% as.data.frame() # receiving but NA CE ; 30 countries
df.NA %>% filter(legend.ctry == "DAC_donors_only") %>% as.data.frame() # receiving but NA CE , 33 countries
df.NA %>% filter(legend.ctry == "CE_NA_USD_NA") %>% as.data.frame() # receiving but NA CE , 33 countries/territories

# save the CI for the 84 countries
df.NA.CE <- df.NA %>%
  filter(legend.ctry == "CE_USD") %>%
  left_join(coastal.ctr %>% dplyr::select(alpha.3,region),by=c("iso_a3"="alpha.3"))
write.csv(df.NA.CE,here("outputs","countries_84.csv"))

# transform into sf
df.NA.sf <-projected.world %>%
  left_join(df.NA, by="iso_a3") %>%
  filter(legend.ctry %in% c("DAC_donors_only","CE_NO_USD","TRUE_CE_NA_USD")) %>%
  mutate(legend.ctry = as.factor(legend.ctry))
saveRDS(df.NA.sf,here::here("data","derived-data","countries_84_sf.rds"))

## add to the full sf
rm(oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA)
oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA <- oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf %>%
  left_join(df.NA %>% dplyr::select(-cum.usd.comm.ctry,-iso_a2,-name_en), by=c("iso_a3")) %>%
  mutate(legend.ctry = as.factor(legend.ctry))
summary(oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA)
#### END FIGURE 3A

#### PREP FIGURE 3B
# add add color columns for the trend in investment
oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA <- oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA %>%
  mutate(significant.50 = as.factor(significant.50),
         significant.90 = as.factor(significant.90)) %>%
  mutate(color.vect.50 = ifelse(trend > 0 & significant.50 == "S","pos",
                                ifelse(trend < 0 & significant.50 == "S","neg",
                                       ifelse(trend > 0 & significant.50 == "NS","NS.pos",
                                              ifelse(trend < 0 & significant.50 =="NS","NS.neg",NA))))) %>%
  mutate(color.vect.90 = ifelse(trend > 0 & significant.90 == "S","pos",
                                ifelse(trend < 0 & significant.90 == "S","neg",
                                       ifelse(trend > 0 & significant.90 == "NS","NS.pos",
                                              ifelse(trend < 0 & significant.90 =="NS","NS.neg",NA)))))
# remove NAs from trend color
oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.50 <- addNA(oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.50)  
oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.90 <- addNA(oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.90)  

# change order levels
levels(oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.50) <- c("Negative","Negative.uncertain","Positive.uncertain","Positive","NA")
levels(oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.90) <- c("Negative","Negative.uncertain","Positive.uncertain","Positive","NA")
oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.50 <- factor(oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.50 ,levels=c("Negative","Negative.uncertain","Positive.uncertain","Positive","NA"))
oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.90 <- factor(oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.90 ,levels=c("Negative","Negative.uncertain","Positive.uncertain","Positive","NA"))

# reduce number of factors
oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.50.V2 <- fct_collapse(oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.50, Uncertain = c("Negative.uncertain", "Positive.uncertain"))
oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.90.V2 <- fct_collapse(oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA$color.vect.90, Uncertain = c("Negative.uncertain", "Positive.uncertain"))

# remove NA in contextual equity
rm(cont.ineq.ctry.sf.nonNA)
cont.ineq.ctry.sf.nonNA <- oecd.dat.sf.ctry.equity.commit.trend.percap.total.sf.NA %>%
  drop_na(cont.eq.score.mean.rank) %>%
  drop_na(usd.comm.year.CADC.total) %>%
  dplyr::select(-cadc.type,-tot.project.eq,-perc.equi,
                -usd.comm.ctry.cadc.inhab,-usd.disb.ctry.cadc.inhab,
                -usd.comm.ctry.cadc,-usd.disb.ctry.cadc) %>%
  distinct()
dim(cont.ineq.ctry.sf.nonNA) # 84 43 sanity check
#### end trend information

# threshold values for both investment and CI (median values)
med.CE <- summary(cont.ineq.ctry.sf.nonNA$cont.eq.score.mean.rank)[3] ; med.CE# 0.67
med.dollars <- summary(cont.ineq.ctry.sf.nonNA$usd.comm.year.CADC.total)[3] ; med.dollars# 168.065 

# ## add total number of projects by countries (cadc projects)
# cont.ineq.ctry.sf.nonNA <- cont.ineq.ctry.sf.nonNA %>%
#   left_join(oecd.dat.sf.ctry.equity.commit %>% dplyr::select(iso_a3,tot.project.CADC) %>% distinct(), by="iso_a3")
# summary(cont.ineq.ctry.sf.nonNA$cont.eq.score.Gini.rank)

### PREP FIGURE 3A
# on projected EEZ
eez.world <- vect(here("data","raw-data","World_EEZ_v11_20191118","eez_v11.shp"))
#plot(eez.world)
crs.val.robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # The World Robinson projection (ESRI 54030)
# 
projected.eez.world <- project(eez.world, crs.val.robinson)
projected.eez.world.sf <- st_as_sf(projected.eez.world)

# cont to df to remove country geometry and add ZEE geometry
cont.ineq.ctry.sf.df <- cont.ineq.ctry.sf%>% st_drop_geometry()
# add data
eez.world.cont.eq <-terra::merge(eez.world,cont.ineq.ctry.sf.df,by.x="ISO_SOV1",by.y="iso_a3")

# projected - contextual inequity
projected.eez.world.cont.eq.sf <-project(eez.world.cont.eq, crs.val.robinson)
projected.eez.world.cont.eq.sf <- st_as_sf(projected.eez.world.cont.eq.sf)

# filter removing NAs for both CE and USD = 84 countries
projected.eez.world.cont.eq.sf.NA <- projected.eez.world.cont.eq.sf %>%
  drop_na(cont.eq.score.mean.rank) %>%
  drop_na(usd.comm.year.CADC.total) %>%
  distinct()

# 
df.NA.sf.eez <- terra::merge(projected.eez.world.sf,df.NA.sf %>% st_drop_geometry(), by.x="ISO_SOV1",by.y="iso_a3")
projected.df.NA.sf.eez <- st_transform(df.NA.sf.eez, crs = crs.val.robinson)
projected.df.NA.sf.eez <- st_as_sf(projected.df.NA.sf.eez)

# keep NAs
ctr_NA_2keep <- df.NA.sf %>% filter(legend.ctry == "CE_NO_USD") %>% dplyr::select(iso_a3) %>% as.data.frame()
projected.eez.world.cont.eq.sf.NA.keep <- projected.eez.world.cont.eq.sf %>%
  filter(ISO_SOV1 %in% (ctr_NA_2keep$iso_a3))

# donors
ctr_donors_2keep <- df.NA.sf %>% filter(legend.ctry == "DAC_donors_only") %>% dplyr::select(iso_a3) %>% as.data.frame()
projected.eez.world.cont.eq.sf.donors.keep <- projected.eez.world.cont.eq.sf %>%
  #filter(is.na(cont.eq.score.mean.rank)) %>%
  filter(ISO_SOV1 %in% (ctr_donors_2keep$iso_a3))

# END FIGURE 1A

#### Figure 1A ###########
### chloropeth map mean cont inequity and commited investment

# create classes
N <- 2
pal = "PinkGrn"

### Jenks
data.eq <- bi_class(projected.eez.world.cont.eq.sf.NA, x = usd.comm.year.CADC.total, y = cont.eq.score.mean.rank, style = "quantile", dim = N,dig_lab = 5) %>%
  distinct()
dim(data.eq)
break_vals.eq <- bi_class_breaks(projected.eez.world.cont.eq.sf.NA, style = "quantile",
                              x = usd.comm.year.CADC.total, y = cont.eq.score.mean.rank, dim = N, dig_lab = c(x = 4, y = 5),
                              split = TRUE)
rm(data.eq.main) ## add the bi class values for the 84 countries countries - sf file
data.eq.main <- cont.ineq.ctry.sf.nonNA %>%
  st_drop_geometry() %>%
  left_join(data.eq %>%
              dplyr::select(sov_a3,bi_class,-geometry,-name_en) %>%
              st_drop_geometry(),by=c("iso_a3" = "sov_a3")) %>%
  distinct() %>%
  dplyr::select(-geometry)

## add sf to data.eq.main
data.eq.main.sf <- projected.world %>%
  left_join(data.eq.main %>% dplyr::select(-name_en), by="iso_a3") %>%
  distinct() %>%
  filter(!is.na(cont.eq.score.mean.rank))
data.eq.main.sf %>% filter(name_en == "South Africa")  # main country
data.eq %>% filter(name == "South Africa") # eez
dim(data.eq.main.sf) # 84 210
### investment by region and quadrants
data.eq.df <- data.eq %>%
  st_drop_geometry()

# invest.quadrants <- data.eq.df %>%
#   dplyr::select(region_un, bi_class, usd.comm.year.CADC.total, name_en) %>%
#   distinct() %>%
#   group_by(region_un, bi_class) %>%
#   summarize(
#     tot.usd.com = sum(usd.comm.year.CADC.total),
#     n = n(),
#     .groups = "drop"  # Automatically ungroups after summarization
#   ) %>%
#   group_by(bi_class) %>%
#   mutate(
#     tot.usd.com.stakes = sum(tot.usd.com)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     conditions = case_when(
#       bi_class == "1-1" ~ "Low investment - Low inequity",
#       bi_class == "1-2" ~ "Low investment - High inequity",
#       bi_class == "2-1" ~ "High investment - Low inequity",
#       bi_class == "2-2" ~ "High investment - High inequity",
#       TRUE ~ "Unknown"  # Handles any unexpected values
#     )
#   ) %>%
#   as.data.frame() %>%
#   arrange(bi_class)
# sum(invest.quadrants$tot.usd.com)
# sum(invest.quadrants$n)
# write.table(invest.quadrants,here("outputs","invest.quadrants.csv"),row.names = F)
# 
# ### invest quadrants countries
# invest.quadrants.ctry <- data.eq.df %>%
#   dplyr::select(region_un, bi_class, usd.comm.year.CADC.total, name_en) %>%
#   distinct() %>%
#   group_by(region_un, name_en, bi_class) %>%
#   summarize(
#     tot.usd.com = sum(usd.comm.year.CADC.total),
#     n = n(),
#     .groups = "drop"  # Automatically ungroups after summarization
#   ) %>%
#   mutate(
#     conditions = case_when(
#       bi_class == "1-1" ~ "Low investment - Low inequity",
#       bi_class == "1-2" ~ "Low investment - High inequity",
#       bi_class == "2-1" ~ "High investment - Low inequity",
#       bi_class == "2-2" ~ "High investment - High inequity",
#       TRUE ~ "Unknown"  # Handles any unexpected values
#     )
#   ) %>%
#   as.data.frame() %>%
#   arrange(bi_class)
# 
# write.table(invest.quadrants.ctry,here("outputs","invest.quadrants.ctry.csv"),row.names = F)
# 
# # in latex
# knitr::kable(
#   invest.quadrants
# )
# # get aspect ratior:
# asp.ratio <- get_asp_ratio( projected.world)
# 
# # map 4 quadrants
# map <- ggplot(data =  projected.world) +
#   geom_sf(color="black",fill = NA) + # empty country
#   geom_sf(data=projected.eez.world.sf,color="black",fill = "white") + # empyy eez
#   geom_sf(data=projected.eez.world.cont.eq.sf.NA,fill="lightgrey",color = "black")  + # EEZ 84 countries
#   geom_sf(data=df.NA.sf %>% filter(legend.ctry == "TRUE_CE_NA_USD"),fill="ivory1",color = "black") + # mainland
#   geom_sf(data=  projected.df.NA.sf.eez %>% filter(legend.ctry == "TRUE_CE_NA_USD"),fill="ivory1",color = "black") + # eez
#   geom_sf(data = data.eq, mapping = aes(fill = bi_class), color = "black", size = 0.1, show.legend = FALSE,alpha = 0.4) + # eez
#   geom_sf(data = data.eq.main.sf, mapping = aes(fill = bi_class), color = "black", size = 0.1, show.legend = FALSE) + # mainland
#   bi_scale_fill(pal = pal, dim = N) +
#   theme(plot.margin = margin(-2, 0, -2, 0, "cm"),
#         panel.border = element_blank(),
#         panel.spacing = unit(0, "lines"),
#         plot.background = element_blank(),
#         panel.background = element_blank())+
#   bi_theme()  +
#   coord_sf(expand = FALSE)   # This ensures the plot extends to the edge of the panel
# 
# # legend
# legend <- bi_legend(pal = pal,
#                     dim = N,
#                     xlab = "Total invest. (million USD) ",
#                     ylab = "Avg. contextual inequity ",
#                     size = 8,
#                     breaks = break_vals.eq) +
#   theme(
#                       plot.margin = margin(0, 0, 0, 0),
#                       legend.box.margin = margin(0, 0, 0, 0),
#                       legend.margin = margin(0, 0, 0, 0)
#                     )+ my_theme
# 
# height=7
# finalPlot.cont.ineq <- ggdraw() +
#   draw_plot(map, 0, 0, 1, 1) +
#   draw_plot(legend, x = -0.075, y = -0.015, width = 0.35, height = 0.35)
# ggsave(here("figures","Figure3A_chloropeth_quartile_test.pdf"),finalPlot.cont.ineq,width=height*asp.ratio,height=height,dpi=300, bg = "transparent")
# 
# #ggsave(here("figures","Figure1A_chloropeth_quartile_4colors_EEZ_RANK.pdf"),finalPlot.cont.ineq,width=height*asp.ratio,height=height,dpi=300, bg = "transparent")
# 
# ### END FIGURE 1A
# 
# ### FIGURE 1B
# # retrieve color names
# g <- ggplot_build(map)
# 
# colors.legend <- cbind(g$data[[6]]["fill"],g$data[[6]]["group"],data.eq$ISO_SOV1,data.eq$iso_n3) %>%
#   distinct() %>%
#   arrange(group) %>%
#   as.data.frame()
# names(colors.legend)[3] <- "ISO_SOV1"
# names(colors.legend)[4] <- "iso_n3"
# 
# # CADC invest - contextual ineq
# equ.Q.top25 <- quantile(projected.eez.world.cont.eq.sf.NA$cont.eq.score.mean.rank)[4] # 0.79
# committed.Q.top25 <- quantile(projected.eez.world.cont.eq.sf.NA$usd.comm.year.CADC.total)[4] # 322.0225 
# equ.Q.top50 <- quantile(projected.eez.world.cont.eq.sf.NA$cont.eq.score.mean.rank)[3] # 0.66
# committed.Q.top50 <- quantile(projected.eez.world.cont.eq.sf.NA$usd.comm.year.CADC.total)[3] # 179.38 
# 
# # top left
# TOP_LEFT <- cont.ineq.ctry.sf.nonNA %>% filter(usd.comm.year.CADC.total <med.dollars & cont.eq.score.mean.rank >=med.CE)
# TOP_LEFT$name_en
# 
# # top right
# TOP_RIGHT <- cont.ineq.ctry.sf.nonNA %>% filter(usd.comm.year.CADC.total >med.dollars & cont.eq.score.mean.rank >=med.CE)
# TOP_RIGHT$name_en
# 
# # CI 90%
# # plot quartiles for equity
# equ.Q.top25 <- quantile(projected.eez.world.cont.eq.sf.NA$cont.eq.score.mean.rank)[4]
# committed.Q.top25 <- quantile(projected.eez.world.cont.eq.sf.NA$usd.comm.year.CADC.total)[4]
# 
# equ.Q.top50 <- quantile(projected.eez.world.cont.eq.sf.NA$cont.eq.score.mean.rank)[3]
# committed.Q.top50 <- quantile(projected.eez.world.cont.eq.sf.NA$usd.comm.year.CADC.total)[3]
# 
# # countries top 25%
# top25_perc = projected.eez.world.cont.eq.sf.NA %>% filter(usd.comm.year.CADC.total >= committed.Q.top25 & cont.eq.score.mean.rank >= equ.Q.top25)
# 
# # Figure 1B
# sc.risk.CADC.dollars.90 <- ggplot(cont.ineq.ctry.sf.nonNA %>% distinct(),aes(x=usd.comm.year.CADC.total,y=cont.eq.score.mean.rank,label=name_en,color=color.vect.90.V2,size=abs(trend))) +
#   annotate("rect", xmin = 0, xmax = committed.Q.top50, ymin = equ.Q.top50, ymax = 1, fill= "#bc177d", alpha = 0.3) + # top left
#   annotate("rect", xmin = 0, xmax = committed.Q.top50, ymin = 0, ymax = equ.Q.top50, fill= "#d3d3d3", alpha = 0.3) + # bottom left
#   annotate("rect", xmin = committed.Q.top50, xmax = 2250, ymin = 0, ymax = equ.Q.top50, fill= "#459b22", alpha = 0.3) + # bottom right
#   annotate("rect", xmin = committed.Q.top50, xmax = 2250, ymin = equ.Q.top50, ymax =1, fill= "#3e1114", alpha = 0.3) + # top right
#   geom_point()+
#   geom_text_repel( size=4,force=4,max.overlaps=10,show_legend=F,force_pull=2) +
#   geom_point(data=top25_perc,aes(x=usd.comm.year.CADC.total,y=cont.eq.score.mean.rank,label=name_en),color="white",size=1)+
#   xlab("Total investment (Million USD)") +
#   ylab("Average contextual inequity") +
#   ylim(0,1) + #   xlim(0,1)  + 
#   scale_color_manual(values = c("firebrick4","grey10","blue4","lightgrey")) +
#   labs(color = "Investment direction",size="Investment level (Million USD/year)") +
#   theme(legend.position = c(0.45, 0.15),legend.direction="horizontal",
#         panel.background = element_rect(fill = "white",
#                                         colour = "grey",
#                                         size = 0.5, linetype = "solid"),
#         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                         colour = "lightgrey"), 
#         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                         colour = "lightgrey"),
#         legend.box.background = element_rect(colour = "black"))+ my_theme +
#       guides(color = guide_legend(nrow = 2),size = guide_legend(nrow = 2),legend.text=element_text(size = 14),
#              plot.margin = unit(c(1, 2, 1, 2), "cm"))
# 
# sc.risk.CADC.dollars.90
# 
# ### save Figure 1B
# height_2=7
# ggsave(here("figures","Figure1B_trend_90CI.simple_RANK_updates.pdf"),sc.risk.CADC.dollars.90,width=10,height=10,dpi=300,
#        device = cairo_pdf)
# ggsave(here("figures","Figure1B_trend_90CI.simple_RANK_updates.png"),sc.risk.CADC.dollars.90,width=10,height=10,dpi=300,
#        device = cairo_pdf)
# 
# #### END FIGURE 1B OK
# 
# # START SUPP FIG S9: with project count
#   # add colors by country
# cont.ineq.ctry.sf.nonNA <- cont.ineq.ctry.sf.nonNA %>%
#   left_join(colors.legend, by=c("iso_a3" = "ISO_SOV1"))
# 
# # median
# equ.Q.top50 <- quantile(projected.eez.world.cont.eq.sf.NA$cont.eq.score.mean.rank)[3]
# count.Q.top50 <- quantile(cont.ineq.ctry.sf.nonNA$tot.project.CADC)[3]
# 
# sc.risk.CADC.count <- ggplot(cont.ineq.ctry.sf.nonNA %>% distinct(),aes(x=tot.project.CADC,y=cont.eq.score.mean.rank,label=name_en,color=fill)) +
#   geom_point(size=4) +
#   scale_color_manual(values = c("#3e1114","#459b22","#bc177d","#d3d3d3"),
#                      labels = c("High $/High CI","High $/Low CI","Low $/High CI","Low $/Low CI"),
#                      name="") +
#   geom_text_repel( size=4,force=4,max.overlaps=10,show_legend=F,force_pull=2) +
#   geom_vline(xintercept = count.Q.top50,color="red",linetype="dashed") +
#   geom_hline(yintercept=equ.Q.top50, linetype="dashed", color = "red")+
#   xlab("Total number of projects") +
#   ylab("Average contextual inequity by country") +
#   ylim(0,1) + #   xlim(0,1)  + 
#   theme(legend.position = c(0.45, 0.15),legend.direction="horizontal",
#         panel.background = element_rect(fill = "white",
#                                         colour = "grey",
#                                         size = 0.5, linetype = "solid"),
#         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                         colour = "lightgrey"), 
#         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                         colour = "lightgrey"),
#         legend.box.background = element_rect(colour = "black"),
#         legend.text = element_text(size=5),
#         guides(color=guide_legend(nrow=2, byrow=TRUE)))+ my_theme #+
#   #guides(fill="none", color="none")
# 
# sc.risk.CADC.count
# ggsave(here("figures","SUPP_Figure9_project_counts.png"),sc.risk.CADC.count,width=9,height=9,dpi=300)
# ggsave(here("figures","SUPP_Figure9_project_counts.pdf"),sc.risk.CADC.count,width=9,height=9,dpi=300)
# 
# ### END SUPP FIG S9
# 
# #### Statistics of Figure 1A and 1B
# # number of countries above both 25%
# test0 <- cont.ineq.ctry.sf.nonNA %>% filter(usd.comm.year.CADC.total >= committed.Q.top25 & cont.eq.score.mean.rank >= equ.Q.top25)
# # number of countries above both 50%
# cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total >= committed.Q.top50 & cont.eq.score.mean.rank >= equ.Q.top50) %>% dim()
# test_2 <- cont.ineq.ctry.sf.nonNA %>% filter(usd.comm.year.CADC.total >= committed.Q.top50 & cont.eq.score.mean.rank >= equ.Q.top50)
# sum(test_2$usd.comm.year.CADC.total)
# 
# # number of countries top left
# cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total < committed.Q.top50 & cont.eq.score.mean.rank >= equ.Q.top50) %>% dim()
# test_3 <-cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total < committed.Q.top50 & cont.eq.score.mean.rank >= equ.Q.top50)
# sum(test_3$usd.comm.year.CADC.total)
# 
# # number of countries bottom left
# cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total < committed.Q.top50 & cont.eq.score.mean.rank < equ.Q.top50) %>% dim()
# test_4 <-cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total < committed.Q.top50 & cont.eq.score.mean.rank < equ.Q.top50)
# sum(test_4$usd.comm.year.CADC.total)
# 
# # number of countries bottom right
# cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total >= committed.Q.top50 & cont.eq.score.mean.rank < equ.Q.top50) %>% dim()
# test_5 <-cont.ineq.ctry.sf.nonNA %>% dplyr::filter(usd.comm.year.CADC.total >= committed.Q.top50 & cont.eq.score.mean.rank < equ.Q.top50)
# sum(test_5$usd.comm.year.CADC.total)
# 
# # total
# tot <- sum(test_2$usd.comm.year.CADC.total)+sum(test_3$usd.comm.year.CADC.total)+
#   sum(test_4$usd.comm.year.CADC.total)+sum(test_5$usd.comm.year.CADC.total)
# round(sum(test_2$usd.comm.year.CADC.total)/tot,2)*100
# round(sum(test_3$usd.comm.year.CADC.total)/tot,2)*100
# round(sum(test_4$usd.comm.year.CADC.total)/tot,2)*100
# round(sum(test_5$usd.comm.year.CADC.total)/tot,2)*100
# 
# # PREP FIGURE 3C  equity
#   # join contextual vulnerability
# oecd.dat.sf.ctry.equity.commit.vulnerab <- oecd.dat.sf.ctry.equity.commit %>%
#   left_join(data.eq %>% st_drop_geometry() %>% dplyr::select(ISO_SOV1,cont.eq.score.mean.rank,cont.eq.score.Gini.rank,bi_class),by=c("iso_a3"='ISO_SOV1'))
# 
# oecd.dat.sf.ctry.equity.commit.vulnerab <- oecd.dat.sf.ctry.equity.commit.vulnerab %>%
#   left_join(colors.legend,by=c("iso_a3" = "ISO_SOV1")) %>%
#   filter(!is.na(cont.eq.score.mean.rank)) %>%
#   filter(cadc.type=="equity CADC") %>%
#   droplevels()
# oecd.dat.sf.ctry.equity.commit.vulnerab$fill
# 
# oecd.dat.sf.ctry.equity.commit.vulnerab$colorname <-  l_colorName(as.character(oecd.dat.sf.ctry.equity.commit.vulnerab$fill))
# 
# # create color scale
# color.biclass <- oecd.dat.sf.ctry.equity.commit.vulnerab %>%
#   dplyr::select(bi_class,fill) %>%
#   distinct() %>%
#   mutate(bi_class = as.factor(bi_class))
# 
# eq.df <- oecd.dat.sf.ctry.equity.commit.vulnerab %>% filter(cadc.type=="equity CADC") %>% distinct()
# summary(eq.df$perc.equi)
# 
# # END PREP FIGURE 3C
# 
# #### Figure 3C - V1
# CADC.risk.eq.dollars <- ggplot(oecd.dat.sf.ctry.equity.commit.vulnerab %>% filter(cadc.type=="equity CADC") %>% distinct(),aes(x=perc.equi,y=cont.eq.score.mean.rank,label=name_en)) +
#   geom_point(aes(colour = fill),size=5) +
#   scale_color_identity() +
#   xlab("Equity (% total of CADC projects)") +
#   geom_text_repel(force_pull=2) +
#   ylab("Average contextual inequity by country") +
#   xlim(0,100) +  ylim(0,1) +
#   geom_vline(xintercept=50,linetype = 2)+
#   geom_hline(yintercept=0.66,linetype = 2) +
#   scale_x_break(c(60,101),space=0.05, ticklabels = c(60,100)) +
#   theme_bw()
# 
# ggsave(here("figures","Figure1C_quantile_updates_V1.pdf"),CADC.risk.eq.dollars,width=10,height=10,dpi=300)
# ggsave(here("figures","Figure1C_quantile_updates_V1.png"),CADC.risk.eq.dollars,width=10,height=10,dpi=300)
# 
# ### END FIGURE 3C
# 
# ### START FigS8
# # average by group of countries
# equity.cont.average.ctry.group <- oecd.dat.sf.ctry.equity.commit.vulnerab %>% 
#   filter(cadc.type=="equity CADC") %>%
#   group_by(fill) %>%
#   summarize(mean.cont.eq = mean(cont.eq.score.mean.rank),sd.cont.eq = sd(cont.eq.score.mean.rank),
#             mean.equity.perc = mean(perc.equi),sd.equity.perc = sd(perc.equi),
#             sum.invest = sum(usd.comm.year)) %>%
#   mutate(fill = as.factor(fill))
# sum(equity.cont.average.ctry.group$sum.invest)
# 
#   # anova 
# rm(eq.data)
# eq.data <- oecd.dat.sf.ctry.equity.commit.vulnerab %>% filter(cadc.type=="equity CADC") %>% distinct() %>%
#   mutate(name_quadrats = case_when(
#     fill == "#d3d3d3" ~ "Low $/Low CI",
#     fill == "#3e1114" ~ "High $/High CI",
#     fill == "#459b22" ~ "High $/Low CI",
#     fill == "#bc177d" ~ "Low $/High CI")) %>%
#   mutate(name_quadrats = as.factor(name_quadrats)) %>%
#   mutate(fill.col = fill)
# 
# # reoarder levels
# eq.data$name_quadrats <- fct_relevel(eq.data$name_quadrats, "Low $/Low CI", "High $/Low CI", "Low $/High CI","High $/High CI")
# 
# anova.perc.equ <- aov( perc.equi ~ fill.col,data=eq.data)
# 
# my_comparisons <- list( c("#d3d3d3", "#bc177d"), c("#d3d3d3", "#459b22"), c("#d3d3d3", "#3e1114"),
#                         c("#bc177d", "#459b22"), c("#bc177d", "#3e1114"),
#                         c("#459b22", "#3e1114"))
# 
# tukey <- TukeyHSD(anova.perc.equ)
# print(tukey)
# cld <- multcompLetters4(anova.perc.equ, tukey)
# print(cld)
# # extracting the compact letter display and adding to the Tk table
# cld <- as.data.frame.list(cld$fill.col)
# cld <- tibble::rownames_to_column(cld,"fill.col")
# 
# # join the name of the quadrants
# cld <- cld %>%
#   left_join(eq.data %>% dplyr::select(fill.col,name_quadrats) %>% distinct(),by="fill.col")
# 
# eq.boxplot <- ggplot(eq.data, aes(x = name_quadrats, y = perc.equi, fill = fill.col)) +
#   ggdist::stat_halfeye(
#     adjust = .5,
#     width = .6,
#     .width = 0,
#     justification = -.2,
#     point_colour = NA
#   ) +
#   geom_text(data = cld, aes(name_quadrats, y = 55, label = Letters), size = 4, vjust=-1, hjust =1)+
#   scale_fill_manual(values = setNames(unique(eq.data$fill.col), unique(eq.data$fill.col)),
#                    guide="none") +
#   geom_boxplot(
#     width = .15,
#     outlier.shape = NA
#   ) +
#   geom_point(
#     shape = 95, # draw horizontal lines instead of points
#     size = 15,
#     alpha = .2
#   ) +
#   coord_cartesian(xlim = c(1.2, 4), clip = "off") +
#   ylab("Equity (% total of CADC projects)") +
#   xlab("Quadrants")+
#   coord_flip() +
#   theme_bw()+ my_theme
# eq.boxplot
# ggsave(here("figures","FigS8_quantile_distribution.pdf"),eq.boxplot,width=7,height=7)
# ggsave(here("figures","FigS8_quantile_distribution.png"),eq.boxplot,width=7,height=7)
# 
# res <- cor.test(eq.data$perc.equi, eq.data$cont.eq.score.mean.rank, 
#                 method = "spearman")
# res
# 
# ### END FigS8
# 
# #### START Figure 1C - V2
# rm(CADC.risk.eq.dollars.2)
# data.risk.eq <- oecd.dat.sf.ctry.equity.commit.vulnerab %>% filter(cadc.type=="equity CADC") %>% distinct()
# CADC.risk.eq.dollars.2.test <- ggplot(data=data.risk.eq,aes(x=perc.equi,y=cont.eq.score.mean.rank,label=name_en)) +
#   geom_point(aes(colour = fill),size=5) +
#   scale_color_identity() +
#   xlab("Equity (% total of CADC projects)") +
#   geom_text_repel(force_pull=2) +
#   ylab("") +
#   xlim(0,55) +  ylim(0,1) +
#   geom_vline(xintercept=50,linetype = 2)+
#   geom_hline(yintercept=0.65,linetype = 2) +
#   #scale_x_break(c(50,101),space=0.05, ticklabels = c(50,100)) +
#   theme_bw()+
#   theme(axis.text.x.top = element_blank(),
#         axis.ticks.x.top = element_blank(),
#         axis.line.x.top = element_blank(),
#         axis.title.y = element_blank(),
#         plot.margin = unit(c(1, 2, 1, 2), "cm"))+ 
#   my_theme
# CADC.risk.eq.dollars.2.test
# 
# ### END FIGURE 3C
# 
# #### START COMBINE PLOTS
# combined_plot <- finalPlot.cont.ineq / 
#   (sc.risk.CADC.dollars.90 + CADC.risk.eq.dollars.2.test) +
#   plot_layout(nrow = 2, heights = c(1, 1)) +
#   patchwork::plot_annotation(tag_levels = 'A')
# 
# ggsave(here("figures","CADC.risk.equity.ABC_2.quartiles.pdf"),combined_plot,width=18.4,height=18.4,dpi = 300)
# ggsave(here("figures","CADC.risk.equity.ABC_2.quartiles.tiff"),combined_plot,width=18.4,height=18.4,dpi = 300)
# 
# combined_plot_2 <- ggarrange(finalPlot.cont.ineq, sc.risk.CADC.dollars.90, CADC.risk.eq.dollars.2.test, ncol=1,nrow=3,
#                              labels=c("A","B","C"))
# 
# ggsave(here("figures","CADC.risk.equity.ABC_2_V2.quartiles.pdf"),combined_plot_2,width=18.4,height=30,units="cm",dpi = 300)
# ggsave(here("figures","CADC.risk.equity.ABC_2_V2.quartiles.tiff"),combined_plot_2,width=18.4,height=30,units="cm",dpi = 300)
# 
# ### END COMBINED PLOT
# 
# #### START SUPP FIG S7
# #### Per capita Supplemental 
# # estimated coastal pop
# rm(oecd.dat.invest.by.country)
# oecd.dat.invest.by.country <- read.csv(here("outputs","investment_ctry_per_capita.csv"))
# 
# ## add coastal population and estimate investment by inhabitants
# oecd.dat.invest.by.country <- left_join(cont.ineq.ctry.sf.nonNA,oecd.dat.invest.by.country,by="iso_a3")
# 
# data.invest.per.capita <- oecd.dat.invest.by.country %>% dplyr::select(name_en.y,tot.usd.comm.ctry,tot.usd.comm.ctry.inhab,fill) %>% distinct()
# 
# cor.totalinvest.per.capita <- ggplot(data.invest.per.capita, aes(x = tot.usd.comm.ctry, y = tot.usd.comm.ctry.inhab, label = name_en.y, color = fill)) +
#   geom_point(size=4) +
#   #scale_y_break(c(4000, 187301), ticklabels = c(4000, 187300), space = 0.01, scale = 0.15) +
#   geom_text_repel(force_pull = 2) +
#   scale_color_manual(values = setNames(unique(data.invest.per.capita$fill), unique(data.invest.per.capita$fill)),
#                       labels = c("High $/High CI","High $/Low CI","Low $/High CI","Low $/Low CI"),name="") +
#   xlab("Total committed investment ($)") +
#   ylab("Total committed investment per capita ($)") +
#   #stat_cor(method = "spearman", label.x = 1500, label.y = 3000) +
#   #stat_cor(method = "pearson", label.x = 1500, label.y = 2500) +
#   theme_bw()  +
#   theme(
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14),
#     legend.position = c(0.45, 0.15),legend.direction="horizontal",
#     panel.background = element_rect(fill = "white",
#                                     colour = "grey",
#                                     size = 0.5, linetype = "solid"),
#     panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                     colour = "lightgrey"), 
#     panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                     colour = "lightgrey"),
#     legend.box.background = element_rect(colour = "black"))+
#   my_theme
# cor.totalinvest.per.capita
# ggsave(here("figures","SUPP_cor.totalinvest.per.capita.png"),cor.totalinvest.per.capita,width=8,height=7)
# ggsave(here("figures","SUPP_cor.totalinvest.per.capita.pdf"),cor.totalinvest.per.capita,width=8,height=7)
# 
# cor(oecd.dat.invest.by.country$tot.usd.comm.ctry,oecd.dat.invest.by.country$tot.usd.comm.ctry.inhab,method="spearman",use="pairwise.complete.obs")
# 
# #### END SUPP FIG S7
# 
# 
# 
# 
# 
