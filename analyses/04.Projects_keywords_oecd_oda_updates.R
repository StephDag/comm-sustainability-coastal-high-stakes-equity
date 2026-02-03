# #***********************************************************************
# # Categorized projects based based on coastal, sustainability (CADC) and sustainable land-based projects
# # --------------------------------------------------------------
# # INPUTS:
# # - OECD projects database
# # 
# # OUTPUTS
# # - Dataset identifying CADC projects and equity mentioning in title and description of the projects
# #***********************************************************************
# # Creation : David Gill & Stéphanie D'Agata
# # Email :david.gill@duke.edu ; stephanie.dagata@ird.fr
# # ORCID : https://orcid.org/0000-0001-6941-8489
# # Institution : Institut de Recherche pour le Développement
# #***********************************************************************
# 
library(here)
source(here("analyses","00_setup.R"))

# ############################
# #   COASTAL COUNTRIES      #
# ############################
# 
# load dataset with potential projects in coastal countries
oecd.dat.orig <- import(here("data","raw-data","OECD","ocean_ODA_2010_2021.csv"),sep=";",dec=",")
head(oecd.dat.orig)
dim(oecd.dat.orig)
# 
# ################################################
# #           identify key words                #
# ################################################
# 
# # ----- import keywords ------
# # Create function to read in keyword lists from Excel sheet Chechi prepared (keywords list.xlsx)
# # Level of confidence: 1: drop/doesn't work; 2: needs additional term; 3: works well; 4: new term to validate; 5: preexisting term not validated yet
my_get_keyword <- function(x) {
  # import dataframe
  keyword.list.all <- import("data/raw-data/keywords list.xlsx", sheet=x) %>%
    mutate(keyword=str_trim(tolower(keyword)), # remove leading and trailing white space, convert to lowercase
           keyword=gsub("\\//","\\\\",keyword)) %>%  # have no idea why we have to do this, using "\\b" in Excel creating issues so reversed it in spreadsheet
    distinct(keyword,.keep_all = T)

  # keyword list
  keyword.list <- keyword.list.all %>%
    filter(level_of_confidence!=1) %>% # remove low confidence keywords (see definitions tab in excel sheet for details)
    mutate(keyword=str_c("\\b",keyword)) %>% # paste break term in front of each term
    pull(keyword)
  assign(paste0(tolower(x), ".list"), keyword.list.all, envir = parent.frame()) #assign variable name

  # low confidence list (those that require 2 marine terms)
  lowconfid.list <- keyword.list.all$keyword[keyword.list.all$level_of_confidence%in%c(2)]
  assign(paste0(tolower(x), ".low.confid"), lowconfid.list, envir = parent.frame())

  # to check list (new or older terms that we havent' checked yet. These might be ok)
  check.confid.list <- keyword.list.all$keyword[keyword.list.all$level_of_confidence%in%c(4,5)]
  assign(paste0(tolower(x), ".check.confid"), check.confid.list, envir = parent.frame())
  return(keyword.list)
}

# # Extract terms from each tab in Excel file
key.marine <- my_get_keyword("Marine")
key.marine <- key.marine[!is.na(key.marine)] # rm NAs
key.sustainability <- my_get_keyword("Sustainability")
key.sustainability <- key.sustainability[!is.na(key.sustainability)] # rm NAs
key.land <- my_get_keyword("Land")
key.land <- key.land[!is.na(key.land)] # rm NAs
key.equity <- my_get_keyword("Equity")
key.equity <- key.equity[!is.na(key.equity)] # rm NAs

# # Get terms associated with the different equity dimensions (experiemental)
recogn.list <- str_c("\\b",equity.list$keyword[!is.na(equity.list$Recognition)])
proc.list <- str_c("\\b",equity.list$keyword[!is.na(equity.list$Procedural)])
distrib.list <- str_c("\\b",equity.list$keyword[!is.na(equity.list$Distributional)])

# # terms to identify irrelevant projects
marine.bad.list <- str_c("\\b",c("river","rivers","lake","lakes")) # OECD methods: projects to remove from dataset (not currently implemented)
equity.bad.list <- str_c("\\b",equity.list$keyword[!is.na(equity.list$remove)]) # terms to remove from text before searching for equity terms

# #----- Identify terms in project title and description ----
# # alternative way to find matching project along with terms
oecd.dat.to.search <- oecd.dat.orig %>%
  mutate(id=row_number(), # add unique identifier
         comb = str_to_lower(paste(project,long_description,purpose,sector_name)),  # combine fields to search
         #comb = gsub(paste0(marine.bad.list, collapse="|"),"",comb),  # remove unwanted words
         comb.equity = gsub(paste0(equity.bad.list, collapse="|"),"",comb))  %>% # create new variable without terms that return incorrect hits
         dplyr::select(id,everything()) # change column order
names(oecd.dat.to.search)

# # take a subsample for keyword validation (if needed)
 oecd.sample <- slice_sample(oecd.dat.to.search,prop=0.02)

# # identify matching terms (15-20 min)
start.time <- Sys.time()
oecd.dat2 <-  oecd.dat.to.search %>%
    rowwise() %>%
    mutate(marine.term= paste(unique(str_extract_all(comb,paste0(key.marine, collapse="|"))[[1]]),collapse="; "), # extract all unique matching terms
           n.marine.term=str_count(marine.term, '\\w+'), # count # different terms identified
           sustain.term= paste(unique(str_extract_all(comb,paste0(key.sustainability, collapse="|"))[[1]]),collapse="; "),
           n.sustain.term=str_count(sustain.term, '\\w+'),
           land.term= paste(unique(str_extract_all(comb,paste0(key.land, collapse="|"))[[1]]),collapse="; "),
           n.land.term=str_count(land.term, '\\w+'),
           equity.term= paste(unique(str_extract_all(comb.equity,paste0(key.equity, collapse="|"))[[1]]),collapse="; "),
           n.equity.term=str_count(equity.term, '\\w+'))

# identify high/low confidence projects
oecd.dat3 <- oecd.dat2 %>%
  mutate(
    marine.confid = case_when(
      n.marine.term == 0  ~ "NA",
      n.marine.term == 1 & grepl(paste0(marine.check.confid, collapse="|"),marine.term) ~ "check", # terms that were not validated yet
      n.marine.term == 1 & grepl(paste0(marine.low.confid, collapse="|"),marine.term) ~ "low", # terms that require 2 terms
      TRUE ~ "high"), # high confidence = matches 1 high confidence term or >1 term
    sustain.confid = case_when(
      n.sustain.term == 0  ~ "NA",
      n.sustain.term == 1 & grepl(paste0(sustainability.check.confid, collapse="|"),sustain.term) ~ "check",
      n.sustain.term == 1 & grepl(paste0(sustainability.low.confid, collapse="|"),sustain.term) ~ "low",
      TRUE ~ "high"), # high confidence = matches 1 high confidence term or >1 term
    land.confid = case_when(
      n.land.term == 0  ~ "NA",
      n.land.term == 1 & grepl(paste0(land.check.confid, collapse="|"),land.term) ~ "check",
      n.land.term == 1 & grepl(paste0(land.low.confid, collapse="|"),land.term) ~ "low",
      TRUE ~ "high"), # high confidence = matches 1 high confidence term or >1 term
    equity.confid = case_when(
      n.equity.term == 0  ~ "NA",
      n.equity.term == 1 & grepl(paste0(equity.check.confid, collapse="|"),equity.term) ~ "check",
      n.equity.term == 1 & grepl(paste0(equity.low.confid, collapse="|"),equity.term) ~ "low",
      TRUE ~ "high"), # high confidence = matches 1 high confidence term or >1 term
    recognition = ifelse(grepl(paste0(recogn.list, collapse="|"),equity.term),1,0),
    procedural = ifelse(grepl(paste0(proc.list, collapse="|"),equity.term),1,0),
    distributional = ifelse(grepl(paste0(distrib.list, collapse="|"),equity.term),1,0)

  )

# Code projects based on matching terms: 1: high confidence, 2: low confidence
oecd.dat4 <-  oecd.dat3 %>%
  mutate(
    marine = case_when(
      n.marine.term == 0 ~ 0,
      marine.confid == "high" ~ 1, # high confidence = matches 1 high confidence term or >1 term
      marine.confid%in%c("low","check") ~ 2,
      TRUE ~ NA_integer_),
    sustainability = case_when(
      n.sustain.term == 0 ~ 0,
      sustain.confid == "high" ~ 1, # high confidence = matches 1 high confidence term or >1 term
      sustain.confid%in%c("low","check") ~ 2,
      TRUE ~ NA_integer_),
    land = case_when(
      n.land.term == 0 ~ 0,
      land.confid == "high" ~ 1, # high confidence = matches 1 high confidence term or >1 term
      land.confid%in%c("low","check") ~ 2,
      TRUE ~ NA_integer_),
    equity = case_when(
      gender_equality > 0 ~ 1, # trusting oecd coding
      n.equity.term == 0 ~ 0,
      equity.confid == "high" ~ 1, # high confidence = matches 1 high confidence term or >1 term
      equity.confid%in%c("low","check") ~ 2,
      TRUE ~ NA_integer_))

# recode the data to add indicator variables (to tweak)
oecd.dat.out <- oecd.dat4 %>%
  # CADC projects as identified by our keywords
  mutate(cadc.coded=case_when(
      (marine == 1 & sustainability == 1) | (marine == 0 & land == 1) ~ 1,
      (marine == 1 & sustainability == 2) | (marine == 2 & sustainability == 1) | (marine == 0 & land == 2) ~ 2,
      TRUE ~ 0),
  # CADC projects as identified by the OECD
  cadc=case_when(
      sustainable_ocean_ODA==TRUE|(landbased_ODA==TRUE&ocean_ODA==FALSE) ~ 1,
      TRUE ~ 0),
  # Classify project types
    cadc.type=case_when(
      equity==1 & cadc==1 ~ "equity CADC",
      cadc==1 ~ "other CADC",
      ocean_ODA==TRUE & cadc==0 ~ "other ocean economy",
      TRUE ~ NA_character_),
  # need to figure out if to use disbursement or committed
  cadc.comm.usd=ifelse(cadc==1,commitment_usd_million_defl,0),
  cadc.disb.usd=ifelse(cadc==1,disbursement_usd_million_defl,0)
  )
# total time
Sys.time()-start.time

# # check results
term.check <- oecd.dat.out %>%
  filter(sustainable_ocean_ODA == TRUE & equity >0 & !gender_equality%in%c(1,2))

equity.check <- oecd.dat.out %>%
  filter(!gender_equality%in%c(1,2))
sum(is.na(oecd.dat.out$n.marine.term))
table(oecd.dat.out$land.confid,oecd.dat.out$n.land.term)
table(oecd.dat.out$land,oecd.dat.out$n.land.term)

View(term.check)

# # --- Export data ----
export(term.check,"data/data_check/oecd_equity_keyword_check.csv")
export(oecd.dat.out,here("data","derived-data","ocean_ODA_2010_2021_equity.csv"))

# ##### plots and shapefiles
# 
# ############################
# #   plots and shapefiles   #
# ############################
# 
# # load coded OECD dataset with potential projects in coastal countries
oecd.dat.out <- import(here("data","derived-data","ocean_ODA_2010_2021_equity.csv"))
#names(oecd.dat.out)
summary(oecd.dat.out)

# ## map cumulative number of initiatives
world <- ne_countries(scale = "large", returnclass = "sf")
#class(world)
crs.val.robinson <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # The World Robinson projection (ESRI 54030)
projected.world <-  st_transform(world, crs.val.robinson)

# # rename countries to align with Rnaturalearth shapefile (using the admin variable in the world layer)
 missing.ctry <- oecd.dat.out %>%
  filter(!recipient%in%projected.world$admin & !grepl("regional",recipient)) %>%
  distinct(recipient)
sort(missing.ctry$recipient)
sort(unique(projected.world$subunit))
# grep("Mayotte",oecd.dat$recipient,value=T)
# grep("Mayotte",world$admin,value=T)

oecd <- oecd.dat.out %>%
  mutate(ctry=case_when(
      recipient=="China (People's Republic of)" ~ "China",
      recipient=="Congo" ~ "Democratic Republic of the Congo",
      recipient=="Côte d'Ivoire" ~ "Ivory Coast",
      recipient=="Democratic People's Republic of Korea" ~ "North Korea",
      recipient=="Eswatini" ~ "eSwatini",
      # recipient=="Mayotte" ~ "",
      recipient=="Micronesia" ~ "Federated States of Micronesia",
      recipient=="Sao Tome and Principe" ~ "São Tomé and Principe",
      # recipient=="States Ex-Yugoslavia unspecified" ~ "",
      recipient=="Syrian Arab Republic" ~ "Syria",
      recipient=="Tanzania" ~ "United Republic of Tanzania",
      recipient=="Timor-Leste" ~ "East Timor",
      # recipient=="Tokelau" ~ "",
      recipient=="Türkiye" ~ "Turkey",
      recipient=="Viet Nam" ~ "Vietnam",
      # recipient=="West Bank and Gaza Strip" ~ "",
      TRUE ~  recipient)
  )

# # check for remaining missing countries (should be the 4 commented out above + "Bilateral, unspecified" )
oecd %>%
  filter(!ctry%in%projected.world$admin & !grepl("regional",ctry)) %>%
  distinct(ctry) %>%
  pull

write.csv(oecd,here("data","derived-data","ocean_ODA_2010_2021_full_with_equity_61102.csv"))

# # oecd sf
rm(oecd.dat.out.sf) # 61102 (0 = 16608, 1=44494)
oecd.dat.out.sf <-left_join(oecd,projected.world,by=c("ctry"="admin"))

# check for remaining missing countries (should be the 4 commented out above + "Bilateral, unspecified" )
oecd.dat.out.sf %>%
  filter(!ctry%in%projected.world$admin & !grepl("regional",ctry)) %>%
  distinct(ctry) %>%
  pull

summary(as.factor(oecd.dat.out.sf$cadc))
summary(as.factor(oecd.dat.out.sf$region_un))

# remove regional investment without specific countries ==> n=50177 (0 = 14737; 1 = 35440)
oecd.dat.out.sf <- oecd.dat.out.sf %>%
 filter(!is.na(region_un),recipient != "Bilateral, unspecified")
dim(oecd.dat.out.sf)
oecd.dat.out.sf

summary(as.factor(oecd.dat.out.sf$cadc))
summary(as.factor(oecd.dat.out.sf$region_un))
summary(as.factor(oecd.dat.out.sf$ctry))
unique(oecd.dat.out.sf$ctry) %>% length() # 115 countries

saveRDS(oecd.dat.out.sf,here("data","derived-data","ocean_ODA_2010_2021_full_with_equity_50177.rds"))

# summarize by year, country, type of projects
oecd.dat.out.sf <- readRDS(here("data","derived-data","ocean_ODA_2010_2021_full_with_equity_50177.rds"))

oecd.ctry.cadc <- oecd.dat.out.sf %>% 
  group_by(iso_a3,year,cadc.type) %>%  
  summarize(total.project=n(),
            usd.comm=sum(commitment_usd_million_defl,na.rm=T),
            usd.disb=sum(disbursement_usd_million_defl,na.rm=T)) 
head(oecd.ctry.cadc)
dim(oecd.ctry.cadc)

# join with country
oecd.ctry.cadc.sf <- oecd.dat.out.sf %>% dplyr::select(iso_a3,iso_a2,continent,region_un,subregion,region_wb,name_en) %>%
  distinct() %>%
  left_join(oecd.ctry.cadc,by="iso_a3")

# transform to sf
oecd.ctry.cadc.sf <- oecd.ctry.cadc.sf %>%
  left_join(world %>% dplyr::select(iso_a3),by="iso_a3") %>%
  st_as_sf()
saveRDS(oecd.ctry.cadc.sf,here("data","derived-data","ocean_ODA_CADC_2010_2021_equity_sf.RDS"))

# # # #---- summaries ----
# # # # Annual change in USD disbursement
# oecd.ODA_year_country_CADC <- oecd.dat.out.sf %>%
#    dplyr::group_by(iso_a3,year,cadc.type) %>%
#    summarise(usd.comm=sum(commitment_usd_million_defl,na.rm=T),
#              usd.disb=sum(disbursement_usd_million_defl,na.rm=T)) %>%
#   arrange(cadc.type,year) %>%
#   dplyr::group_by(year,cadc.type) %>%
#   mutate(cum.usd.comm=cumsum(usd.comm),
#          cum.usd.disb=cumsum(usd.disb)) %>%
#   dplyr::group_by(year) %>%
#   mutate(pct.comm=usd.comm/sum(usd.comm),
#          pct.disb=usd.disb/sum(usd.disb)) %>%
#   gather(var,val,usd.comm:pct.disb) #equity % 23% in 2010, 45% in 2021
# 
# saveRDS(oecd.dat.out.sf,here("data","derived-data","ocean_ODA_2010_2021_full_with_equity_50177.rds"))

# # plot temporal trend
# p.oecd.trend <- oecd.trend %>% 
#   filter()
#    ggplot( aes(x = year, y = val, fill=cadc.type)) +
#    geom_area() +
#    labs(title=" 'equity` CADC vs other CADC", y="USD million committed") +
#     facet_wrap(.~var, scales = "free_y") +
#    theme_classic()
# p.oecd.trend
# 
# #----- maps ---- 
# # summarise values by country and cadc.type for export ()
# oecd.ctry <- oecd %>% 
#   group_by(ctry,cadc.type) %>%  
#   summarise(total.project=n(),
#             usd.comm=sum(commitment_usd_million_defl,na.rm=T),
#             usd.disb=sum(disbursement_usd_million_defl,na.rm=T))  
# #  gather(var,val,total.project:usd.disb) # which format do you want this in? wide or long?
# head(oecd.ctry)
# 
# # top countries
# comm.vs.disb <- oecd.ctry %>% 
#   filter(cadc.type%in%c("equity CADC","other CADC") & ctry!="Bilateral, unspecified") %>% 
#   group_by(ctry) %>% 
#   summarise(usd.disb=sum(usd.disb)) %>% 
#   arrange(-usd.disb) %>% 
#   left_join(
# oecd.ctry %>% 
#   filter(cadc.type%in%c("equity CADC","other CADC") & ctry!="Bilateral, unspecified") %>% 
#   group_by(ctry) %>% 
#   summarise(usd.comm=sum(usd.comm)) %>% 
#   arrange(-usd.comm),
# by="ctry")
# comm.vs.disb
# plot(comm.vs.disb$usd.disb,comm.vs.disb$usd.comm)
# 
# # summarize by markers, in case it was needed...
# oecd.ctry.markers <- oecd %>% 
#   group_by(ctry,year,cadc.type,gender_equality,adaptation,mitigation,biodiversity) %>%  
#   summarise(total.project=n(),
#             usd.comm=sum(commitment_usd_million_defl,na.rm=T),
#             usd.disb=sum(disbursement_usd_million_defl,na.rm=T)) 
# head(oecd.ctry.markers)
# 
# # join oecd data to map
# CADC.OECD.ocean.wide.proj.ctr.sf <- projected.world %>%
#   left_join(oecd.ctry.markers,by=c("admin"="ctry")) %>% 
#   rename(ctry=admin)
# #head(select(CADC.OECD.ocean.wide.proj.ctr.sf,cadc.type:total.usd))
# 
# p.cadc.ctry.dat <- CADC.OECD.ocean.wide.proj.ctr.sf %>% 
#   # convert everything else to NAs to retain the other countries on map
#   mutate(across(total.project:usd.disb,
#                 ~ifelse(!cadc.type%in%c("equity CADC","other CADC"), NA,.))) %>% 
#   group_by(ctry) %>% 
#   summarise(across(total.project:usd.disb,~sum(.,na.rm = T))) %>% 
#   # convert everything else to NAs to retain the other countries on map
#   mutate(across(total.project:usd.disb,
#                 ~ifelse(.==0, NA,.))) 
# head(p.cadc.ctry.dat)
# 
# # Map: Total projects 
# p.cadc.ctry.proj <- p.cadc.ctry.dat %>%
#     ggplot() +
#     geom_sf(aes(fill = total.project))  +
#     theme_void() +
#     labs(title = "# CADC projects per country") +
#     scale_fill_distiller(palette = "Spectral", na.value = "white") 
# # Map: Total disbursed 
# p.cadc.ctry.usd.disb<- p.cadc.ctry.dat %>% 
#   ggplot() +
#   geom_sf(aes(fill = usd.disb))  + 
#   theme_void() +
#   labs(title = "CADC $USD million per country") + 
#   scale_fill_distiller(palette = "Spectral", na.value = "white")
# # Map: Total committed 
# p.cadc.ctry.usd.comm <- p.cadc.ctry.dat %>% 
#   ggplot() +
#     geom_sf(aes(fill = usd.comm))  + 
#     theme_void() +
#     labs(title = "CADC $USD million per country") + 
#     scale_fill_distiller(palette = "Spectral", na.value = "white")
# 
# 
# p.cadc.map <- cowplot::plot_grid(p.cadc.ctry.proj,p.cadc.ctry.usd.comm,p.cadc.ctry.usd.disb, nrow = 3)
# p.cadc.map
# ggsave(here("figures","cadc_ctry_map.png"),width = 4, height = 8)
# 
# # Export dataframe and maps
# p.cadc.ctry.usd.comm
# ggsave(here("data","data_check","OECD","cadc_usd_comm_ctry_map.png"),width = 8, height = 4)
# p.oecd.trend
# ggsave(here("data","data_check","OECD","oecd_trend.png"),width = 8, height = 8)
# export(oecd.ctry,here("data","derived-data","ocean_ODA_CADC_2010_2021_equity.csv"))
# saveRDS(CADC.OECD.ocean.wide.proj.ctr.sf,here("data","derived-data","ocean_ODA_CADC_2010_2021_equity_sf.RDS"))
