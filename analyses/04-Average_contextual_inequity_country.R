#***********************************************************************
# Average Contextual inequity score by country
# --------------------------------------------------------------
# INPUTS:
# - Pixel level contextual inequity score
# 
# 
# OUTPUTS
# - Average contextual inequity score by country
#***********************************************************************
# Creation : Stéphanie D'Agata
# Email :stephanie.dagata@ird.fr & David Gill
# ORCID : https://orcid.org/0000-0001-6941-8489
# Institution : Institut de Recherche pour le Développement
#***********************************************************************


library(here)
source(here::here("analyses","00_setup.R"))
source(here::here("analyses","001_Coastal_countries.R"),echo=T)

df.risk.stack.sc.ctry.ind.coastal <- readRDS(here("data","derived-data","df.cont.inequity.compo.coastal.scores.rds"))

# SUMMARIZE one indicator by country for the 115 countries
rm(cont.ineq.ctry)
cont.ineq.ctry <- df.risk.stack.sc.ctry.ind.coastal %>%
  dplyr::group_by(iso_a3) %>%
  dplyr::mutate(cont.eq.score.mean.rank = mean(hierachical.score.rank.ineq,na.rm=T) %>% round(2),
                cont.eq.sd.rank =  sd(hierachical.score.rank.ineq,na.rm=T) %>% round(2),
                vulnerab.score.mean.rank = mean(vulnerab.score.rank,na.rm=T) %>% round(2),
                vulnerab.sd.rank =  sd(vulnerab.score.rank,na.rm=T) %>% round(2),
                gov.score.mean.rank = mean(gov.score.rank,na.rm=T) %>% round(2),
                gov.sd.rank =  sd(gov.score.rank,na.rm=T) %>% round(2),
                ineq.score.mean.rank = mean(ineq.score.rank,na.rm=T) %>% round(2),
                ineq.sd.rank =  sd(ineq.score.rank,na.rm=T) %>% round(2),
                mean.cont.eq.score.3comp = (vulnerab.score.mean.rank+gov.score.mean.rank+ineq.score.mean.rank)/3) %>%
  mutate(cont.eq.score.Gini.rank = Gini(hierachical.score.rank.ineq,na.rm=T) %>% round(4)) %>%
  ungroup() %>%
  dplyr::select(iso_a3,name_en,cont.eq.score.mean.rank:ineq.sd.rank,cont.eq.score.Gini.rank,mean.cont.eq.score.3comp) %>%
  distinct() %>%
  dplyr::filter(!is.na(cont.eq.score.mean.rank)) %>%
  as.data.frame()

saveRDS(cont.ineq.ctry,here("data","derived-data","coastal_CI.rds"))

