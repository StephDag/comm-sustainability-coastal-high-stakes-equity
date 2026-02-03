#***********************************************************************
# Figure2_Contextual_inequity_pixel_level
# ---------------------------------
# INPUTS:
# - composite scores data file : df.cont.inequity.compo.coastal.with.scores.rds (script 07)
# - spatial df of composite scores: sf_df.risk_stack_ctry.rds (script 06)
# OUTPUTS
# - Figure2_4maps.jpg
# - df.cont.inequity.compo.coastal.scores_sr.rds

#***********************************************************************
# Creation : Pavanee Annasawmy
# modification: Stéphanie D'Agata
# Email :angelee.annasawmy@fondationbiodiversite.fr; angelee-pavanee.annasawmy@ird.fr
# ORCID : https://orcid.org/0000-0001-8803-9687
# Institution : Centre de Synthèse et d'Analyse sur la Biodiversité de la Fondation pour la Recherche sur la Biodiversité 
#################################################

library(here)
source(here::here("analyses","00_setup.R"))
source(here::here("analyses","001_Coastal_countries.R"),echo=T)
source(here("R","NormMinMax.R"))

###Map contextual inequity at the pixel level

#Path to RDS file
# save df with composite scores
df.risk.stack.sc.ctry.ind.coastal <- readRDS(here("data","derived-data","df.cont.inequity.compo.coastal.with.scores.rds"))
names(df.risk.stack.sc.ctry.ind.coastal)

#Path to spatial dataframe
spatial_df <- readRDS(here("data","derived-data","sf_df.risk_stack_ctry.rds"))
names(spatial_df)

#Add columns iso_a3 and ID from the spatial df
df.cont.inequity.compo.coastal.scores <- df.risk.stack.sc.ctry.ind.coastal %>%
  left_join(spatial_df %>% dplyr::select(iso_a3, x, y, ID), by = c("ID","iso_a3"))

#source 00_setup.R script prior to running the 4 lines of code below
world <- ne_countries(scale = "large", returnclass = "sf") #Retrieve country-boundary data
class(world)
crs.val.robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #Robinson projection
projected.world = st_transform(world, crs.val.robinson) #transforms the spatial coordinates of the sf object

#Convert the coordinates x and y to an sf object with point geometry
df.cont.inequity.compo.coastal.scores_sr <- st_as_sf(x = df.cont.inequity.compo.coastal.scores,
                                                     coords = c("x", "y"),
                                                     crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

saveRDS(df.cont.inequity.compo.coastal.scores_sr,here("data","derived-data","df.cont.inequity.compo.coastal.scores_sr.rds"))

#Filter out NAs
df.cont.inequity.compo.coastal.scores_sr <- df.cont.inequity.compo.coastal.scores_sr %>%
  filter(!is.na(hierachical.score.rank.ineq))

#Map of scaled contextual inequity  with squares
p.map.ctry.cont_ineq <- ggplot() +
  geom_sf(data = projected.world, color = "black", fill = "white") +  
  geom_point(data = df.cont.inequity.compo.coastal.scores_sr, 
             aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], color = hierachical.score.rank.ineq),shape = 15) +
  scale_color_gradient(low = "#FFF7F3", high = "#AE017E", limits = c(0, 1)) +  
  labs(color = "Contextual inequity") + 
  theme_minimal(base_size = 18, base_family = "Helvetica") +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 18),
        legend.title = element_blank(),  # Remove the legend title
        axis.title.y = element_blank()) +
  ggtitle("A. Contextual inequity") 
p.map.ctry.cont_ineq
ggsave("Cont.ineq_pixel.jpg",  p.map.ctry.cont_ineq, width = 16, height = 10, dpi = 300)

# Load necessary libraries
#Map of scaled weak governance  with squares
p.map.ctry.gov <- ggplot() +
  geom_sf(data = projected.world, color = "black", fill = "white") +  
  geom_point(data = df.cont.inequity.compo.coastal.scores_sr, 
             aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], color = gov.score.rank),shape = 15) +
  scale_color_gradientn(colors = brewer.pal(n = 9, name = "Blues"), limits = c(0.0, 1.0)) +
  labs(color = "Contextual inequity") + 
  theme_minimal(base_size = 18, base_family = "Helvetica") +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 18),
        legend.title = element_blank(),  # Remove the legend title
        axis.title.y = element_blank()) +
  ggtitle("B. Weak governance") 
p.map.ctry.gov
ggsave("Weak_gov_pixel.jpg",  p.map.ctry.cont_ineq, width = 16, height = 10, dpi = 300)

#Map of scaled social inequality with squares
p.map.ctry.ineq <- ggplot() +
  geom_sf(data = projected.world, color = "black", fill = "white") +  
  geom_point(data = df.cont.inequity.compo.coastal.scores_sr, 
             aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], color = ineq.score.rank),shape = 15) +
  scale_color_gradientn(colors = brewer.pal(n = 9, name = "Purples"), limits = c(0.0, 1.0)) +
  labs(color = "Social inequality") + 
  theme_minimal(base_size = 18, base_family = "Helvetica") +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 18),
        legend.title = element_blank(),  # Remove the legend title
        axis.title.y = element_blank()) +
  ggtitle("C. Social inequality") 
p.map.ctry.ineq
ggsave("Social_ineq_pixel.jpg",  p.map.ctry.cont_ineq, width = 16, height = 10, dpi = 300)

#Map of scaled vulnerability with squares
p.map.ctry.SE <- ggplot() +
  geom_sf(data = projected.world, color = "black", fill = "white") +  
  geom_point(data = df.cont.inequity.compo.coastal.scores_sr, 
             aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], color = ineq.score.rank),shape = 15) +
  scale_color_gradientn(colors = brewer.pal(n = 9, name = "Oranges"), limits = c(0.0, 1.0)) +
  theme_minimal(base_size = 18, base_family = "Helvetica") +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 18),
        legend.title = element_blank(),  # Remove the legend title
        axis.title.y = element_blank()) +
  ggtitle("D. Social-ecological vulnerability") 
p.map.ctry.SE
ggsave("Social_SE.jpg",  p.map.ctry.cont_ineq, width = 16, height = 10, dpi = 300)

P.4maps= grid.arrange(p.map.ctry.cont_ineq, p.map.ctry.gov, p.map.ctry.ineq, p.map.ctry.SE, ncol = 1, nrow = 4)
P.4maps
ggsave(here("figures","Figure2_4maps.jpg"),  P.4maps,dpi = 300,width=12,height=16)




