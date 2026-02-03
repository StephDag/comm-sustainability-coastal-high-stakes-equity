#***********************************************************************
# Generate the inland 100km coastal buffer
# --------------------------------------------------------------
# INPUTS:
# - world countries map
# 
# OUTPUTS
# - .rds with the 100km coastal buffer of the world
#***********************************************************************
# Creation : Stéphanie D'Agata
# Email :stephanie.dagata@ird.fr
# ORCID : https://orcid.org/0000-0001-6941-8489
# Institution : Institut de Recherche pour le Développement
#***********************************************************************

robin = CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
prj_dd <- "EPSG:4326"
st_crs(all.gps.sp.1) = 4326

# world
rm(ROI)
ROI = ne_countries(returnclass = 'sf',scale = "large") %>%
  st_combine() %>%
  st_transform(crs=robin) %>%
  st_make_valid()

# check valid to perform union
sf::st_is_valid(ROI)

# union countries
ROI.comb <- st_union(ROI)

# inland buffer 100km
rm(inlandWaters)
inlandWaters = ROI.comb %>%
  st_buffer(-100000) # 100km inland

#### loop to make buffer from 100km from the coast
buffer <- seq(10000)

st_crs(ROI.comb)$units

for (i in 1:length(buffer)) {
  # inland full buffer
  inlandWaters.temp = ROI.comb %>%
    st_buffer(-buffer[i])
  
  # intersection
  rm(inlandWaters.int.temp)
  inlandWaters.int.temp <- st_difference( ROI.comb,inlandWaters.temp)

  # save buffer
  saveRDS(inlandWaters.int.temp,here::here("data","derived-data",paste0("inlandBuffer_",buffer[i]/1000,"km.rds")))
  
}

mapview::mapView(inlandWaters.int.temp)

# test match crs to plot
st_crs(ROI.comb) == st_crs(inlandWaters.temp)

# plot inland with 
plot.inland <- ggplot() +
geom_sf(data = inlandWaters.int.temp) +
geom_sf(data = inlandWaters.int.temp, fill = "lightblue", col = "transparent") 
plot.inland


