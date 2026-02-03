#***********************************************************************
# Compute the gravity of species at risk indicator for each coastal pixel
# --------------------------------------------------------------
# INPUTS:
# - species at risk count from Ohara2021_Science
# - countries' iso codes + regions
# - 100km coastal raster
# 
# OUTPUTS
# - Populated Terrestrial raster (2015 - 5km) of species at risk count
#***********************************************************************
# Creation : Stéphanie D'Agata
# Email :stephanie.dagata@ird.fr
# ORCID : https://orcid.org/0000-0001-6941-8489
# Institution : Institut de Recherche pour le Développement
#***********************************************************************


 source(here::here("analyses","00_setup.R"))

# vulnerability frameword of IPCC 2019: hazards, exposure, contextual vulnerability

# Ohara et al,  2021 - https://www.science.org/doi/full/10.1126/science.abe6731
# already associate species with pressures: maps of at risk marine biodiversity, including all taxa, and all pressure
# a good proxy of both hazard and a part of exposure linked to ecosystem integrity at 1km resolution

## load data from figure 2 linked to github: https://github.com/oharac/bd_chi/tree/master
# https://github.com/oharac/bd_chi/tree/master/_output/rasters/impact_maps

# ## Fig. 2A - species count
imp_count  <- rast(here("data","raw-data","Ohara2021_Science","impact_all_2013.tif"))
# plot(imp_count)

## Fig. 2B - % species
nspp <- rast(here("data","raw-data","Ohara2021_Science","n_spp_map.tif")) 
imp_pct <- imp_count / nspp
# plot(imp_pct)

## Fig. 2C - intensification
incr <- rast(here("data","raw-data","Ohara2021_Science","intens_all_incr2.tif"))
decr <- rast(here("data","raw-data","Ohara2021_Science","intens_all_decr2.tif"))
nspp <- rast(here("data","raw-data","Ohara2021_Science","n_spp_map.tif")) 

int_pct <- (incr - decr) / nspp
# plot(int_pct)

#### human population in coastal areas (100km, 100m elevation) from sedac - in mollweide projection
# load the 100km coastal buffer
inlandWaters.100km <- readRDS(here::here("data","derived-data","inlandBuffer_100km.rds"))
inlandWaters.100km.vect <- vect(inlandWaters.100km)
inlandWaters.100km.vect <- project(inlandWaters.100km.vect,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

# load population raster 2015
pop.world.nc <- rast(here("data","raw-data","Word Population count  SEDAC 5km","gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev11_totpop_2pt5_min_nc","gpw_v4_population_count_adjusted_rev11_2pt5_min.nc"))

# the 4th raster is the count population for 2015: 4	Population Count, v4.11 (2015) (see doc)
pop.world <- pop.world.nc[[4]] 
# plot(pop.world)

# mollweide projection
pop.world.proj <- project(pop.world,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# plot(pop.world.proj)
# check

# intersect with 100km coastal buffer
# check crs and extent
#crs(pop.world.proj) == crs(inlandWaters.100km.vect)
#ext(pop.world.proj); ext(inlandWaters.100km.vect)

# crop the populated raster to 100km buffer
pop.world.nc.100km <- crop(pop.world.proj,ext(inlandWaters.100km.vect)+0.01)
pop.world.nc.100km.b <- mask(pop.world.nc.100km,inlandWaters.100km.vect)
#writeRaster(pop.world.nc.100km.b, here("data","derived-data","pop.world.nc.100km.b_100km.tif"),overwrite=TRUE)

## plot(pop.world.nc.100km.b)
#lines(inlandWaters.100km.vect)

# extract centroid of each pixel of coastal population
values.xy <- as.data.frame(pop.world.nc.100km.b,xy = TRUE)
names(values.xy) <- c("lon","lat","pop.count")
sf_obj <- st_as_sf(values.xy, coords = c("lon", "lat"),crs=crs(imp_count))

# source the marine_terrestial_raster.r function: 1. find within the 120km buffer the closest imp_count cell, and 2. compute the mean imp.count and gravity in a 25km buffer around this point
source(here::here("R","marine_terrestial_raster.R"))

# inputs
# exemple with 10 point
start_time <- Sys.time()

##### run by chunks
# Define the size of each chunk
#chunk_size <- 1000
chunk_size <- 20

# number of rows of sf
chunks.groups <- rep(seq_len(ceiling(dim(sf_obj)[1]/ chunk_size)),each = chunk_size,length.out = dim(sf_obj)[1])

# split by chunks
sf_obj.chunks <- split(sf_obj,f = chunks.groups)

# check dim (should be 1000 for each, and the last one == 549)
lapply(sf_obj.chunks,dim)

# full populated raster
#point <-sf_obj
#point <- sf_obj.chunks
buffer_ter <- 150000 # 150km terrestrial raster
buffer_marine <- 20000 # 20km coastal raster
#n.cores=40
n.cores=4
# standardized - min-max
source(here("R","NormMinMax.R"))

#################################
#    species at risk - count.   #
#################################

mcf<-function(f){ function(...) {tryCatch({f(...)} , error=function(e) e)}}
biodiv=imp_count
rm(results.sp.count.risk.df.full)
results.sp.count.risk.df.full <- data.frame()

for (i in 1:length(sf_obj.chunks)){
# for (i in c(1, 701)){
  print(paste("i=",i))
  point <- sf_obj.chunks[[300000]]
  results.sp.count.risk.temp <- mclapply(split(point, seq(nrow(point))), FUN=marine_terrestial_raster, buffer_ter=buffer_ter, buffer_marine=buffer_marine, biodiv=biodiv,mc.cores=n.cores)
  #results.sp.count.risk.temp <- mclapply(split(point, seq(nrow(point))), FUN=mcf(marine_terrestial_raster), buffer_ter=buffer_ter, buffer_marine=buffer_marine, biodiv=biodiv,mc.cores=n.cores)
  #results.sp.count.risk <- mclapply(split(point[[1]], seq(nrow(point[[1]]))), FUN=mcf(marine_terrestial_raster), buffer_ter=buffer_ter, buffer_marine=buffer_marine, biodiv=biodiv,mc.cores=n.cores)
  results.sp.count.risk.df.temp <- do.call(rbind.data.frame, results.sp.count.risk.temp)
  results.sp.count.risk.df.full <- rbind(results.sp.count.risk.df.full,results.sp.count.risk.df.temp)
  # save dataframe in case of crash
  saveRDS(results.sp.count.risk.df.full, here("data","derived-data","results.sp.count.risk.chunks.rds"))

}

#results.sp.count.risk.df <- do.call(rbind.data.frame, results.sp.count.risk)

# read
results.sp.count.risk.sf <- readRDS(here("data","derived-data","results.sp.count.risk.rds"))
summary(results.sp.count.risk.sf$mean.count)

results.sp.count.risk.df.full <- results.sp.count.risk.sf %>%
  mutate(mean.count.grav.V2 = mean.count/((distance/1000)^2)) %>%
  mutate(mean.count.grav.V2.log = log(mean.count.grav.V2+1)) %>%
  mutate(mean.count.grav.V2.log.norm = normalize(mean.count.grav.V2.log,na.rm = TRUE))

results.sp.count.risk.df.full$mean.count.grav.norm <- NULL
#head(results.sp.count.risk.df.full)
#hist(results.sp.count.risk.df.full$mean.count.grav.V2.log.norm)  

# transform to sf 
results.sp.count.risk.sf <- st_as_sf(x = results.sp.count.risk.df.full,                         
                                     geometry = results.sp.count.risk.df.full$point.geometry,
                                     crs = sf::st_crs(imp_pct))
#saveRDS(results.sp.count.risk.df.full, here("data","derived-data","results.sp.count.risk.corrected.rds"))

gc()

# rasterize : # pas certaine que ça fonctionne
results.sp.count.risk.df.full <- readRDS(here("data","derived-data","results.sp.count.risk.corrected.rds"))

sp.count.rast.ter<- stars::st_rasterize(results.sp.count.risk.df.full %>% dplyr::select(mean.count.grav.V2.log, geometry),
                                        stars::st_as_stars(st_bbox(pop.world.nc.100km.b), dx=res(pop.world.nc.100km.b)[1],dy=res(pop.world.nc.100km.b)[2],
                                                           values = NA_real_))

sp.count.rast.ter <- terra::rast(sp.count.rast.ter)
gc()
# save as tiff
writeRaster(sp.count.rast.ter, here("data","derived-data","sp.count.rast.ter.grav.tif"),overwrite=T)
gc()
