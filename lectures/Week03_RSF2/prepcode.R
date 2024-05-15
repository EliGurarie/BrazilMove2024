load("data/ducks22.covars.rda")
ducks.used <- subset(ducks22.covars, Used == 1)
ducks.used <- ducks.used[seq(1, nrow(ducks.used), 5),]
table(ducks.used$DuckID)
require(mapview)
mapview(ducks.used, zcol = "DuckID")

require(magrittr)
counties <- st_read("data/NYcounties/Counties_Shoreline.shp")

require(terra)



# distance to coast raster






cropbox <- ext(c(650000, 760000, 4500000, 4570000))

LI.landcover.cropped <- crop(LI.landcover, cropbox)


# compute distance to shore raster
eval <- FALSE
if(eval){
  Suffolk.lines <- st_cast(Suffolk, "MULTILINESTRING")
  xy <- rastercoord.sf[onland == 1,] %>% st_coordinates
  d <- terra::distance(LI.landcover.cropped, Suffolk.lines)
  d.masked <- mask(d, Suffolk)
  
  names(LI.landcover.cropped) <- "landcover"
  names(d.masked) <- "dtoshore"
  
  habitat <- c(LI.landcover.cropped, d.masked)
  names(habitat) <- c("landcover", "dtoshore")
  writeRaster(d.masked, "data/DtoShore.tif")
  writeRaster(LI.landcover.cropped, "data/Landcover.tif")
              
  Suffolk <- counties %>% subset(NAME == "Suffolk") %>% st_transform(crs(ducks.used))
  save(Suffolk, file = "data/Suffolk.rda")
}

habitat
plot(d.masked)
# stack & save distance & land cover raster

# extact the distance & lc for the RSF object

#LI.landcover <- rast("data/nlcd_2021_land_cover_LI.tif")


duck.landcover <- rast("data/Landcover.tif")
dtoshore <- rast("data/DtoShore.tif")
habitat <- c(duck.landcover,dtoshore)

load("data/Suffolk.rda")
plot(habitat)

require(magrittr)
require(sf)
load("data/ducks22.rda")


# load duck data and transform to habitat CRS

ducks22 <- ducks22[,c("device_id", "Longitude", "Latitude","datetime")] %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% st_transform(crs(Suffolk))


ducks22 <- ducks22[seq(1,nrow(ducks22), 4),]
  

plot(st_geometry(Suffolk))
plot(st_geometry(ducks22), add = TRUE)

xy.null <- st_sample(Suffolk, 30000)
points(st_coordinates(xy.null), col = 2, pch = 19, cex = 0.2)

require(terra)
landcover.null <- terra::extract(habitat, st_coordinates(xy.null))
landcover.ducks <- terra::extract(habitat, st_coordinates(ducks22))

ducks.rsf <- rbind(data.frame(
                    datetime = ducks22$datetime,
                    st_coordinates(ducks22), 
                              landcover.ducks, Used = TRUE),
                   data.frame(
                     datetime = NA,
                     st_coordinates(xy.null), 
                              landcover.null, Used = FALSE))

lc.names <- read.csv("./data/LandcoverVal.csv")
names(lc.names) <- c("landcover", "landcover.name")
ducks.rsf <- merge(ducks.rsf, lc.names, keep = "all")
ducks.rsf$landcover.name <- factor(ducks.rsf$landcover.name)
save(ducks.rsf, file = "data/ducks_rsf.rda")

# save RSF object

# Merge landcover names

