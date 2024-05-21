library(ggplot2)
library(sf)

load("data/elk_processed.rda")
is(elk_gps)
is(elk_sf)

str(elk_gps)
elk_gps <- elk_gps |> subset(id %in% levels(id)[1:9])
elk_sf <- elk_sf |> subset(id %in% levels(id)[1:9])

E4049 <- subset(elk_gps, id == "4049")
with(E4049, plot(lon, lat))

with(E4049, plot(lon, lat, type = "o"))

long2UTM <- function(long) (floor((long + 180)/6) %% 60) + 1

long2UTM(-115)

E4049.xy <-  elk_sf |> subset(id == 4049) |> st_transform(32611) |> st_coordinates()
plot(E4049.xy, asp = 1, type = "o")

require(sf)
elk_gps <-  elk_gps |> data.frame(elk_sf |> st_transform(32611) |> st_coordinates())
str(elk_gps)

par(mar = c(0,4,0,0), oma = c(4,0,5,2), xpd=NA)
layout(rbind(c(1,2), c(1,3)))
with(E4049, {
  plot(lon, lat, asp = 1, type="o", ylab="Latitude", xlab="Longitude")
  plot(datetime, lon, type="o", xaxt="n", ylab="Longitude", xlab="")
  plot(datetime, lat, type="o", ylab="Latitude", xlab="Datetime")
  title(paste("ID", id[1]), outer = TRUE)
})

scan_track <- function(dataframe, x = "lon", y = "lat",
                       time = "datetime", id = "id", ...){
  par(mar = c(0,4,0,0), oma = c(4,0,5,2), xpd=NA)
  layout(rbind(c(1,2), c(1,3)))
  with(dataframe, {
    plot(get(x), get(y), asp = 1, type="o", ylab=y, xlab=x, ...)
    plot(get(time), get(x), type="o", xaxt="n", ylab=y, xlab="", ...)
    plot(get(time), get(y), type="o", ylab=y, xlab=time, ...)
    title(paste("ID", id[1]), outer = TRUE)
  })
}

scan_track(elk_gps |> subset(id == id[1]))

myelk <- elk_gps |> subset(id == id[1])
scan_track(myelk, x = "X", y = "Y",
           col = topo.colors(nrow(myelk)))

require(plyr)
elk_gps |> subset(id %in% levels(id)[1:5]) |> d_ply("id", scan_track)

library(ggplot2)

ggplot(data = E4049, aes(x = lon, y = lat)) +
  geom_path(size = 0.5) +
  geom_point(aes(color = datetime)) + theme_classic()

ggplot(data = elk_gps, aes(x = lon, y = lat)) +
  geom_path(size = 0.5, color = "darkgrey") +
  geom_point() +
  theme_classic() +
  facet_wrap(~id, scale="free", ncol=3)

ggplot(data = elk_gps, aes(x = lon, y = lat, col = id, group =id)) +
  geom_path(size = 0.5, color = "darkgrey") +
  geom_point() +
  theme_classic()

ggplot(data = elk_gps, aes(x = datetime, y = lat)) +
  geom_path(size = 0.5) +
  xlab("DateTime") + ylab("Latitude") +
  theme_classic() +
  facet_wrap(~id, scale="free", ncol = 2)

plot(elk_sf[,"id"], pch = 19)

library(ggmap)

key <- "e644fe03-1f7b-4b05-87a8-c65335eb4625"
register_stadiamaps(key, write = FALSE)

elk_box <- c(left = -116.5, bottom = 51.3, right = -115.4, top = 52.2)

basemap <- get_map(elk_box, source = "stadia", maptype  = "stamen_terrain")

ggmap(basemap, extent = "normal") +
  geom_point(data = elk_gps, aes(color=id)) +
  geom_path(data = elk_gps, aes(color=id)) +
  scale_color_brewer()

library(ggspatial)

box <- st_bbox(c(xmin = -116.4, xmax = -115.4, ymin = 51.5, ymax = 52.2), crs = st_crs(4326))

E4049.sf <- subset(elk_sf, id == "4049")

ggplot() +
  annotation_map_tile(type = 'osm', zoom = 11) +
  annotation_scale() +
  annotation_north_arrow(height=unit(0.5,"cm"), width=unit(0.5,"cm"), pad_y = unit(1,"cm"))+
  shadow_spatial(box )+
  ylab("Latitude") + xlab("Longitude")+
  scale_x_continuous(breaks= seq(-116, -115.2, .4))+
  geom_sf(data=E4049.sf, aes(), color="orange", size=2)+
  theme_classic()

jpeg(file="./E4049_Map.jpg", units="in", width=4, height=7,res=300)
E4049.sf <- subset(elk_sf, id == "4049")
ggplot() +
  annotation_map_tile(type = 'osm', zoom = 12) +
  annotation_scale() +
  annotation_north_arrow(height=unit(0.5,"cm"), width=unit(0.5,"cm"), pad_y = unit(1,"cm"))+
  shadow_spatial(box )+
  ylab("Latitude") + xlab("Longitude")+
  scale_x_continuous(breaks= seq(-116, -115.2, .4))+
  geom_sf(data=E4049.sf, aes(), color="orange", size=2)+
  theme_classic()
dev.off()

library(mapview)

elk_tracks <- elk_sf |>
  group_by(id) |>
  summarize(do_union=FALSE) |>
  st_cast("LINESTRING")

mapview(elk_tracks, zcol="id")
