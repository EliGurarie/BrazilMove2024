## ----setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----------------------------------------------------------------------------
library(ggplot2)
library(plyr)
library(sf)
library(adehabitatHR)
library(mapview)


## ----loadData, echo = -1-----------------------------------------------------
setwd("../")
load("./data/elk_processed.rda")
head(elk_gps)


## ----SelectResidentElk-------------------------------------------------------
elk_res <- elk_gps |>
  subset(id %in% c("YL80", "YL91", "YL94")) |>
  mutate(id = droplevels(id))

elk_res_sf <- elk_res |> 
  st_as_sf(coords = c("lon","lat"), crs = 4326) |> 
  st_transform(32611) 

elk_res <- cbind(elk_res, st_coordinates(elk_res_sf))
str(elk_res)


## ----ThreeElkMapped----------------------------------------------------------
ggplot(data=elk_res, aes(x=X, y=Y, col = id)) +
  geom_path() + coord_fixed() + ggtitle("Some (mostly) resident elk")


## ----mapviewElkTracks--------------------------------------------------------
require(dplyr)
elk_tracks <- elk_res_sf |> 
  group_by(id) |> 
  summarize(do_union=FALSE) |> 
  st_cast("LINESTRING")

mapview(elk_tracks,zcol="id")


## ----------------------------------------------------------------------------
library(adehabitatHR)


## ----------------------------------------------------------------------------
myelk_sf <- elk_res_sf |> subset(id == "YL94")
myelk_sp <- myelk_sf |> as_Spatial()


## ----mcp95-------------------------------------------------------------------
(mcp95 <- mcp(myelk_sp, percent = 95, unout = "km2"))


## ----mcp100------------------------------------------------------------------
(mcp100 <- mcp(myelk_sp, percent = 100, unout = "km2"))


## ----------------------------------------------------------------------------
mcp95 <- st_as_sf(mcp95)
mcp100 <- st_as_sf(mcp100)

ggplot() + 
  geom_sf(data = mcp100, color = "blue", fill = alpha("blue",.2)) +
    geom_sf(data = mcp95, color = "red", fill = alpha("red",.2)) + 
     geom_sf(data = myelk_sf, alpha = 0.2)


## ----findMCPFunction---------------------------------------------------------
findMCP <- function(id_sf, percent){
  id_sp <- id_sf |> as_Spatial()
  id_mcp <- mcp(id_sp, percent, unout="km2")
  return(st_as_sf(id_mcp))
}
findMCP(elk_res_sf |> subset(id == "YL80"), 95)
findMCP(elk_res_sf |> subset(id == "YL91"), 95)
findMCP(elk_res_sf |> subset(id == "YL94"), 95)


## ----------------------------------------------------------------------------
elk_sp <- elk_res_sf |> mutate(datetime = NULL) |> as_Spatial(IDs = "id") 
elk_mcps <- mcp(elk_sp, percent = 95, unout = "km2") |> st_as_sf()
mapview(elk_mcps, zcol = "id")

