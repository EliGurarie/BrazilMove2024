
## ----cache = FALSE--------------------------------------------------------------
library(ggplot2)
library(plyr)
library(sf)
library(adehabitatHR)
library(mapview)


## ----loadData, echo = -1--------------------------------------------------------
setwd("../")
load("./data/elk_processed.rda")
head(elk_gps)


## ----SelectResidentElk----------------------------------------------------------
elk_res <- elk_gps |>
  subset(id %in% c("YL80", "YL91", "YL94")) |>
  mutate(id = droplevels(id))

elk_res_sf <- elk_res |>
  st_as_sf(coords = c("lon","lat"), crs = 4326) |>
  st_transform(32611)

elk_res <- cbind(elk_res, st_coordinates(elk_res_sf))


## ----PickAnElk------------------------------------------------------------------
myelk_sf <- elk_res_sf |> subset(id == "YL80")
myelk_sp <- myelk_sf |>  as_Spatial()
myelk_kernelud <- kernelUD(myelk_sp, grid = 200)


## -------------------------------------------------------------------------------
plot(myelk_kernelud)


## -------------------------------------------------------------------------------
require(raster)
my_kernel <- raster(myelk_kernelud)
my_kernel
plot(my_kernel)


## ----mapviewKernel, cache = FALSE-----------------------------------------------
mapview(my_kernel)


## ----contourPlotKernel----------------------------------------------------------
plot(my_kernel)
contour(my_kernel, add = TRUE)


## ----PerspPlotKernel------------------------------------------------------------
persp(my_kernel, border = NA, shade = TRUE)


## ----getKDEpolygon--------------------------------------------------------------
myelk_kde_poly <- getverticeshr(myelk_kernelud, percent = 95) |>
  st_as_sf()
myelk_kde_poly


## ----getArea--------------------------------------------------------------------
st_area(myelk_kde_poly)


## ----mapview2, cache =FALSE-----------------------------------------------------
mapview(myelk_kde_poly) + mapview(myelk_sf)


## ----getKernelPolyFunction------------------------------------------------------
getKernelPoly <- function(sf, percent = 95, ...){
  sp <- sf |> mutate(id = droplevels(id))
    as_Spatial(sp[,"id"], cast = TRUE, IDs = "id") |> kernelUD(...) |>
    getverticeshr(percent = 95) |>
    st_as_sf()
}


## ----ComparingKernels-----------------------------------------------------------
kde_poly_norm <- getKernelPoly(elk_sf |> subset(id == "YL91"), kern = "bivnorm")
kde_poly_epa <- getKernelPoly(elk_sf |> subset(id == "YL91"), kern = "epa")

kde_compare_kernels <- rbind(
  kde_poly_norm |> mutate(type = "Bivariate Normal"),
  kde_poly_epa |> mutate(type = "Epanechnikov"))

ggplot(kde_compare_kernels) + geom_sf(aes(fill = type), alpha = .5) +
    geom_sf(data = elk_sf |> subset(id == "YL91"), alpha = .2, size = 1)


## ----computeAllMCPs-------------------------------------------------------------
MCP_allElks <- getKernelPoly(elk_sf |> st_transform(32611), kern = "epa")


## ----mapAllMCPs-----------------------------------------------------------------
ggplot(MCP_allElks) +
  geom_sf(aes(fill = id, color = id), alpha = .2)


## ----AndSummarize---------------------------------------------------------------
MCP_allElks
summary(MCP_allElks$area)


## ----computeTwoKernels----------------------------------------------------------
elk1 <- elk_res_sf |> subset(id == "YL80")
elk2 <- elk_res_sf |> subset(id == "YL94")

kernel1 <- elk1 |>  as_Spatial() |>  kernelUD(grid = 200)
kernel2 <- elk2 |>  as_Spatial() |>  kernelUD(grid = 200)


## ----Countour2Kernels-----------------------------------------------------------
contour(kernel1,
        xlim = c(590e3, 610e3), ylim = c(5720e3, 5745e3),
        col = "blue")
 contour(kernel2, add = TRUE, col = "red")
axis(1); axis(2)


## -------------------------------------------------------------------------------
threeElks_sp <- as_Spatial(elk_res_sf[,"id"], cast = TRUE, IDs = "id")
kerneloverlap(threeElks_sp, method = "VI", kern = "epa")

