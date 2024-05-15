require(terra)
require(magrittr)
require(plyr)


# Trimming raster

eval <- FALSE
if(eval)
{
  duck.landcover <- rast("data/Landcover.tif")
  
  # xy <- locator(3)
  
  require(sf)
  
  cutoff <- st_polygon(list(as.matrix(data.frame(xy))[c(1:3,1),])) %>% 
    st_sfc(crs = crs(duck.landcover)) %>% st_as_sf
  
  require(fasterize)
  d <- fasterize(st_as_sf(cutoff), 
                 raster(duck.landcover), fun = "any")
  duck.landcover[rast(d) == 1] <- NA
  duck.landcover[duck.landcover == 0] <- NA
  
  plot(duck.landcover)
  
  terra::writeRaster(duck.landcover, file = "data/Landcover2.tif")
}

duck.landcover <- rast("data/Landcover2.tif")
plot(duck.landcover)


counts <- table(values(duck.landcover))
counts[["11"]] <- 10e5

coltab <-  coltab(duck.landcover)[[1]] %>% subset(value %in% names(counts)) %>%
  mutate(color = rgb(red/255, green/255, blue/255)) %>% rename(c(value = "Value"))


landcover.df <- read.csv("data/LandcoverVal.csv") 
names(landcover.df)[1] <- "Value"
landcover.df <- merge(landcover.df, coltab[,c("Value","color")], by = "Value", all.x = TRUE)

load("data/ducks_rsf.rda")


duck_available <- 
  as.data.frame(counts) %>% plyr::rename(c(Var1 = "Value", Freq = "Available")) %>% 
  merge(landcover.df) 

duck_used <-table(ducks.rsf$landcover.name[ducks.rsf$Used == TRUE]) %>% as.data.frame %>% 
  plyr::rename(c(Var1 = "Landcover", Freq = "Used"))

duck_ua <- merge(duck_available, duck_used, keep = "all")



with(duck_ua %>% arrange(Available, decreasing = TRUE),{
  barplot(Available/sum(Available) * 100, horiz = TRUE, space = 0, 
          col = color, xlim = c(0, 40), main = "Available")
    mtext(side = 2, at = 1:length(Used) - .5, Landcover, las = 1, cex = 0.7, line = 0.5)
    barplot(Used/sum(Used) * 100, horiz = TRUE, space = 0, 
            col = color, xlim = c(0, 40), main = "Used")
})

png("DuckAvailable.png", width = 2000, height = 1000, res = 200)

layout(t(1:2), widths = c(1.5,1))
plot(duck.landcover)
par(mar = c(3,8,4,1), tck = 0.01, 
    mgp = c(1,.25,0), cex.axis = 0.8)

with(duck_ua %>% arrange(Available, decreasing = TRUE),
     {
       
       barplot(Available/sum(Available) * 100, horiz = TRUE, space = 0, 
          col = color, xlim = c(0, 40), main = "Available habitat")
       mtext(side = 2, at = 1:length(Used) - .5, Landcover, las = 1, cex = 0.7, line = 0.5)
     }
)

dev.off()


png("DuckUsed.png", width = 2000, height = 1000, res = 200)

layout(t(1:2), widths = c(1.5,1))
plot(duck.landcover, main = "Long Island - Suffolk County")
with(ducks.rsf %>% subset(Used),
    points(X,Y, col = rgb(0,0,0,.3), pch = 19, cex= 0.5)     
)

par(mar = c(3,8,4,1), tck = 0.01, 
    mgp = c(1,.25,0), cex.axis = 0.8)

with(duck_ua %>% arrange(Available, decreasing = TRUE),
     {
       barplot(Used/sum(Used) * 100, horiz = TRUE, space = 0, 
               col = color, xlim = c(0, 40), main = "Used habitat")
       mtext(side = 2, at = 1:length(Used) - .5, Landcover, las = 1, cex = 0.7, line = 0.5)
     }
)
dev.off()

png("ProportionAvailableVsUsed.png", width = 2000, height = 1000, res = 200)


par(mar = c(3,0,4,1), tck = 0.01, mfrow = c(1,3), oma = c(0,14,0,0),
    mgp = c(1,.25,0), cex.axis = 0.8, bty = "l")

with(duck_ua %>% arrange(Available, decreasing = TRUE),
     {
       barplot(Available/sum(Available) * 100, horiz = TRUE, space = 0, 
               col = color, xlim = c(0, 40), main = "Available habitat")
       mtext(side = 2, at = 1:length(Used) - .5, Landcover, 
             las = 1, cex = 0.7, line = 0.5)
     }
)

with(duck_ua %>% arrange(Available, decreasing = TRUE),
     {
       barplot(Used/sum(Used) * 100, horiz = TRUE, space = 0, 
               col = color, xlim = c(0, 40), main = "Used habitat")
     }
)

n <- nrow(duck_ua)

with(duck_ua %>% arrange(Available, decreasing = TRUE),
     {
       plot(Used/Available * sum(Available)/sum(Used), 
            1:length(Used), 
            pch = 19, col = color, cex = 2, log = "x", 
            main = "Preference", xlab = "")
       segments(rep(1, n), 1:n, 
                Used/Available * sum(Available)/sum(Used), 
            col = color, lwd = 2)
       abline(v = 1, col = "grey")
       
       #points
       #col = color, xlim = c(0, 40), main = "Used habitat")
     }
)

dev.off()
