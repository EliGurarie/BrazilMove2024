
## packages ------------------------------------------------
require(sf)
require(terra)


## habitatplot ----------------------------------------------------------------------
require(terra)
duck.landcover <- rast("data/Landcover.tif")
dtoshore <- rast("data/DtoShore.tif")
habitat <- c(duck.landcover,dtoshore)
plot(habitat)


## Load data ----------------------
load("data/ducks_rsf.rda")
ducks.rsf %>% head
load("data/Suffolk.rda")


## Plot Points -----------------------------------
plot(st_geometry(Suffolk))
with(ducks.rsf[ducks.rsf$Used == 0,],
     points(X,Y, pch = 19, cex = 0.1, col = rgb(0,0,0,.1)))
with(ducks.rsf[ducks.rsf$Used == 1,], 
     points(X,Y, pch = 19, cex = 0.3, col = rgb(1,0,0,.5)))


## FITTED MODEL ---------------------------------------------------------------------
duck.fit <- glm(Used ~ landcover.name + 
                  dtoshore, family = "binomial", data = ducks.rsf)

summary(duck.fit)


## Coefficient Plots ----

require(broom)
tidy(duck.fit)
require(coefplot)
coefplot(duck.fit, sort = "magnitude")


## 5-fold validation: Blocking by Individuals --------------------------------------------------------------

ducks.rsf %>% head


n <- nrow(ducks.rsf)
k <- 5

training <- sample(1:n)[1:(round(n*(1-1/k)))]
test <- (1:n)[! (1:n) %in% training]

ducks_training <- ducks.rsf[training,]
ducks_test <- ducks.rsf[test,]


## -------------------------------------------------------------------------------------
duck.fit.training <- glm(Used ~ landcover.name + 
                  dtoshore, family = "binomial", data = ducks_training)


## -------------------------------------------------------------------------------------
nbins <- 10
duck.predict.test  <- predict(duck.fit.training, newdata = ducks_test)
predict.quantiles <- quantile(duck.predict.test, seq(0,1, length = nbins+1), na.rm = TRUE)

predict.matrix <- as.matrix(table(ducks_test$Used, cut(duck.predict.test, predict.quantiles)))

p.present <- apply(predict.matrix, 2, function(x) x[2]/sum(x))
cv.score <- cor(1:nbins, p.present, method = "spearman")


## -------------------------------------------------------------------------------------
cv.score


## ---- eval = FALSE--------------------------------------------------------------------
## #library(devtools)
## #install_github("BastilleRousseau/IndRSA")


## -------------------------------------------------------------------------------------
#require(IndRSA)
#duck_kfold <- kfoldRSF(duck.fit)
#duck_kfold


## -------------------------------------------------------------------------------------
duck.fit <- glm(Used ~ factor(landcover) + 
                  dtoshore, family = "binomial", data = ducks.rsf)


## -------------------------------------------------------------------------------------
allcovars.df <- terra::as.data.frame(habitat, na.rm = FALSE)


## ---- eval = FALSE--------------------------------------------------------------------
## duck.predict.values <- predict(duck.fit, newdata = allcovars.df)


## -------------------------------------------------------------------------------------
allcovars.df$landcover[allcovars.df$landcover == 0] <- NA
duck.predict.values <- predict(duck.fit, newdata = allcovars.df)


## -------------------------------------------------------------------------------------
duck.predict.raster <- habitat[[1]]


## -------------------------------------------------------------------------------------
values(duck.predict.raster) <- duck.predict.values


## -------------------------------------------------------------------------------------
plot(duck.predict.raster, main = "predictor")

## -------------------------------------------------------------------------------------
require(rasterVis)
levelplot(duck.predict.raster)


## -------------------------------------------------------------------------------------
levelplot(exp(duck.predict.raster)/(1+exp(duck.predict.raster)), main = "probabilities?")


## -------------------------------------------------------------------------------------
levelplot(exp(duck.predict.raster), main = "w(x)")


## -------------------------------------------------------------------------------------
n.bins <- 10
breaks <- quantile(duck.predict.values, seq(0,1,length = n.bins + 1), na.rm = TRUE)
duck.predict.binned <- cut(duck.predict.values, breaks)

duck.predict.raster.binned <- duck.predict.raster
values(duck.predict.raster.binned) <- duck.predict.binned


## -------------------------------------------------------------------------------------
require(gplots)
plot(duck.predict.raster.binned, col = rich.colors(n.bins))


## -------------------------------------------------------------------------------------
getBinnedRaster  <- function(duck.model, n.bins){
  
  duck.predict.values <- predict(duck.model, newdata = allcovars.df)
  breaks <- quantile(duck.predict.values, seq(0,1,length = n.bins + 1), na.rm = TRUE)
  duck.predict.binned <- cut(duck.predict.values, breaks)
  duck.predict.raster.binned <- duck.predict.raster
  
  values(duck.predict.raster.binned) <- duck.predict.binned
  return(duck.predict.raster.binned)
}


## -------------------------------------------------------------------------------------
r1 <- getBinnedRaster(duck.fit, 10)
plot(r1,  col = rich.colors(10))


## -------------------------------------------------------------------------------------
duck.fit2 <- glm(Used ~ factor(landcover) + dtoshore + I(dtoshore^2), 
                 family = "binomial", data = ducks.rsf)
r2 <- getBinnedRaster(duck.fit2, 10)
plot(r2,  col = rich.colors(10))


## -------------------------------------------------------------------------------------
require(lubridate)

duck.fit.dawn <- glm(Used ~ factor(landcover) + dtoshore, 
                 family = "binomial", data = ducks.rsf %>% 
                   subset(hour(datetime) > 5 & hour(datetime) < 8 | 
                    Used == FALSE))

duck.fit.dusk <- glm(Used ~ factor(landcover) + dtoshore, 
                 family = "binomial", data = ducks.rsf %>% 
                   subset(hour(datetime) > 18 | hour(datetime) < 21 | 
                    Used == FALSE))

r.dawn <- getBinnedRaster(duck.fit.midday, 10)
r.dusk <- getBinnedRaster(duck.fit.midnight, 10)



## ----DayVNight------------------------------------------------------------------------
par(mfrow = c(1,2))
plot(r.midday,  col = rich.colors(10), main = "dawn")
plot(r.midnight,  col = rich.colors(10), main = "dusk")


## -------------------------------------------------------------------------------------
glm(Used ~ landcover.name + dtoshore, 
                 family = "binomial", data = ducks.rsf %>% 
                   subset(hour(datetime) > 5 & hour(datetime) < 8 | Used == FALSE)) %>% 
  coefplot(sort = "magnitude", title = "dawn")

glm(Used ~ landcover.name + dtoshore, 
                 family = "binomial", data = ducks.rsf %>% 
                   subset(hour(datetime) > 18 | hour(datetime) < 21 | Used == FALSE))%>% 
  coefplot(sort = "magnitude", title = "dusk")


