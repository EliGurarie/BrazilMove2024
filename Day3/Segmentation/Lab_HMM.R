## ----setup, include=FALSE--------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, 
                      warning = FALSE, cache = TRUE)


## --------------------------------------------------------
setwd("../../")
load("./data/elk_processed.rda")
head(elk_gps)


## --------------------------------------------------------
library(ggplot2)
library(plyr)


## ----fig.height=15, fig.width=6--------------------------
ggplot(data=elk_gps, aes(x=lon, y=lat)) +
  geom_path(size=0.5) +
  theme_classic() +
  facet_wrap(~id, scale="free", ncol=3)


## --------------------------------------------------------
elk_mig <- elk_gps |>
  subset(id %in% 
           c("YL96", "GP2", "YL25", "YL29", "YL73", "YL77", "YL78")) |>
  mutate(id = droplevels(id))


## ----ggplotTracks----------------------------------------
ggplot(data=elk_mig, aes(x=datetime, y=lat)) +
  geom_path(size=0.5) +
  theme_classic() +
  facet_wrap(~id, scale="free", ncol=2)


## ----eval=FALSE------------------------------------------
## # install.packages("aniMotum",
## #                 repos = c("https://cloud.r-project.org",
## #                           "https://ianjonsen.r-universe.dev"),
## #                 dependencies = TRUE)
## 
## # install.packages("Matrix")
## # install.packages("TMB")


## --------------------------------------------------------
library(aniMotum)


## --------------------------------------------------------
elk_mig2 <- elk_mig |>
  mutate(lc = "G", date = datetime) |>
  dplyr::select(id, date, lc, lon, lat)


## --------------------------------------------------------
dtime <- function(t, ...) {difftime(t[-1], t[-length(t)], ...) %>% as.numeric}

require(dplyr)
sample.rate <- elk_mig2 |> 
  group_by(id) |>arrange(date) |>
  summarize(dtime = round(dtime(date, units ="hours")))

table(sample.rate$dtime)
plot(table(sample.rate$dtime))


## ----eval=FALSE------------------------------------------
## myelk <- elk_mig2 |> subset(id == "YL96")
## myelk_ssm_fit <- fit_ssm(myelk,
##                vmax = 20,
##                model = "crw",
##                time.step = 2,
##                control = ssm_control(verbose = 0))

## ----saveSSMFITresults, eval = FALSE, echo = FALSE-------
## setwd("../../")
## #save(elk_ssm_fit, file="./data/elk_ssm_fit.rda")
## save(myelk_ssm_fit, file="data/myelk_ssm_fit.rda")


## ----loadDataForAnalysis, echo = -1----------------------
setwd("../../")
load("./data/myelk_ssm_fit.rda")
myelk_reg <- elk_mig2 |> subset(id == "YL96")


## ----summarySSM------------------------------------------
summary(myelk_ssm_fit)


## ----extactPredictionFunction----------------------------
extractPredictions <- function(ssm_fit){
  predictions <- data.frame(ssm_fit$ssm[[1]]$predicted) |>
    sf::st_as_sf() |>
    sf::st_transform(4326)
  
  coords <- sf::st_coordinates(predictions)
  
  predictions$lon <- coords[, 1]
  predictions$lat <- coords[, 2]
  
  new_data <- predictions |> 
    data.frame() |>
    dplyr::select(id, date, lon, lat)
  
  return(new_data)
}


## --------------------------------------------------------
myelk_regdata <- extractPredictions(myelk_ssm_fit)


## ----compareRegularizedToOriginal, fig.width = 8, fig.height = 4, echo = -1----
par(mar = c(3,2,1,1), mfrow = c(1,2))

myelk <- subset(elk_gps, id == "YL96") |> dplyr::mutate(id = as.character(id))

plot(lon~datetime, type = "o", pch = 19, data = myelk[1:100,])
lines(lon~date, type = "o", pch = 4, col = 2, data = myelk_regdata[1:100,])
legend("topright", legend = c("original", "regular"), pch = c(19,4), col = 1:2)

plot(lon~lat, type = "o", pch = 19, data = myelk[1:100,])
lines(lon~lat, type = "o", pch = 4, col = 2, data = myelk_regdata[1:100,])
legend("topright", legend = c("original", "regular"), pch = c(19,4), col = 1:2)


## ----eval= FALSE, include=FALSE, echo = FALSE------------
## #save(elk_regdata, file="./data/elk_regdata.rda")


## ----echo = -1-------------------------------------------
setwd("../../")
load(file="./data/elk_regdata.rda")


## --------------------------------------------------------
library(sf)


## ----echo = -1-------------------------------------------
setwd("../../")
load(file="./data/elk_regdata.rda")
myelk_utm <- elk_regdata |> subset(id == "YL96") |> 
  st_as_sf(coords = c("lon","lat"), crs = 4326) |>
  st_transform(32611)


## --------------------------------------------------------
myelk_utm <- cbind(myelk_utm, st_coordinates(myelk_utm)) |> data.frame()
head(myelk_utm)


## --------------------------------------------------------
library(momentuHMM)


## --------------------------------------------------------
myelk_hmm_prep <- prepData(myelk_utm |> data.frame(), type = "UTM",
                                    coordNames = c("X","Y")) 

head(myelk_hmm_prep)


## --------------------------------------------------------
table(myelk_hmm_prep$step == 0)


## --------------------------------------------------------
stepMean0 <- c(m1 = 100, m2 = 4000)
stepSD0 <- c(sd1 = 50, sd2 = 1000)
angleCon0 <- c(rho1  = 0.1, rho2 = 0.8)


## --------------------------------------------------------
stateNames <- c("resident","transit")


## --------------------------------------------------------
dist <- list(step = "gamma", angle = "wrpcauchy")
Par0 <- list(step=c(stepMean0, stepSD0), angle = c(angleCon0))


## --------------------------------------------------------
myelk_hmm_fit <- fitHMM(data = myelk_hmm_prep, nbStates = 2, dist = dist, 
                      Par0 = Par0, stateNames = stateNames)

print(myelk_hmm_fit)


## --------------------------------------------------------
hmm_states <- viterbi(myelk_hmm_fit)
str(hmm_states)


## --------------------------------------------------------
myelk_utm$state <- hmm_states


## ----DefaultTwoStatePlot, echo  = 2, eval = 1------------
plot(myelk_hmm_fit, ask = FALSE)
plot(myelk_hmm_fit)


## ----TwoState_ScanTtrack---------------------------------
layout(cbind(c(1,1),2:3))
par(bty = "l", mar = c(2,2,2,2))

with(myelk_utm, {
  plot(X, Y, asp =1, col = c("orange","blue")[state], pch = 19, cex = 0.7)
  segments(X[-length(X)], Y[-length(Y)], 
           X[-1], Y[-1], col = c("orange","blue")[state[-length(state)]])
  plot(date, X, col = c("orange","blue")[state], pch = 19, cex = 0.7)
  segments(date[-length(X)], Y[-length(Y)], 
           date[-1], Y[-1], col = c("orange","blue")[state[-length(state)]])
  plot(date, Y, col = c("orange","blue")[state], pch = 19, cex = 0.7)
  segments(date[-length(X)], Y[-length(Y)], 
           date[-1], Y[-1], col = c("orange","blue")[state[-length(state)]])
})


## --------------------------------------------------------
library(mapview)


## --------------------------------------------------------
myelk_sf <- myelk_utm |>
  st_as_sf(coords=c("X","Y"), crs= 32611) |>
  st_transform(4326) |>
  mutate(state = as.character(state))

myelk_track <- myelk_sf |>
  summarize(do_union=FALSE) |> 
  st_cast("LINESTRING")

mapview(myelk_track, color="darkgrey") +
  mapview(myelk_sf, zcol="state", col.regions=c("orange","blue"))


## ----Priors_ThreeState-----------------------------------
stepMean0 <- c(m1 = 50, m2 = 2000, m3 = 200)
stepSD0 <- c(sd1 = 50, sd2 = 1000, sd3 = 100)
angleCon0 <- c(rho1  = 0.1, rho2 = 0.8, rho3 = 0.2)


## --------------------------------------------------------
stateNames <- c("resident-slow","transit", "resident-faster")


## --------------------------------------------------------
dist <- list(step = "gamma", angle = "wrpcauchy")
Par0 <- list(step=c(stepMean0, stepSD0), angle = c(angleCon0))


## ----Fit_ThreeState--------------------------------------
myelk_hmm_threestate <- fitHMM(data = myelk_hmm_prep, nbStates = 3, 
                         dist = dist, Par0 = Par0, 
                         stateNames = stateNames, 
                         formula = ~1)


## --------------------------------------------------------
myelk_hmm_threestate


## --------------------------------------------------------
hmm_3states <- viterbi(myelk_hmm_threestate)
myelk_utm$state <- hmm_3states


## ----ThreeStateDefaultPlot, echo = 2, eval = 1-----------
plot(myelk_hmm_fit, ask = FALSE)
plot(myelk_hmm_fit)


## ----ThreeStateMapview-----------------------------------
myelk_track <- myelk_sf |>
  summarize(do_union=FALSE) |> 
  st_cast("LINESTRING")
myelk_sf <- myelk_sf |> mutate(state = hmm_3states)
mapview(myelk_track, color="darkgrey") +
  mapview(myelk_sf, zcol="state", col.regions=c("orange","blue", "green"))

