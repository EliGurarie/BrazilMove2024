

## ----echo = -1----------------------------------------------------------------------------------
load("./data/elk_processed.rda")


## -----------------------------------------------------------------------------------------------
myelk <- subset(elk_gps, id == "YL96") |>
  dplyr::mutate(id = as.character(id))

str(myelk)


## -----------------------------------------------------------------------------------------------
par(mar = c(0,4,0,0), oma = c(4,0,5,2), xpd=NA)
layout(rbind(c(1,2), c(1,3)))
with(myelk, {
  plot(lon, lat, asp = 1, type="o")
  plot(datetime, lon, type="o")
  plot(datetime, lat, type="o")
  title(id[1], outer = TRUE)
})


## -----------------------------------------------------------------------------------------------
require(adehabitatLT)


## -----------------------------------------------------------------------------------------------
elk_lavielle_example <- lavielle(myelk$lat, Lmin = 20, Kmax = 10, type = "mean")


## -----------------------------------------------------------------------------------------------
chooseseg(elk_lavielle_example)


## -----------------------------------------------------------------------------------------------
elk_lavielle_example_path <- findpath(elk_lavielle_example, 7)


## -----------------------------------------------------------------------------------------------
breakpoints <- do.call(rbind, elk_lavielle_example_path)
breakpoints


## ----AugmentDataWithPhase-----------------------------------------------------------------------
require(plyr)
cuts <- c(breakpoints[,1]-.5, nrow(myelk))
myelk <- myelk |>
  mutate(Row_ID = 1:nrow(myelk),
         phase = cut(Row_ID,
                     breaks = cuts,
                     labels = 1:nrow(breakpoints)))


## ----PhaseWithLatitude--------------------------------------------------------------------------
require(ggplot2)
ggplot(data=myelk, aes(x=datetime, y=lat)) +
  geom_path(col = "grey") +
  geom_point(aes(color = phase)) +
  theme_classic()


## ----mapWithPhase-------------------------------------------------------------------------------
require(ggplot2)
ggplot(data=myelk, aes(x=lon, y=lat)) +
  geom_path(col = "grey") +
  geom_point(aes(color = phase)) +
  theme_classic()


## -----------------------------------------------------------------------------------------------
require(sf)
require(dplyr)

elk_df <- elk_gps |>
  data.frame(elk_sf |> st_transform(32611) |> st_coordinates()) |>
  group_by(id) |>
  mutate(
      Z = X + 1i*Y,
    	Step = c(NA, diff(Z)),
    	StepLength = Mod(Step),
    	dTime = c(NA, difftime(datetime[-1],
                           	datetime[-length(datetime)],
                           	units = "hours")),
    	Speed = StepLength/dTime)

myelk <- subset(elk_df, id == "YL96")


## -----------------------------------------------------------------------------------------------
with(myelk,
  plot(datetime, Speed, type = "l"))


## -----------------------------------------------------------------------------------------------
qqnorm(myelk$Speed)


## -----------------------------------------------------------------------------------------------
hist(log(myelk$Speed))


## -----------------------------------------------------------------------------------------------
with(myelk, plot(datetime, log(Speed), type = "l"))


## -----------------------------------------------------------------------------------------------
myelk_subset <- myelk[1000:1600,]
ggplot(myelk_subset, aes(X,Y)) + geom_path()

LogSpeed <- log(myelk_subset$Speed)[-1]
myelk_speed_lv <-  lavielle(LogSpeed, Lmin = 24, Kmax = 20, type = "mean")


## -----------------------------------------------------------------------------------------------
chooseseg(myelk_speed_lv)


## -----------------------------------------------------------------------------------------------
elk_segments <- findpath(myelk_speed_lv, 10)


## -----------------------------------------------------------------------------------------------
breakpoints <- do.call(rbind, elk_segments)
breakpoints


## ----AugmentDataWithBreakpoints-----------------------------------------------------------------
require(plyr)
cuts <- c(breakpoints[,1]-.5, nrow(myelk))
myelk_subset <- myelk_subset |>
  mutate(Row_ID = 1:nrow(myelk_subset),
         speedphase = cut(Row_ID,
                     breaks = cuts,
                     labels = 1:nrow(breakpoints)))


## ----plotSpeedPhase-----------------------------------------------------------------------------
require(ggplot2)
ggplot(data=myelk_subset, aes(x=datetime, y=Speed)) +
  geom_path(col = "grey") +
  geom_point(aes(color = speedphase)) +
  theme_classic()


## ----mapWithSpeedPhase--------------------------------------------------------------------------
require(ggplot2)
ggplot(data=myelk_subset, aes(x=lon, y=lat)) +
  geom_path(col = "grey") +
  geom_point(aes(color = speedphase)) +
  theme_classic()


## -----------------------------------------------------------------------------------------------
myelk_subset |> group_by(speedphase) |>
  dplyr::summarize(start = min(datetime),
            end = max(datetime),
            Speed.mean = mean(Speed),
            Speed.sd = sd(Speed))

