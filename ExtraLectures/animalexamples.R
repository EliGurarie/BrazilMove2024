
# Buffalo example
  require(ctmm)

  vignette("akde")

  data(buffalo, package = "ctmm")
  Pepper <- buffalo$Pepper
  plot(Pepper)
  vg <- variogram(Pepper)
  ctmm.guess(Pepper)
  ctmm::plot(vg)


  M.IID <- ctmm.fit(Pepper) # no autocorrelation timescales
  GUESS <- ctmm.guess(Pepper,interactive=FALSE) # automated model guess
  M.OUF <- ctmm.fit(Pepper,GUESS) # in general, use ctmm.select instead

  KDE <- akde(Pepper, M.IID) # KDE
  AKDE <- akde(Pepper, M.OUF) # AKDE



  par(mfrow = c(1,2))
  EXT <- extent(list(KDE, AKDE),level=0.95)
  EXT <- extent(list(KDE),level=0.95)

  plot(Pepper,UD=KDE,xlim=EXT$x,ylim=EXT$y)
  title(expression("IID KDE"["C"]))
  plot(Pepper,UD=AKDE,xlim=EXT$x,ylim=EXT$y)
  title(expression("OUF AKDE"["C"]))


# Tapir example


  require(sf)
  load("data/tapir.rda");
  plot(tapir[,"ID"], type = "o")
  require(ctmm)
  load("data/tapir_tm.rda")
  T3 <- tapir.tm[["Tapir2"]]
  head(T3)


  elieslides::pars()
  T3.vg <- variogram(T3)
  plot(T3.vg, main = "large scale")

  require(mapview)
  mapview(tapir, zcol = "ID")


  elieslides::pars()
  plot(T3.vg, xlim = c(0, 10 %#% "hour"), main = "small scale")

  T3.guess <- ctmm.guess(T3, interactive = FALSE)
  T3.fit <- ctmm.fit(T3, CTMM=T3.guess, #ctmm(tau = 13*3600),
                     method="pHREML",COV=FALSE,
                     control=list(),trace=FALSE)
  summary(T3.fit)
  T3.akde <- akde(T3, T3.fit)
  plot(T3, UD = T3.akde)
