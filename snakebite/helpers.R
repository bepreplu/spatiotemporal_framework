# Some folder definitions
dataFolder = "../data"
mapFolder = "../../Brazil"
modelFolder = "modelsR"
eFolder = "../../../Es code"
imageFolder = "../images"
cacheFolder = "../cache"
animationFolder = "../animation"


library("tseries")
library("forecast")
library("inlatools")


#
# sarima methods
#

sarimaLibrary = "INLA"

fitSarima = function(data, region=NULL, addAR1 = FALSE, showPlot=TRUE,
                     covariates=c("temperature","humidity","rain")){

  switch(sarimaLibrary,
         "INLA" = {
           return(fitSarima_inla(data,region,addAR1,showPlot,covariates))
         },
         "astsa" = {
           return(fitSarima_astsa(data,region,addAR1,showPlot,covariates))
         },
         "sarima" = {
           return(fitSarima_sarima(data,region,addAR1,showPlot,covariates))
         },
         "forecast" = {
           return(fitSarima_forecast(data,region,addAR1,showPlot,covariates))
         })


}



predictSarima = function(region, regionpast, regionfuture,
                         covariates=c("temperature","humidity","rain"),
                         addAR1 = FALSE,
                         showPlots=FALSE){
  switch(sarimaLibrary,
        "INLA" = {
          return(predictSarima_inla(region,regionpast,regionfuture,covariates,addAR1,showPlots))
        },
        "astsa" = {
          return(predictSarima_astsa(region,regionpast,regionfuture,covariates,addAR1,showPlots))
        },
        "sarima" = {
          return(predictSarima_sarima(region,regionpast,regionfuture,covariates,addAR1,showPlots))
        },
        "forecast" = {
          return(predictSarima_forecast(region,regionpast,regionfuture,covariates,addAR1,showPlots))
        })
}

#
#
#
#   tseries and forecast
#
#
#

fitSarima_forecast = function(data, region=NULL, addAR1 = FALSE, showPlot=TRUE,
                     covariates=c("temperature","humidity","rain")){
  # assume the data has already been filtered by region
  sbites = data
  # if one is specified then filter the data by region here
  if(isset(region)){
    if(is.numeric(region)){
      sbites = filter(data, gid==region)
    }
    else{
      sbites = filter(data, gid==regionNumber(region))
    }
  }
  sbites$lcases = log(1 + sbites$cases) # note this somewhere, equal shift up for zeroes. zeroes not impossible snakebites, too few and rare to report.
  ntimes = nrow(sbites)


  sfit = forecast::Arima(sbites$lcases, xreg=as.matrix(select(sbites, all_of(covariates))),
                         order=c(as.numeric(addAR1),0,0),
                         seasonal=list(order=c(1,0,0), period=12),
                         method="ML")

  # note the data and flag used
  sfit$addAR1 <- addAR1
  sfit$data <- sbites
  sfit$resid = sfit$fit$residuals
  # Plot if necessary, but save the acf either way
  title = paste("Region",region)
  sfit$acf = acf(sfit$resid, na.action=na.omit, main=title, plot=showPlot)
  return(sfit)
}



predictSarima_forecast = function(region, regionpast, regionfuture,
                         covariates=c("temperature","humidity","rain"),
                         addAR1 = FALSE,
                         showPlots=FALSE){

  regionpast$lcases = log(1 + regionpast$cases) # note this somewhere, equal shift up for zeroes. zeroes not impossible snakebites, too few and rare to report.

  mPast = as.matrix(select(regionpast, all_of(covariates)))
  mFuture = as.matrix(select(regionfuture, all_of(covariates)))
  sfit = forecast::Arima(regionpast$lcases, xreg=mPast,
                         order=c(as.numeric(addAR1),0,0),
                         seasonal=list(order=c(1,0,0), period=12),
                         method="ML")

  pred = forecast::forecast(sfit, xreg=mFuture)
  pred$pred = as.numeric(pred$mean)
  pred$se = as.numeric(pred$se)

  return(pred)
}



#
#
#
#   INLA
#
#
#

fitSarima_inla = function(data, region=NULL, addAR1 = FALSE, showPlot=TRUE,
                     covariates=c("temperature","humidity","rain")){
  # assume the data has already been filtered by region
  sbites = data
  # if one is specified then filter the data by region here
  if(isset(region)){
    if(is.numeric(region)){
      sbites = filter(data, gid==region)
    }
    else{
      sbites = filter(data, gid==regionNumber(region))
    }
  }
  sbites$lcases = 1 + sbites$cases # here we avoid zero, but leave log to the link function
  ntimes = nrow(sbites)


  model = form("lcases", c("1", covariates, "f(ym2, model='seasonal', season.length=12)"))
  if(addAR1){
    model = form("lcases", c("1", covariates, "f(ym, model='ar1')", "f(ym2, model='seasonal', season.length=12)"))
  }

  sbites$ym2 = sbites$ym
  sfit = inla(model, data=sbites, family="poisson",
              control.predictor=list(compute=TRUE),
              control.compute=list(dic=1),
              verbose = F)

  # residuals are difference between fitted and observed?
  obs = sbites$lcases - 1 # removing that offset from earlier
  res = obs - fitted(sfit)

  # note the data and flag used
  sfit$addAR1 <- addAR1
  sfit$data <- sbites
  sfit$resid = res
  # Plot if necessary, but save the acf either way
  title = paste("Region",region)
  sfit$acf = acf(sfit$resid, na.action=na.omit, main=title, plot=showPlot)
  return(sfit)
}



predictSarima_inla = function(region, regionpast, regionfuture,
                         covariates=c("temperature","humidity","rain"),
                         addAR1 = FALSE,
                         showPlots=FALSE){
  # set up the data with appropriate indexes and log response
  regionpast$lcases = 1 + regionpast$cases # here we avoid zero, but leave log to the link function
  regionfuture$lcases = rep(NA, nrow(regionfuture))
  df = rbind(regionpast, regionfuture)
  df$ym2 = df$ym
  joinAt = nrow(df) - nrow(regionfuture) + 1

  # define model
  model = form("lcases", c("1", covariates, "f(ym2, model='seasonal', season.length=12)"))
  if(isset(addAR1)){
    model = form("lcases", c("1", covariates, "f(ym, model='ar1')",
                             "f(ym2, model='seasonal', season.length=12)"))
  }
  sfit = inla(model, data=df,
              family="poisson",
              control.predictor=list(link=NA,compute=TRUE))
  projections = sfit$summary.fitted.values[joinAt:nrow(df),]
  pred = list(pred = projections$mean,
              se = rep(0,length(projections$mean)), # blank out for now
              sd = projections$summary.fitted.values$sd,
              lb = projections$summary.fitted.values$'0.025quant',
              ub = projections$summary.fitted.values$'0.975quant')

  return(pred)
}


#
#
#
#   sarima library
#
#
#
#

fitSarima_sarima = function(data, region=NULL, addAR1 = FALSE, showPlot=TRUE,
                     covariates=c("temperature","humidity","rain")){
  # assume the data has already been filtered by region
  sbites = data
  # if one is specified then filter the data by region here
  if(isset(region)){
    if(is.numeric(region)){
      sbites = filter(data, gid==region)
    }
    else{
      sbites = filter(data, gid==regionNumber(region))
    }
  }
  sbites$lcases = log(1 + sbites$cases) # note this somewhere, equal shift up for zeroes. zeroes not impossible snakebites, too few and rare to report.
  ntimes = nrow(sbites)


  sarparts = c("sar(12,1)")
  if(addAR1){
    sarparts = c(sarparts, "ar(1)")
  }

  smodel = form(response="lcases", covariates=c(covariates), given=sarparts)
  sfit = sarima::sarima(model = smodel, data=sbites)

  # note the data and flag used
  sfit$addAR1 <- addAR1
  sfit$data <- sbites
  sfit$resid = sfit$fit$residuals
  # Plot if necessary, but save the acf either way
  title = paste("Region",region)
  sfit$acf = acf(sfit$resid, na.action=na.omit, main=title, plot=showPlot)
  return(sfit)
}



predictSarima_sarima = function(region, regionpast, regionfuture,
                         covariates=c("temperature","humidity","rain"),
                         addAR1 = FALSE,
                         showPlots=FALSE){

  regionpast$lcases = log(1 + regionpast$cases) # note this somewhere, equal shift up for zeroes. zeroes not impossible snakebites, too few and rare to report.
  ntimes = nrow(regionpast)


  sarparts = c("sar(12,1)")
  if(addAR1){
    sarparts = c(sarparts, "ar(1)")
  }

  smodel = form(response="lcases", covariates=c(covariates), given=sarparts)
  sfit = sarima::sarima(model = smodel, data=regionpast)

  xregs = as.matrix(select(regionfuture, all_of(covariates)))

  pred = sarima::fun.forecast(past=regionpast$lcases, n=nrow(regionfuture),
                       eps=NULL, pasteps=0,# ignore these 2
                       sarima::as.SarimaModel(sfit))

  return(pred)
}

#
#
#
# astsa library
#
#
#
#

fitSarima_astsa = function(data, region=NULL, addAR1 = FALSE, showPlot=TRUE,
                     covariates=c("temperature","humidity","rain")){
  # assume the data has already been filtered by region
  sbites = data
  # if one is specified then filter the data by region here
  if(isset(region)){
    if(is.numeric(region)){
      sbites = filter(data, gid==region)
    }
    else{
      sbites = filter(data, gid==regionNumber(region))
    }
  }
  sbites$lcases = log(1 + sbites$cases) # note this somewhere, equal shift up for zeroes. zeroes not impossible snakebites, too few and rare to report.
  ntimes = nrow(sbites)

  # handle case where best model has no covariates
  sfit = NULL
  if(isset(covariates)){
    D = as.matrix(dplyr::select(sbites, all_of(covariates)))
    sfit = sarima(xdata = sbites$lcases,
                  p = as.numeric(addAR1), d = 0, q = 0,
                  P = 1, D = 0, Q = 0, S = 12,
                  xreg = D,
                  details = FALSE)
  }
  else{
    sfit = sarima(xdata = sbites$lcases,
                  p = as.numeric(addAR1), d = 0, q = 0,
                  P = 1, D = 0, Q = 0, S = 12,
                  details = FALSE)
  }

  # note the data and flag used
  sfit$addAR1 <- addAR1
  sfit$data <- sbites
  sfit$resid = sfit$fit$residuals
  # Plot if necessary, but save the acf either way
  title = paste("Region",region)
  sfit$acf = acf(sfit$resid, na.action=na.omit, main=title, plot=showPlot)
  return(sfit)
}



predictSarima_astsa = function(region, regionpast, regionfuture,
                         covariates=c("temperature","humidity","rain"),
                         addAR1 = FALSE,
                         showPlots=FALSE){
  # well, we can't have neg inf from log zero cases. Need to decide what to do
  # are these zeroes low chance of cases, or impossible. go with former.
  # add one to all cases

  # Predict using sarima.for - make sure the specification is identical to fitSarima
  Dpast = as.matrix(dplyr::select(regionpast, all_of(covariates)))
  Dfuture = as.matrix(dplyr::select(regionfuture, all_of(covariates)))
  DObserved = log(1+regionpast$cases)

  pred = sarima.for(DObserved, nrow(regionfuture),
                    p = as.numeric(addAR1), d = 0, q = 0,
                    P = 1, D = 0, Q = 0, S = 12,
                    xreg = Dpast, newxreg = Dfuture, plot=showPlots,
                    method="ML")


  return(pred)
}

#
#
#


regionNumber = function(charlist){
  return(as.integer(str_match(charlist, "BRA\\.([0-9]+)_1")[,2]))
}


checkRegion = function(areaID, model=baseform){
  areaName = locs$NAME_1[areaID]

  regone = filter(snakebites, gid==areaID)
  cases = plot(ggplot(regone, aes(x=ym, y=cases)) +
                 geom_line(color="steelblue3") +
                 xlab("month") +
                 ggtitle(paste("Cases per month in region ", areaName)) +
                 theme_bw())

  covonly = lm(model, data=regone)
  covonlyresid = covonly$residuals

  acfcases = ggacf(regone$cases) +
    ggtitle(paste("ACF plot of cases in region", areaName))
  plot(acfcases)
  before = acfUV(regone$cases)

  acfresid = ggacf(covonlyresid) +
    ggtitle(paste("ACF plot of base model residuals in region", areaName))
  plot(acfresid)
  after = acfUV(covonlyresid)


  plots = list(caseplt=cases,
               caseacfplt=acfcases,
               residacfplt=acfresid,
               before=before,
               after=after)
  return(plots)
}

performance = function(data, regions, columns, metric, ...){
  # uses the default character followed by doubles
  perf = emptyframe(c("region", columns))
  numcols = ncol(perf)

  for(reg in regions){
    regiondata = data[data[[scols]]==reg,]
    row = metric(reg, regiondata, ...)
    # enforces the character followed by doubles
    perf[nrow(perf)+1,1] = reg
    perf[nrow(perf),2:(length(columns)+1)] = row
  }
  return(perf)
}

regionNameLookup = function(regionString, locs, nameCol = "NAME_1", regidCol = "GID_1"){
  name = NULL

  if(length(regionString) == 1){
    row = locs[locs[[regidCol]]==regionString,]
    name = row[[nameCol]]
  } else{
    name = c()
    for(reg in regionString){
      row = locs[locs[[regidCol]]==reg,]
      name = c(name, row[[nameCol]])
    }
  }
  return(name)
}

regionRename = function(df, regCol="region"){
  nameCol = "NAME_1"
  both = applyLookupv(df, regCol, locs, "GID_1", colsToTransfer=nameCol)
  df[[regCol]] = both[[nameCol]]
  return(df)
}
