library(tidyverse)
library(plotly)
library(astsa)
library(sf)
library(reshape2)
library(gridExtra)
library(transformr)
sf_use_s2(FALSE)
source("../../../utils/R/utils.R")
install()

# project wide code and file paths
source("./helpers.R")


#
# CHANGE THESE - strange note: wd working differently in .R vs .Rmd (one fewer ../)
#

verbose=TRUE
overwrite=TRUE
overwriteProjections = TRUE
scenarios = c("ssp126","ssp245","ssp585")
forcing = "ipsl-cm6a-lr"
#social="ssp1"
ecovs = c("temperature","humidity","rain")

#
#
#

# load the country geo data
regions = st_read("../../Brazil/Brazil_1.json")
regions$gid = regionNumber(regions$GID_1)
regions = sfinject(regions, sortdf, "gid")

# starting with baseline historical data
snakebites = read.csv(paste(dataFolder,"clean.csv",sep="/"))[,-1]
snakebites = sortdf(snakebites, c("ym","gid"))

# the regions have to be in the same order if we want to very easily copy geometry column
if(sum(unique(regions$GID_1) != unique(snakebites$GID_1)) > 0){
  info("Something has messed up the order of the regions")
  quit(save="no", status=1)
}else{
  info("the order of the geometry columns looks like it matches.")
}

times = unique(select(snakebites, c(year, month, ym)))
ntimes = nrow(times)
locs = unique(snakebites$GID_1)
nlocs = length(locs)

tcols = c("year", "month")
scols = c("GID_1")

# make into an sf for later
snakebites$geometry = rep(regions$geometry, times=ntimes)
snakebites = st_as_sf(snakebites)

# Load covariates


info("reading covariates")

covs = loadOrGenerate(paste(cacheFolder,"scenariocovs",sep="/"), function(){
  covs = list()
  for(scenario in scenarios){
    info("loading ",scenario)
    covspath = paste("../../ISIMIP/Python/csv/", scenario, "/", forcing, "/Brazil.csv", sep="")
    allcovs = read.csv(covspath)[,-1]
    allcovs = splitDates(allcovs, dateColname="time")
    gcovs = st_as_sf(allcovs, coords=c("lon","lat"))
    st_crs(gcovs) = st_crs(regions)

    covs[[scenario]] = gcovs
  }
  return(covs)
})



# load best covariate combinations
best = decache("bestmodel")

getBestCovariates = function(region){
  row = best[best$region == region,]
  covCols = setdiff(colnames(best), c("region","ar1"))

  bestCovs = c()
  for(covCol in covCols){
    if(row[[covCol]] == TRUE){
      bestCovs = c(bestCovs, covCol)
    }
  }

  #TODO:remove when we can
  # bypass this and include all covs instead
  #return(c("temperature","humidity","rain"))
  return(bestCovs)
}

futureBlank = loadOrGenerate(path("future",folder=cacheFolder),
    func=function(){
      # create the basic structure of the future data
      info("making future dataframe")
      endYear = max(covs[[1]]$year)
      # take num years from any gcovs
      ny = (endYear - max(snakebites$year))
      nt = 12 * ny # future months and years
      n = nlocs * nt
      future = data.frame(
        cases=rep(NA,n),
        gid=rep(locs, nt),
        year = rep((max(snakebites$year)+1):endYear, each=12*nlocs),
        month = rep(1:12, each=nlocs, times=ny)
      )

      # add the geometry column to future - checked that the order of gid is 1-27 repeating
      future$geometry = rep(regions[["geometry"]], nt)

      # add environmental covariates from the future
      future = uIndex(future, tcols, "ym")
      future = st_as_sf(future)

      # add columns to hold the predictions
      future$lcases = rep(NA, n)
      future$se = rep(NA, n)
      snakebites$lcases = log(snakebites$cases)

      return(future)
    }, overwrite=overwrite)


# to merge we need to remove the geometry from snakebites
snakebites = sfsuspend(snakebites)
# make future dataset
futures = list()

for(scenario in scenarios){
  future = futureBlank

  info("merging covariates - ", scenario)
  regcovs = groupGeom(regions, covs[[scenario]], scol=scols)
  future = addCovariates(future, regcovs, scols="gid", scols2=scols, crsOf=regions,
                         covcols=c("tas","huss","pr"),
                         renameCols=c(temperature="tas", humidity="huss", rain="pr"))
  # need to align the time index
  future$ym = future$ym + max(snakebites$ym)

  # remove the geometry, sf can't merge
  futuregeom = sfsuspend(future)
  future = sfsuspend(future)

  futures[[scenario]] = future
}

# Standardise all data sets by shared mean and SD
for(ecov in ecovs){
  # get all the future values
  vals = lapply(futures, FUN=function(df){return(df[[ecov]])})
  vals = melt(vals, value.name=ecov)
  # add in the historical temps
  vals = c(vals[[ecov]], snakebites[[ecov]])
  # get the expectations
  valmean = mean(vals)
  valsd = sd(vals)
  # scale all the futures and the past
  for(s in scenarios){
    info("before", mean(futures[[s]][[ecov]]))
    futures[[s]][[ecov]] = (futures[[s]][[ecov]] - valmean) / valsd
    info("after", mean(futures[[s]][[ecov]]))
  }
  snakebites[[ecov]] = (snakebites[[ecov]] - valmean) / valsd
}

# overwrite the snakebites with scaling
snakebites = sortdf(snakebites, c(tcols,scols))
write.csv(snakebites, "../data/cleanscaled.csv")
print("Data saved to cleanscaled.csv")

# make past and future columns match
snakebites$lcases = log(snakebites$cases)
snakebites$se = rep(0,nrow(snakebites))
snakebites$gid = snakebites$GID_1
snakebites = select(snakebites, -GID_1)

# going to need a fresh copy per model
info("Projecting into the future..")

for(scenario in scenarios){
  # start with the projected environmental covariates
  predFuture = futures[[scenario]]

  # use each model to predict the future cases
  # linfuture = loadOrGenerate(path(paste("linfuture_",scenario,sep=""), folder=cacheFolder),
  #                            func = function(){
  #                              # project using best
  #                              future = predFuture
  #                              info("making projections - linear model")
  #                              for(i in 1:nrow(regions)){
  #                                region = regions$GID_1[i]
  #                                info("projecting region ", region)
  #                                regionpast = filter(snakebites, gid==region)
  #                                regionfuture = filter(future, gid==region)
  #
  #                                covs = getBestCovariates(region)
  #
  #                                regionFit = lm(formula=form(covariates=covs), data=regionpast)
  #                                regionPredictions = predict(regionFit, newdata=regionfuture)
  #
  #                                regionfuture$lcases = log(regionPredictions-1) # make comparable
  #                                regionfuture$cases = regionPredictions
  #                                regionfuture$se = rep(0, length(regionPredictions))
  #
  #                                future = applyLookupv(future, c("ym","gid"), regionfuture,
  #                                                      colsToTransfer=c("lcases", "cases", "se"))
  #                              }
  #                              return(future)
  #                            }, overwrite=overwriteProjections)


  # info("Sarima")
  # sfuture = loadOrGenerate(path(paste("sfuture_",scenario,sep=""), folder=cacheFolder),
  #                          func = function(){
  #                            # project using best
  #                            future = predFuture
  #                            info("making projections - sarima")
  #                            for(i in 1:nrow(regions)){
  #                              region = regions$GID_1[i]
  #                              info("projecting region ", region)
  #                              regionpast = filter(snakebites, gid==region)
  #                              regionfuture = filter(future, gid==region)
  #
  #                              covs = getBestCovariates(region)
  #                              useAr1 = FALSE
  #                              regionPredictions = predictSarima_inla(i, regionpast, regionfuture,
  #                                                                covariates=covs, addAR1=useAr1, showPlots = F)
  #
  #                              regionfuture$lcases = regionPredictions$pred
  #                              regionfuture$cases = exp(regionfuture$lcases) - 1
  #                              regionfuture$se = regionPredictions$se
  #
  #                              future = applyLookupv(future, c("ym","gid"), regionfuture,
  #                                                    colsToTransfer=c("lcases", "cases", "se"))
  #                            }
  #                            return(future)
  #                          }, overwrite=overwriteProjections)


  # info("SARIMA+AR")
  # sarfuture = loadOrGenerate(path(paste("sarfuture_",scenario,sep=""), folder=cacheFolder),
  #                            func = function(){
  #                              # project using best
  #                              future = predFuture
  #                              info("making projections - sarima+AR")
  #                              for(i in 1:nrow(regions)){
  #                                region = regions$GID_1[i]
  #                                info("projecting region ", region)
  #                                regionpast = filter(snakebites, gid==region)
  #                                regionfuture = filter(future, gid==region)
  #
  #                                covs = getBestCovariates(region)
  #                                useAr1 = TRUE#best[best$region == region,]$ar1
  #                                regionPredictions = predictSarima_inla(i, regionpast, regionfuture,
  #                                                                  covariates=covs, addAR1=useAr1, showPlots = F)
  #
  #                                regionfuture$lcases = regionPredictions$pred
  #                                regionfuture$cases = exp(regionfuture$lcases) - 1
  #                                regionfuture$se = regionPredictions$se
  #
  #                                future = applyLookupv(future, c("ym","gid"), regionfuture,
  #                                                      colsToTransfer=c("lcases", "cases", "se"))
  #                              }
  #                              return(future)
  #                            }, overwrite=overwriteProjections)

  info("Best Projections")
  sarfuture = loadOrGenerate(path(scenario, folder=cacheFolder),
                             func = function(){
                               # project using best
                               future = predFuture
                               info("making projections - sarima+AR")
                               for(i in 1:nrow(regions)){
                                 region = regions$GID_1[i]
                                 info("projecting region ", region)
                                 regionpast = filter(snakebites, gid==region)
                                 regionfuture = filter(future, gid==region)

                                 covs = getBestCovariates(region)
                                 useAr1 = best[best$region == region,]$ar1
                                 regionPredictions = predictSarima_inla(i, regionpast, regionfuture,
                                                                        covariates=covs, addAR1=useAr1, showPlots = F)

                                 regionfuture$lcases = regionPredictions$pred
                                 regionfuture$cases = exp(regionfuture$lcases) - 1
                                 regionfuture$se = regionPredictions$se

                                 future = applyLookupv(future, c("ym","gid"), regionfuture,
                                                       colsToTransfer=c("lcases", "cases", "se"))
                               }
                               return(future)
                             }, overwrite=overwriteProjections)
  info(".done")


  allprojimg = ggplot(sarfuture) +
    geom_line(aes(x=ym, y=lcases), color="steelblue") +
    geom_ribbon(aes(x=ym, ymin=(lcases-se), ymax=(lcases+se)),color="gray", alpha=.25) +
    facet_wrap(vars(gid)) +
    theme_bw()
  plot(allprojimg)

  saveimg(allprojimg, pltname=paste("projections",scenario,sep="_"))
  ggplotly(allprojimg)
}


