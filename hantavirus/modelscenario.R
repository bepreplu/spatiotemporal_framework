library(tidyverse)
library(inlabru)
library(sf)
library(INLA)
library(INLAspacetime)

source("../../../utils/R/utils.R")
unifyWD()
source("helpers.R")


#
# CHANGE THESE - strange note: wd working differently in .R vs .Rmd (one fewer ../)
#

verbose=TRUE
overwrite=FALSE
usePredict=TRUE
scenarios=c("ssp126","ssp245","ssp585")
#social="ssp1"
forcing = "ipsl-cm6a-lr"
cachedModel = "inlapoisST"

#
# Prep the data
#

dataFolder = "../data"
popDataFolder = "../../geostats/ISIMIP/Python/csv/pop/Sweden/"
cacheFolder = "../cache"
outFolder = "../projections"
png=".png"

# Load covariates
info("reading covariates")
covariates = loadCovariates()
#pop = read.csv(path(social, folder=popDataFolder, extension = "csv"))[,-1]

#gcovs = st_as_sf(allcovs, coords=c("lon","lat"))
#gpop = st_as_sf(pop, coords=c("lon","lat"))

#
# Load case data
#
info("loading case data")
hanta = read.csv(path("clean", folder=dataFolder, extension=".csv"))[,-1]
sweden = decache("sweden")

# get our unique st coordinates
scols = c("long","lat")
tcols = c("year","month")

locs = unique(select(hanta, all_of(scols)))
times = unique(select(hanta, all_of(tcols)))

nlocs = nrow(locs)
ntimes = nrow(times)

# convert to simple features
hanta = sortdf(hanta, c(tcols,scols))
hanta = tosf(hanta, coords=scols)
hanta$ym2 = hanta$ym # silly index replication for rbind


# fit the best model
model = decache(cachedModel)
# loadOrGenerate(file=path("fittedmodel", folder=cacheFolder), func=function(){
#   print("Fitting Model")
#   sfit = fitbestmodel(hanta)
#   return(sfit)
# }, overwrite=overwrite)

#info("model has been fit to past data")

#
# Generate future data AND Projections
#
info("Projecting into the future..")

for(scenario in scenarios){
  info(scenario)
  futureProjection = loadOrGenerate(path(paste("pred_",scenario,sep=""), folder=cacheFolder),
    func = function(){

      future = loadOrGenerate(path(paste("future_",scenario,sep=""), folder=cacheFolder), func=function(){
        # get the covariates for this scenario
        covs = covariates$data[[scenario]]
        covs = tosf(covs, coords=c("long","lat"))
        # create the basic structure of the future data
        info("making future dataframe")
        ny = (max(covs$year - max(hanta$year)))
        nt = 12 * ny # future months and years
        n = nlocs * nt
        future = data.frame(
          cases=rep(NA,n),
          long = rep(locs$long, nt),
          lat = rep(locs$lat, nt),
          year = rep((max(hanta$year)+1):max(covs$year), each=12*nlocs),
          month = rep(1:12, each=nlocs, times=ny)
        )

        # make sure we have the necessary indexes for the model definition
        future = uIndex(future, tcols, "ym")
        future$ym2 = future$ym
        future = st_as_sf(future, coords=scols)
        st_crs(future) = st_crs(sweden)

        # add environmental covariates from the future
        info("merging covariates")
        covcols = c("tas","huss","pr","prsn")
        future = alignGeom(future, covs)
        # use the aligned lat and long added by align geom to transfer covariate data
        # note: converting covs changes the scols to lat and long for ease
        future = addCovariates(future, dropSF(covs), crsOf=sweden)
        # drop the clamped coordinates
        future = sfinject(future, sortdf, c(tcols,scols))
        future = select(future, -scols)
        return(future)
      },overwrite=overwrite)


      info("making projections")

      ## Using inla()
      if(usePredict){
        info("Projecting using INLABru")
        projdata = future
        #projdata = filter(projdata, year <= 2050) # remove this ideally
        results = predict(model, newdata=projdata, formula=bestformula, n.samples=50)
        results$cases = exp(results$mean)
      } else{
        info("Projecting using INLA")
        hantasubset = select(hanta, all_of(c(colnames(future),"cases")))
        projdata = rbind(hantasubset, future)

        projdata = filter(projdata, year <= 2050) # remove this ideally
        results = fitbestmodel(projdata)
      }

      # some diagnostics
      # plots()
      #
      # info("range of risk for ", scenario, " ", min(tfuture$risk), " - ", max(tfuture$risk))
      # info("range of risk for ", scenario, " ", min(future$cases), " - ", max(future$cases))

      return(results)
    }, overwrite=T) # if you aren't doing this fresh, then what's the point?
}
info(".done")


