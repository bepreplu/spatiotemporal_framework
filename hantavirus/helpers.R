# projection system for this project
# ED50 / UTM zone 32N
epsgnum = "23032"
epsg=paste("+init=epsg:", epsgnum, sep="")
wgs84=4326
nullCRS = st_crs(NULL)


# developed model
bestformula = cases ~ Intercept + humidity + rain + temperature + snowflux + S

fitbestmodel = function(data){
  coords = unique(st_coordinates(data))
  mesh = inla.mesh.2d((loc=coords), crs = st_crs(sweden))
  spde = inla.spde2.matern(mesh=mesh)

  data$ym2 = data$ym
  # make sure this matches the bestformula
  # spatial evolves with ar1 pattern/function    separable
  stmodel = cases ~ Intercept(1) + humidity + rain + temperature + snowflux +
    S(geometry, model=spde, group=ym, control.group=list(model="ar1"))


  sfit = bru(stmodel, family="poisson", data=data,
             options=list(control.compute = list(waic = TRUE, cpo = FALSE),
                          control.inla = list(int.strategy = "eb"),
                          verbose = F))
  return(sfit)
}

tosf = function(df, coords){
  iswgs84 = min( df[,coords[1]] ) < 100

  if(st_crs(df) == nullCRS){
    # not already projected, convert to sf
    df = st_as_sf(df, coords=coords)

    if(iswgs84){
      # otherwise, perform the transformation
      st_crs(df) = wgs84
      df = st_transform(df, epsg)
    }else{
      # should have been already projected
      st_crs(df) = epsg
    }
  }

  return(df)
}


# quickly test variations
testfit = function(data, modelFormula, family="poisson", ciReps=10){
  results = list()

  # make the matern spatial term
  coords = unique(st_coordinates(data))
  mesh = inla.mesh.2d((loc=coords), crs = st_crs(sweden)) # can error
  spde = inla.spde2.matern(mesh=mesh)


  fit = bru(modelFormula, family="poisson", data=data,
            options=list(control.compute = list(waic = TRUE, cpo = FALSE),
                         control.inla = list(int.strategy = "eb"),
                         verbose = F))

  fit$resid = fit$residuals$deviance.residuals
  data$resid = fit$resid

  cgrams = stoverview(data, form=resid~1, ciReps=10,
                      spaceCols=scol, timeCols=tcol,
                      spaceSummaryFunction=mean, timeSummaryFunction=mean)

  results$fit = fit
  results$vario = cgrams$vario
  results$acf = cgrams$acf

  return(results)
}




loadScaledCovariates = function(){
  info("loading covariates")
  covariates = list(data=list(), metrics=list())
  covmetrics = list()

  covcols = c("tas","huss","pr","prsn")

  scenarios = c("hist-nat","ssp126","ssp245","ssp585")
  forcing = "ipsl-cm6a-lr"

  for(scenario in scenarios){
    info("loading ", scenario)
    covspath = paste("../../ISIMIP/Python/csv/", scenario, "/", forcing, "/Sweden.csv", sep="")
    covs = read.csv(covspath)[,-1]
    covs = splitDates(covs, dateColname="time")
    # convert to correct crs
    covs = tosf(covs, c("lon","lat"))
    # add it to the list
    covariates$data[[scenario]] = dropSF(covs)
  }

  # calculate some metrics for possible scaling
  info("Calculating metrics")
  for(cov in covcols){
    # first get a vector of all scenarios per covariates
    covVector = c()
    for(scenario in scenarios){
      covVector = c(covVector,covariates$data[[scenario]][[cov]])
    }
    # now calculate the metrics for each covariate
    covariates$metrics[[cov]]$min = min(covVector)
    covariates$metrics[[cov]]$max = max(covVector)
    covariates$metrics[[cov]]$mean = mean(covVector)
    covariates$metrics[[cov]]$sd = sd(covVector)
    covariates$metrics[[cov]]$total = sum(covVector)
    covariates$metrics[[cov]]$n = length(covVector)
  }

  # finally we can apply scaling
  info("scaling")

  for(scenario in scenarios){
    for(cov in covcols){
      # lets go with z score
      covariates$data[[scenario]][[cov]] = (covariates$data[[scenario]][[cov]] -
                                            covariates$metrics[[cov]]$mean) /
                                            covariates$metrics[[cov]]$sd
    }
  }

  info("covariates loaded")
  return(covariates)
}

loadCovariates = function(recalculate=FALSE){
  covariates = loadOrGenerate("scaledCovariates", loadScaledCovariates, overwrite=recalculate)
  return(covariates)
}









###
# Helper functions
###

plotResids = function(df, colname){
  dft = groupSummary(df, "ym", summariseBy=colname,
                     summaryFunction=mean)

  x = dft$ym
  y = dft[[colname]]
  plt = ggplot() + geom_point(aes(x=x, y=y), color="steelblue") + xlab("time") +
    ylab("residuals") + theme_bw()
  return(plt)
}


# do the same with snakebites , making sure to take avg temperature across regions, all points
sproject = function(cases, year, month, ym, fittedmodel, covariates, region, dims){
  coords = unique(st_coordinates(cases))
  mesh = inla.mesh.2d((loc=coords), crs = st_crs(region))
  basemap = fm_pixels(mesh, mask = region, format = "sp", dims=dims)

  # make a blank slice raster
  blank = blankdf(c(), geom=basemap)
  st_crs(blank) = st_crs(region)
  n = nrow(blank)

  # fill it with data from a target month (year-month)
  slide = data.frame(blank)
  slide$month = rep(month, n)
  slide$year = rep(year, n)
  slide$ym = rep(ym, n)
  slide = st_as_sf(slide)

  slide = alignGeom(slide, covariates)
  covdf = dropSF(covariates)
  slide = addCovariates(slide, covdf, crsOf=sweden)

  # now project cases

  # project the data for this month
  preds = predict(fittedmodel, newdata=slide, formula=fittedmodel$sform, n.samples=1)
  predraster = st_as_sf(preds)
  st_crs(predraster) = epsg
  predraster$risk = predraster$mean # exp gives a barely readable map

  return(predraster)
}

plotym = function(row){
  ympred = sproject(future, row$year, row$month, model, sweden)

  # make it look good
  plt = ggplot() + geom_sf(data=ympred,aes(color=risk), shape=15, size=1) +
    #scale_color_gradientn(limits = c(min(ympred$cases), max(ympred$cases)), colors = c("seagreen3", "goldenrod3", "darkorange3", "firebrick3")) +
    scale_color_gradientn(limits = c(-7,0), colors = c("seagreen3", "goldenrod3", "darkorange3", "firebrick3")) +
    ggtitle(paste(ympred$year[1], "-", ympred$month[1]))
  plot(plt)

  return(plt)
}

#
#
#

plots = function(){

  # collapse to have only a temporal element.
  tfuture = groupSummary(future, tcols, c("cases", "mean", "q0.025","q0.975"))
  tfuture = st_drop_geometry(tfuture)
  tfuture = uIndex(tfuture, tcols, "ym")
  tfuture$risk = tfuture$cases
  tfuture$cases = exp(tfuture$cases)

  # pick out some interesting points to plot fully
  minrow = tfuture[which.min(tfuture$cases),]
  maxrow = tfuture[which.max(tfuture$cases),]

  # plot across all time
  ggplotly(ggplot(tfuture) +
             geom_line(aes(x=ym,y=risk), color="steelblue3") +
             geom_ribbon(aes(x=ym, ymin=q0.025, ymax=q0.975), alpha=0.25) +
             geom_vline(xintercept= which(tfuture$ym%%12==0), color="red4", alpha=.1) +
             geom_vline(xintercept = minrow$ym, color="salmon", alpha=.8) +
             geom_vline(xintercept = maxrow$ym, color="salmon4", alpha=.8) +
             ylim(-7,0) +
             theme_bw())
  ggsave(path("alltime", folder=outFolder, extension=png))

  # plot each month separately
  ggplot(tfuture, aes(x=ym,y=risk)) +
    geom_line(color="steelblue3") +
    facet_wrap(vars(month)) +
    geom_smooth(color="salmon") +
    ylim(-5,-4) +
    theme_bw()
  ggsave(path("bymonth", folder=outFolder, extension=png))



  plotym(minrow)
  ggsave(path("mintime", folder=outFolder, extension=png))
  plotym(maxrow)
  ggsave(path("maxtime", folder=outFolder, extension=png))
}
