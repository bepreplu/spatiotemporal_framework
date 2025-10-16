#' @title Utilities
#' @description A globally available collection of utilities for use by the
#' various projects.
#'
#'

###########
# Imports #
###########

# general library imports
libs = c("tidyverse",
         "sf",
         "stars",
         "plotly",
         "htmltools",
         "gstat",
         "gtools",
         "automap",
         "kableExtra",
         "reshape2"
)

installKaleido = function(venv="kaleido", full=FALSE){
  #reticulate::use_virtualenv(venv)

  if(!reticulate::condaenv_exists(venv)){
    info("creating new conda venv")
    reticulate::conda_create(venv)
    reticulate::conda_install(venv, c("numpy","plotly","python-kaleido"))
  } else{
    info("setting virtual environment")
    reticulate::use_virtualenv(venv)
  }

  if(full){
    reticulate::use_miniconda(venv)
  }

  reticulate::py_run_string("import sys")
  try( reticulate::py_require(c("numpy","plotly","python-kaleido")) )
}

install = function(libraries=libs, inla=TRUE, deps=TRUE){
  # work out what packages we already have and which we need
  isInstalled = unlist(lapply(libraries, FUN=require, character.only=TRUE))
  uninstalled = libraries[!isInstalled]

  # install anything that is missing
  if(length(uninstalled) > 0){
    install.packages(uninstalled, dependencies = deps)
  }

  # INLA requires it's own thing
  if(inla && !require(INLA)){
    install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
  }

  # get the list of everything we now need to load
  toLoad = libraries
  if(inla){
    toLoad = c(libraries,"INLA")
  }
  # load all required libraries
  for(lib in toLoad){
    library(lib, character.only = T)
  }
}

unifyWD = function(){
  base = getwd()
  if(!endsWith(base, "/R")){
    base = paste(base, "/R", sep="")
  }
  setwd(base)
}

##################
# Project Configs
##################

imgoutFolder = "../paperoutputs/img"
tbloutFolder = "../paperoutputs/tbl"
cacheFolder = "../cache"
imgExt = ".png"
tblExt = ".tex"
# loadorgenerate also defaults to the cache
defaultSaveFolder = cacheFolder

#################
# Utility Methods
#################

LOCAL_CACHE = "~/cache/"

verbose = FALSE

#' @description
#' This method can be used internally to print out updates to the console
#' for debugging purposes. This will slow down the iterations significantly.
#' All of the parameters are simply pasted together and output to the console
#' if the class verbosity is set to true.
#' Further efficiency is obtained if this method is called as little as possible
#' even when it's output is disabled.
info = function(...){
  # check if we should output at all.
  if(verbose){
    output = c()

    for(string in c(...)){
      if(is.vector(string) && length(string) > 1){
        collapsed = paste(string, collapse=", ")
        output = c(output, collapsed)
      }
      else{
        output = c(output, string)
      }
    }
    print(paste(output))
  }
}

isInside = function(values, lb, ub){
  return(values >= lb & values <= ub)
}

isOutside = function(values, lb, ub){
  return(values > ub | values < lb)
}

inside = function(values, lb, ub){
  return(values[isInside(values,lb,ub)])
}

outside = function(values, lb, ub){
  return(values[isOutside(values,lb,ub)])
}

dfinside = function(df, cols, lb, ub){
  return(df[isInside(df[[cols]],lb,ub),])
}

dfoutside = function(df, cols, lb, ub){
  return(df[isOutside(df[[cols]],lb,ub),])
}

#' @description Converts a dataframe into a latex table with a given caption.
latexTable = function(data, caption, tableLabel=deparse(substitute(data)), cellSpec=NULL){

  tableSpec = data
  # apply a given spec
  if(isset(cellSpec)){
    tableSpec = apply(data, c(1,2), cellSpec)
  }

  # Output a table in latex format
  ktable = kbl(tableSpec, caption=caption, label=tableLabel,
               format="latex", escape=FALSE, booktabs=TRUE, digits=2)
  kableTable = kable_classic_2(ktable)

  # a quick regex to rearrange label and caption to be separate. Overleaf likes it better
  tab = gsub("\\\\caption\\{\\\\label\\{(.*?)}(.*?)}",
             "\\\\caption{\\2}\n\\\\label{\\1}",
             kableTable, perl=T)
  return(tab)
}

##############
# SAVE & LOAD
##############

cacheExtension = ".rds"

#' @description Combines folders, parts of filenames and extension into a string
#' path.
path = function(..., folder=".", extension=""){
  extSep = "."
  if(nchar(extension) <1 || substr(extension,1,1) == "."){
    extSep = ""
  }
  filename = paste(... , sep="")
  fileWextension = paste(filename, extension,sep=extSep)
  fullpath = paste(folder, fileWextension, sep="/")
  return(fullpath)
}

FULLPATHREGEX = "[\\/].*?\\.[a-z0-9]{2,4}"
# Simplifies moving from generating, saving and loading data. Useful when you don't
# want to save some data and only want to remake it when that file has been removed.
# A function should be provided which will be used to populate the return value and file
# unless it already exists. If it does exist then it is loaded and returned (using readRDS)
loadOrGenerate = function(filename, func, ..., overwrite=FALSE, localCache=FALSE){


  isFullPath = sum(is.na(str_match(string=filename, pattern=FULLPATHREGEX))) == 0
  # either add in the folder and extension, or just use the file as the full path
  if(!isFullPath){
    folder = defaultSaveFolder
    if(localCache == TRUE){
      folder = LOCAL_CACHE
    }
    filePath = path(filename, folder=folder, extension=cacheExtension)
  }
  else{
    filePath = filename
  }

  # If the data has already been saved, return it.
  if(overwrite == FALSE && file.exists(filePath) == TRUE){
    info("Loading from a previously saved file: ", filePath)
    return(readRDS(filePath))
  }

  # If not then generate the data
  info("Generating data...")
  data = func(...)

  # save it to the file for next time
  info("... Saved as: ", filePath)
  saveRDS(data, filePath)

  # and return it
  return(data)
}

saveimg = function(plt, pltname=deparse(substitute(plt))){
  pltpath = path(pltname, folder=imgoutFolder, extension=imgExt)
  # save plotly or ggplot
  if("plotly" %in% class(plt)){
    save_image(plt, file=pltpath)
  }else{
    ggsave(pltpath,plt)
  }

}

savetbl = function(tbl, caption="", tblname=deparse(substitute(tbl)), cellSpecFunc=NULL){
  save_kable(
    latexTable(tbl, caption, tableLabel=tblname, cellSpec=cellSpecFunc),
    path(tblname,folder=tbloutFolder, extension=tblExt)
  )
}

cache = function(object, filename=deparse(substitute(object)), isRmd=TRUE){
  cFolder = cacheFolder
  if(!isRmd){
    cFolder = str_replace(cacheFolder,"../","")
  }
  outPath = path(filename,folder=cFolder,extension=cacheExtension)
  saveRDS(object, outPath)
  info("Cached: ", outPath)
}

decache = function(filename){
  inPath = path(filename, folder=cacheFolder, extension=cacheExtension)
  obj = readRDS(file=inPath)
  info("Retrieved from cache: ", inPath)
  return(obj)
}




# able to access the passed in variable name using a little trick.
# PLEASE NEVER USE vname DIRECTLY
str = function(var, vname=deparse(substitute(var))){
  return(vname)
}

#' @description increment based on finding all unique combinations with an l
#' length index of up to size n set.
cinc = function(index, n=4){
  # length of index
  l = length(index)
  # right to left try incrementing
  for(i in seq(l,1)){
    endi = n-(l-i) # 1 based index conversion
    # see if this slot can increment further
    if(index[i] < endi){
      # make sure we don't need to reset the position below as we do
      if(i < l){# not incrementing the least significant value
        index[(i+1)] = index[i] + 2 # can't look back
      }
      # increment and return
      index[i] = index[i] + 1
      return(index)
    }
  }

  return(index)
}

#############
# Covariates
#############

#' @description Makes a list of all combinations of a given array of string
#' covariates.
comboList = function(covariates){
  n = length(covariates)
  combos = list()
 # combos[[1]] = "" #blank also counts
  count = 1#2
  for(l in 1:n){

    # num combinations at this length
    nc = choose(n,l)
    # index of length l
    start = 1:l
    index = start
    end = seq(n-l+1,n)

    # for each index part
    for(i in 1:nc){
      # grab the values
      combos[[count]] = covariates[index]

      # increment the index
      index = cinc(index, n)
      count = count + 1 # every element needs a unique index
    }
  }
  return(combos)
}

comboListStrings = function(combinations){
  combinationStrings = unlist(lapply(combinations,FUN=function(item){return(paste(item,collapse=","))}))
  return(combinationStrings)
}

######################
# SF geometry helpers
######################

suspendedGeometry = NULL
suspendedCRS = NULL

#' @description
#' Convenience method to convert a simple feature back into a plain dataframe
dropSF = function(sf, latCol="lat", longCol="long", isAreal=FALSE){
  if(isAreal){
    sf = st_centroid(sf)
  }
  coords = st_coordinates(sf)
  df = st_drop_geometry(sf)
  df[longCol] = coords[,1]
  df[latCol] = coords[,2]
  return(df)
}

#
sfinject = function(sfdata, func, ...){
  # temporarily remove the sf elements
  sfdata = sfsuspend(sfdata)

  # run the given function
  sfdata = st_drop_geometry(sfdata)
  sfdata = func(sfdata, ...)

  # add back the sf elements and return with the altered data
  sfdata = sfresume(sfdata)
  return(sfdata)
}

sfsuspend = function(sfdata){
  if(! "sf" %in% class(sfdata)){
    # can't remove what ain't there
    suspendedGeometry <<- NULL
    suspendedCRS <<- NULL
    info("Tried to suspend geometry of non-sf object")
    return(sfdata)
  }

  # temporarily remove the sf elements
  suspendedGeometry <<- st_geometry(sfdata)
  suspendedCRS <<- st_crs(sfdata)

  # run the given function
  sfdata = st_drop_geometry(sfdata)
  return(sfdata)
}

sfresume = function(sfdata){
  # add back the sf elements and return with the altered data
  if(isset(suspendedGeometry)){
    st_geometry(sfdata) = suspendedGeometry
  }
  else{
    info("Can't resume geometry, none was previously suspended")
  }
  if(isset(suspendedCRS)){
    st_crs(sfdata) = suspendedCRS
  }
  else{
    info("Can't resume projection, none was previously suspended")
  }

  return(sfdata)
}

###################
# Formula Helpers #
###################

form = function(response="cases", covariates="1", given=NULL){
  # take 1 if no covariates are given - represents intercept
  if(length(covariates) == 0){
    covariates = "1"
  }
  # combine the covariates and response into a string
  stringform = paste(response, paste(covariates, collapse=" + "),sep=" ~ ")
  # add any | terms
  if(isset(given)){
    stringform = paste(stringform, paste(given, collapse=" + "),sep=" | ")
  }

  # turn into a formula
  return(as.formula(stringform))
}

baseform = function(fullformula){
  # turn into string
  strform = as.character(fullformula)
  oper = strform[1]
  resp = strform[2]
  covs = strform[3]
  # remove brackets
  cleaned = str_remove_all(covs, "\\(.*?\\)")
  # back to formula and return
  return(form(resp,cleaned))
}

MSE = function(resids){
  se = resids * resids
  mse = mean(se)
  return(mse)
}

RMSE = function(resids){
  rmse = sqrt(MSE(resids))
  return(rmse)
}

logRat = function(simple, complex, pThreshold=0.05){
  a = simple$fit$loglik
  b = complex$fit$loglik

  adof = simple$degrees_of_freedom
  bdof = complex$degrees_of_freedom

  logratio = -2*(a-b)

  p = pchisq(logratio, df=abs(bdof-adof), lower.tail = F)
  sig = p < pThreshold

  return(list(logratio=logratio,p=p,sig=sig))
}

chars = function(strval){
  characters = strsplit(strval,split = NULL)[[1]]
  return(characters)
}

emptyframe = function(cols, types=NULL){
  shapematrix = matrix(nrow=0,ncol=length(cols))
  colnames(shapematrix) = cols
  df = data.frame(shapematrix)

  if(!isset(types)){
    # default to a string column and doubles after that
    types = paste("c",paste(rep("d",length(cols)-1),collapse=""),sep="")
  }
  colnum = 1
  for(type in chars(types)){
    switch(type,
           "c" = {df[,colnum] = as.character(df[,colnum])},
           "n" = {df[,colnum] = as.numeric(df[,colnum])},
           "l" = {df[,colnum] = as.logical(df[,colnum])},
           "d" = {df[,colnum] = as.double(df[,colnum])}
           )
    colnum = colnum + 1
  }

  return(df)
}

colmetric = function(data, byCol, metric, ...){
  results = list()
  sets = unique(data[[byCol]])

  for(set in sets){
    bydata = data[data[[byCol]]==set,]
    metricvals = metric(set, bydata, ...)
    results[[set]] = metricvals
  }
  return(results)
}

listmetric = function(data, metric, columnNames=NULL, ...){
  sets = unique(names(data))

  applied = lapply(seq_along(data), function(index){
    set = names(data)[[index]]
    obj = data[[index]]

    metricVals = metric(set, obj) # can't pass ... on. not sure why. nested calls?
    return(metricVals)
  })
  vals = unlist(applied)

  valsPerSet = length(vals) / length(sets)
  valMatrix = t(matrix(vals, nrow=valsPerSet))

  results = data.frame(valMatrix)
  results$set = sets

  if(isset(columnNames)){
    colnames(results) = columnNames
  }

  return(results)
}

distv = function(a, b, scaleBy=NULL){
  diff = (a-b) * scaleBy # scale the difference by the global
  sqdiff = diff*diff
  d = apply(sqdiff, 1, FUN=sum)
  return(sqrt(d))
}

otherColname = function(colnames){
  othernames = paste(colnames,"other",sep="_")
  return(othernames)
}

pairwise = function(df, coordCols=colnames(df), fun=NULL, otherColnameFunction=otherColname){
  end = nrow(df)

  increasing = c()
  decreasing = c()
  for(i in 1:(end-1)){
    decreasing = c(decreasing, (i+1):end)
    increasing = c(increasing, rep(i,end-i))
  }

  pairs = data.frame("i" = 1:length(decreasing))
  otherCoordCols = otherColnameFunction(coordCols)

  for(i in 1:length(coordCols)){
    col = coordCols[i]
    pairs[[col]] = df[increasing,col]
    other = otherCoordCols[i]
    pairs[[other]] = df[decreasing,col]
  }

  # ditch the id backbone and re-order the columns nicely
  pairs = pairs[c(coordCols, otherCoordCols)]

  if(isset(fun)){
    pairs$distance = fun(pairs[coordCols], pairs[otherCoordCols])
  }

  return(pairs)
}

#' @desription Create a dataframe that can be used as a raster, in the form
#' of a simple feature. A grid is generated matching the given resolution over
#' the full extent of the boundary. The boundary is then used as a mask to return
#' a cutout of the points which lie within.
sfRaster = function(boundary, res){
  # interpolate coordinates on both axes
  # calculate the metrics required to fill the grid
  corners = raster::extent(boundary) #test data # c(0,10,5,20) #
  xmin = corners[1]
  xmax = corners[2]
  xstep = (xmax-xmin) / (res[1]-1)
  ymin = corners[3]
  ymax = corners[4]
  ystep = (ymax-ymin) / (res[2]-1)

  # generate the coordinate axes
  xs = xmin + rep(0:(res[1]-1)) * xstep
  ys = ymin + rep(0:(res[2]-1)) * ystep

  # fill in all off the possible combos
  xcombos = rep(xs, each=res[2])
  ycombos = rep(ys, times=res[1])
  n = length(xcombos)*length(ycombos)

  # convert to a simple feature, but retain the lat and long
  coords = data.frame(long=xcombos,lat=ycombos, x=xcombos, y=ycombos)
  coords = tosf(coords, coords=c("x","y"))

  # cut out the points that lie within the actual geometry
  insidePoints = coords[st_contains(boundary, coords$geometry)[[1]],]

  # tests: For making the x and y combinations
  if(verbose){
    info("tests passed x: ",
         xs[1] == xmin &&
           xs[res[1]] == xmax &&
           length(xs)==res[1]
    )

    info("tests passed y: ",
         ys[1] == ymin &&
           ys[res[2]] == ymax &&
           length(ys)==res[2]
    )

    # a visual check
    print(ggplot() + geom_sf(data=boundary) + geom_sf(data=coords) + geom_sf(data=insidePoints, color="red3"))
  }

  return(insidePoints)
}


##############
# Diagnostics
##############


#' @description
#' A place to start when looking for an overview of a dataset.
overview = function(data){
  # Name and dimensions of the data
  print(str(data))
  print(paste("Columns: ", ncol(data), "  Rows: ", nrow(data)))

  # Show what types the columns have
  colsByType = list()
  for(col in 1:ncol(data)){
    name = colnames(data)[col]
    type = typeof(data[[col]])

    if( isset(colsByType[[type]]) ){
      colsByType[[type]] = c(colsByType[[type]], name)
    }
    else{
      colsByType[[type]] = name
    }
  }
  print(colsByType) # TODO: make a pretty table from this

  # Show the scales of each column
  numericCols = c()
  if(isset(colsByType[["double"]])){
    numericCols = c(numericCols, colsByType[["double"]])
  }
  if(isset(colsByType[["integer"]])){
    numericCols = c(numericCols, colsByType[["integer"]])
  }
  pivotScaled = dplyr::select(pivot_longer(data, cols=numericCols), c("name","value"))
  violins = ggplot(pivotScaled, aes(x=name, y=value, fill=name)) + geom_violin() +
    theme_bw() + xlab("Column") + ylab("Magnitude") +
    labs(title="Violin Plot of Numeric Columns", fill="")
  print(violins)

  # Scale everything so that we can look at the distributions a bit
  dataScaled = data
  dataScaled[numericCols] = scale(dataScaled[numericCols])
  pivotScaled = dplyr::select(pivot_longer(dataScaled, cols=numericCols), c("name","value"))
  violinsScaled = ggplot(pivotScaled, aes(x=name, y=value, fill=name)) + geom_violin() +
    theme_bw() + xlab("Column") + ylab("Magnitude") +
    labs(title="Scaled Violin Plot of Numeric Columns", fill="")
  print(violinsScaled)

  # show any correlations
  numberdata = dplyr::select(data,all_of(numericCols))
  correlationHeatmap = plotHeatmapDF(numberdata,use="complete.obs")
  print(correlationHeatmap)

  # missingness
  missing = sapply(data, function(col) sum(is.na(col)))
  missingPlt = ggplot(NULL,aes(x=names(missing),y=missing, fill=names(missing))) +
    geom_bar(stat="identity") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) +
    xlab("Column") + ylab("Missing Values") +
    labs(title="Missingness by Column", fill="")
  print(missingPlt)

  return(list(colsByType=colsByType, violins=violins, violinsScaled=violinsScaled,
              correlationHeatmap=correlationHeatmap, missing=missingPlt))
}

#' @description
#' I just prefer adding periodic components using something more compatible
#' with fourier, i.e. complex numbers
wave = function(x, freq, amp=1, wl=2){
  i = (0 + 1i)
  w = amp * exp(i * pi * (1:length(x)/freq))
  return(Re(w) + Im(w))
}

waves = function(x, frequencies){
  total = rep(0, length(x))
  for(freq in frequencies){
    total = total + wave(x, freq)
  }
  return(total)
}

#' @description A quick spatio temporal overview. Good for seeing if there is evidence
#' of spatial or temporal covariance / residuals in a dataset.
#'
#' the variomodel needs to be a gstat vgm
stoverview = function(data, form=cases ~ 1, spaceCols = c("lat","long"), timeCols=c("year","month"),
                      varioModel=NULL, ciReps=0, autofitVario=FALSE,
                      spaceSummaryFunction=sum, timeSummaryFunction=sum){
  # note the response variable
  response = all.vars(form)[1]

  # look for spatial residuals
  sdata = groupSummary(data, spaceCols, response, summaryFunction=spaceSummaryFunction)
  #sp::coordinates(sdata) = as.formula(paste("~", paste(spaceCols, collapse=" + ")))

  # fit an unspecified matern model to get initial values for a spherical model fit
  vario = ggvario(sdata, form, varioModel, autofit=autofitVario)
  varioplot = vario$plt
  if(ciReps > 0){
    ci = ggvarioConfidenceIntervals(vario$vario, form, sdata, ciReps)
    varioplot = varioplot + ci$ribbon
  }

  # Now look at the whole dataset collapsed into time dimension
  tdata = groupSummary(data, timeCols, response, summaryFunction=timeSummaryFunction)
  acfplot = ggacf(tdata[[response]], p=0.95)
  print(acfplot)

  return(list('vario'=varioplot, 'acf'=acfplot, 'vfit'=vario$fit))
}

ggvario = function(sdata, sform=cases~1, varioModel=NULL, autofit=TRUE){
  # If the sdata is already a spatialpointsdataframe then all is well
  # if we have an simple feature then lets convert it here
  if("sf" %in% class(sdata)){
    info("Converted an sf into a spatialpointsdataframe")
    sdata = as_Spatial(sdata)
  }

  # start with the sample variogram
  vario = variogram(sform, data=sdata)

  # Either fit the suggested model, or fall back to a search
  vfit=NULL
  if(isset(varioModel)){
    vfit = fit.variogram(vario, model=varioModel)
  }else if(autofit){
    # there is useful info here we are throwing away.
    vfit = autofitVariogram(sform, sdata)$var_model
  }

  # the ggplot
  varioplot = ggplot(vario, aes(x=dist, y=gamma)) +
    geom_point(color="steelblue4") +
    scale_y_continuous(limits=c(0,NA)) +
    theme_bw()
  if(isset(vfit)){
    varioplot = varioplot +
      geom_line(data=variogramLine(vfit, maxdist=max(vario$dist)), color="salmon2")
  }

  result = list(plt=varioplot, fit=vfit, vario=vario, sform=sform)
  return(result)
}

#' @description
ggvarioConfidenceIntervals = function(vario, sform, sdata, nreps, makeRibbon=TRUE){
  # calculate confidence interval of gamma - H0 is that order doesn't matter
  # i.e. distance makes no difference
  dists = unique(vario$dist)
  ndists = length(dists)
  runs = 1:nreps
  n = nreps * ndists

  varioruns = data.frame(
    runs = rep(runs, each=ndists),
    dist = rep(dists, times=nreps),
    gamma = rep(NA, n)
  )
  response = all.vars(sform)[1]
  shuffled = sdata

  # get the replicated variograms in one big table by run
  for(i in runs){
    # shuffle
    shuffled[[response]] = sample(shuffled[[response]])
    # add the vario data
    shuffvario = variogram(sform, data=shuffled)
    varioruns[varioruns$run==i,]$gamma = shuffvario$gamma
  }

  # work out the confidence intervals at each distance
  citable = data.frame(dist = dists, lb = rep(0,ndists), ub=rep(0,ndists))
  for(d in dists){
    shufmeansd = varioruns[varioruns$dist==d,]
    ci = quantile(shufmeansd$gamma, c(0.025, 0.975))
    citable[citable$dist==d,]$lb = ci[1]
    citable[citable$dist==d,]$ub = ci[2]
  }

  ggribbon = NULL
  if(makeRibbon){
    ggribbon = geom_ribbon(aes(ymin=citable$lb, ymax=citable$ub), color="gray", alpha=.25)
  }
  result = list(ci=citable,
                ribbon=ggribbon)
  return(result)
}

#' @description Converts a base R acf plot to a ggplot
ggacf = function(data, p=0.95){
  ci = acfCI(data, p)

  acfplot = ggplot(data = ci$df, aes(x=lag, y=acf)) +
    geom_hline(aes(yintercept=0), color='salmon4') +
    geom_segment(aes(xend=lag, yend=0)) +
    geom_hline(aes(yintercept=ci$ub), linetype=2, color = 'steelblue4') +
    geom_hline(aes(yintercept=ci$lb), linetype=2, color = 'steelblue4') +
    theme_bw()

  return(acfplot)
}

acfCI = function(data, p=0.95){
  acfd = acf(data, plot=FALSE)
  acfdf = data.frame(acf=acfd$acf, lag=acfd$lag)

  cv = qnorm((1+p)/2) / sqrt(acfd$n.used)
  e = mean(acfdf$acf)
  return(list("lb"=-cv, "mean"=e, "ub"=cv, "df"=acfdf))
}

# Calculates the magnitude of unexplained variance, i.e. the total acf which
# lies outside of the bounds. Removes the first lag assuming it is 0 and therefore
# not unexplained
acfUV = function(data, p = 0.95){
  ci = acfCI(data, p)
  vals = ci$df$acf[-1]
  # make sure that direction in sign doesn't matter in terms of magnitude
  below = sum(abs(vals[vals < ci$lb]) - abs(ci$lb))
  above = sum(abs(vals[vals > ci$ub]) - abs(ci$ub))
  unexplained = above + below
  return(unexplained)
}


#' @description A convenience method for finding out the size of each item in a list,
#' by index.
listsize = function(listvar, units="Mb"){
  sizes = sapply(listvar, function(item) format(object.size(item), units=units))
  view(sizes)
}

# shifts a vector to the left or right by an amount. Values that fall out are
# removed and zeroes are inserted in the newly made spaces.
# can only shift up until the length -1 th value. Anything beyond that can
# produce undesired results.
timeshift = function(column, by, replacement=NA){

  # ignore zero length shifts
  if(by == 0){
    return(column)
  }

  # note the end of the column
  end = length(column)

  if(by < 0){
    # negative shift, but we will need a positive number
    by = abs(by)
    # put together a slice of the end of the column, then add zeroes
    shifted = append(column[(by+1):end] ,rep(replacement, by))
    return(shifted)
  }
  else{
    # begin with the zero padding
    pad = rep(replacement,by)
    # copy the first elements from the column
    shifted = append(pad, column[0:(end-by)])
    return(shifted)
  }
}

#' @description A convenience method for sorting data frames without external libraries
#' in response to the new error thrown by order. Additionally, re-indexes the data frame.
sortdf = function(df, cols){
  sortIndex = do.call(base::order, as.list(dplyr::select(df, all_of(cols))))
  sortedDF = df[sortIndex,]

  rownames(sortedDF) = 1:nrow(sortedDF)
  return(sortedDF)
}

splitdf = function(df, splitAt){
  firstHalf = df[1:splitAt,]
  secondHalf = df[splitAt+1:(nrow(df)-splitAt),]
  return(list("before"=firstHalf, "after"=secondHalf))
}

#' @description Finds the index of the first column with a value greater than
#' or equal to the given one.
firstGTE = function(df, column, value){
  splityear = 2016
  row = df[df[[column]] >= value,][1,]
  index = strtoi(rownames(row))
  return(index)
}

#' @description Creates a unique contiguous index from the specified columns.
#' In the range [1,N] where N is the number of unique combinations.
uIndex = function(data, cols, newColName="uid"){
  data = ungroup(data) # groupsummaries leave these traces
  # first we make a unique ID from all of the identified columns
  data = unite(data, col="pk", all_of(cols), sep="-", remove=FALSE)
  # then make a numbered list of all unique elements - including the above
  index = unique(select(data, c("pk",cols)))  #all_of( c("pk", cols) )
  index[newColName] = 1:nrow(index) # our unique id
  # Then we put the index into the original table
  data = applyLookupv(data, "pk", index, "pk", colsToTransfer=c(newColName))
  # Now we can remove the temporary column
  data = select(data, -pk)
  # and send it back
  return(data)
}

#' @description
#' A convenience method to group and summarise, with the ability to specify
#' column names.
groupSummary = function(data, groupBy, summariseBy, renameTo=NA,
                        summaryFunction=mean, copyCols=c()){
  grouped = dplyr::select(data, all_of(c(groupBy, summariseBy, copyCols))) %>%
    group_by(across(groupBy)) %>%
    summarise(across(all_of(summariseBy), summaryFunction),
              across(all_of(copyCols), identity))

  if(!is.na(renameTo)){
    colnames(grouped) = renameTo
  }

  # remove the grouping, it has done it's job now
  final = ungroup(grouped)
  return(final)
}

#' @description A convenience method for scaling multiple columns and adding
#' a log of the cases
scaleDF = function(data, cols=colnames(data)){
  # scale each of the columns
  n = nrow(data)
  for(col in cols){
    scaled = scale(data[[col]])[,1] # first column - could vectorise more
    if(sum(is.na(scaled)) == n)(
      scaled = rep(0,n)
    )
    data[[col]] = scaled
  }

  # return the locally modified dataframe
  return(data)
}

#' @description A convenience method for scaling multiple columns and adding
#' a log of the cases
lscaleDF = function(data, cols=colnames(data), casesCol="cases"){
  # scale each of the columns
  data = scale(data, cols)
  # add the log cases
  logCasesCol = paste("l", casesCol, sep="")
  data[[logCasesCol]] = log(data[[casesCol]])

  # return the locally modified dataframe
  return(data)
}

#' @description Just a convenience min-max scaling function [0,1]
normalise = function(x){
  n = (x - min(x)) / (max(x) - min(x))
  return(n)
}

monthlyMeans = function(data, targetCol, monthCol="month"){
  #data$month = as.numeric(format(as.Date(data$date, format="%Y-%m-%d"), "%m"))
  #data$year = as.numeric(format(as.Date(data$date, format="%Y-%m-%d"), "%Y"))
  #data$monthMean = data$month

  means = dplyr::select(data, monthCol)

  for(monthNum in 1:12){
    # use the data for each month to find the monthly means
    dataForMonth = filter(data, data[monthCol] == monthNum)
    monthMean = mean(dataForMonth[[targetCol]])

    # replace the month number with the mean value
    means[means[monthCol] == monthNum,] = monthMean

    print(paste("Month ", monthNum, " has mean ", monthMean, " for ", targetCol))
  }

  # weird, but return the month column from the means as we replaced the month
  # number with the mean value earlier. Gives a raw vector of length of original data
  return(means[[monthCol]])
}


alignIndex=function(row, rowID, lookupCol){
  rowGeoID = row[rowID]
  return(which(rowGeoID == lookupCol))
}

findMatch = function(data, dataCols, row, rowCols){
  # reduce the possible matches one by one
  mask = rep(TRUE, nrow(data))

  for(colnum in 1:length(dataCols)){
    # find the first matching row in the lookup table
    colA = dataCols[colnum]
    colB = rowCols[colnum]
    mask = mask & data[colA]==row[[colB]]
  }

  matches = data[mask,]
  firstMatch = matches[1,]#first match
  return(firstMatch)
}

# deprecated in favor of the vectorised implementation
applyLookup=function(data, rowCol, lookupTable, lookupCol, colsToTransfer, renameCols=NULL){
  # a quick sanity check on rename Cols
  if(isset(renameCols) & length(colsToTransfer) != length(renameCols)){
    warning("rename cols must have the same length as colsToTransfer. renameCols is being ignored")
    renameCols=NULL
  }

  # first, make space in the table for our new values
  for(i in 1:length(colsToTransfer)){
    newname = colsToTransfer[i]
    if(isset(renameCols)){
      newname = renameCols[i]
    }
    data[newname] = rep(NA, nrow(data))
  }

  ncols = length(rowCol)# assumes a LOT
  # now iterate through the table rows
  for(rowNum in 1:nrow(data)){

    # get the current row
    row = data[rowNum,]

    lookupRow = findMatch(lookupTable, lookupCol, row, rowCol)

    # set the values in the data frame's new columns
    for(i in 1:length(colsToTransfer)){
      newname = colsToTransfer[i]
      if(isset(renameCols)){
        newname = renameCols[i]
      }
      data[[rowNum, newname]] = lookupRow[[colsToTransfer[i]]]
    }
  }
  # send back our local copy of the data which has been augmented.
  return(data)
}


applyLookupv = function(data, dataCol, lookupTable, lookupCol=NULL, colsToTransfer){
  # First, note the columns we must keep from the original table
  originalCols = colnames(data)
  # we also want to keep the cols to transfer
  cols = c(originalCols, colsToTransfer)

  # Assume that the column is the same in both tables if not specified
  if(is.null(lookupCol)){
    lookupCol = dataCol
  }

  # merge everything together - prioritising the structure of the original data
  combined = merge(data, lookupTable, by.x=dataCol, by.y=lookupCol,
                   all.x=T, all.y=F, suffixes=c("",".lookup"))

  # manually replace the NA values in the data with non NA values in the lookup
  # i.e. don't overwrite anything
  for(col in colsToTransfer){
    temp = dplyr::coalesce(combined[[col]],combined[[paste(col,".lookup", sep="")]])
    combined[[col]] = temp
  }

  # subset out the columns we actually want
  combined = select(combined, all_of(cols))

  # send it back
  return(combined)
}

lookup=function(row, rowCol, lookup, lookupCol, colsToTransfer){
  mask = lookup[lookupCol] == row[rowCol]
  lookupRow = lookup[mask]

  for(col in colsToTransfer){
    row[col] = lookupRow[col]
  }

  return(row)
}

isset = function(value){
  nas = is.na(value)
  lenNas = length(nas)

  if(sum(nas) == 0 & lenNas == 0){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}


#############
# GEO JSON
#############

getGeoJSONProperties = function(geojson, propertyName){
  values = c()
  for(f in geojson$features){
    props = f$properties
    values = c(values, props[[which(names(props)==propertyName)]])
  }
  return(values)
}

plotGeoJSON = function(geojson, id, title=""){
  locNames = getGeoJSONProperties(geojson, id)

  fig = plot_ly()
  fig = fig %>% add_trace(
    type="choropleth",
    geojson=geojson,
    locations=locNames,
    z=runif(length(locNames),1,10),
    colorscale="Purples",
    featureidkey=paste("properties.", id, sep="")
  )
  fig = fig %>% layout(
    geo = list(
      fitbounds = "locations",
      visible = FALSE
    )
  )
  fig = fig %>% colorbar(title=title)
  fig = fig %>% layout(title=title)
  return(fig)
}

#########
# DATES
#########

# order 1 2 for ymd, 3 2 for dmy. When split by sep, which part is, in order: year, month
splitDates = function(data, dateColname="date", sep="-", order=c(1,2), dateFormat="%Y-%m-%d"){
  dateCol = dplyr::select(data, all_of(dateColname))

  # force coercion to string to use the following methods
  dateCol = as.character(dateCol[[1]])

  parts = strsplit(dateCol, sep)

  data$year = as.numeric(sapply(parts, "[", order[1]))
  data$month = as.numeric(sapply(parts, "[", order[2]))
  data$date = as.Date(paste(data$year, data$month, 1, sep="-"), format=dateFormat)
  data$ts = as.numeric(as.POSIXct(dateCol, format=dateFormat))

  return(data)
}

# helpful constants
dayTS = 86400

# helpful functions
toTimestamp = function(dateColumn){
  timestamps = as.numeric(as.POSIXlt(dateColumn))
  return(timestamps)
}

toDate = function(dateStringColumn, format="%Y-%m-%d"){
  dates = as.Date(dateStringColumn, format=format)
  return(dates)
}

getRange = function(dateColumn){
  start = min(dateColumn, na.rm=TRUE)
  end = max(dateColumn, na.rm=TRUE)
  range = start:end
  return(range)
}


# needs ym column already in there. it's a ym to date mapper
dateAxis = function(df, plt, each=1, format="%b %Y"){
  dates = paste(df$year, df$month, 1, sep="/")
  datelabs = strftime(dates, format="%b")

  yearsIndex = which(!duplicated(df$year))
  if(each > 1){
    yearsIndex = yearsIndex[1:length(yearsIndex) %% each == 0]
  }

  datelabs[yearsIndex] = strftime(dates[yearsIndex], format=format)

  plt = plt + xlab("Date") +
    scale_x_continuous(breaks = df$ym[yearsIndex],
                       labels = datelabs[yearsIndex],
                       guide = guide_axis(minor.ticks = F)) +
    theme(axis.text.x = element_text(angle=45, size = 6))

  return(plt)
}

############
# LOCATIONS
############

geocodeToNumeric = function(data, latColName, longColName){
  # Convert the coordinates (with ,) into doubles
  data[, latColName] = as.numeric(gsub(",",".",as.character(data[, latColName])))
  data[, longColName] = as.numeric(gsub(",",".",as.character(data[, longColName])))

  return(data)
}

processByCoord = function(data){
  # first mmake a dataframe from the table of the data
  result = data.frame(table(data$LATITUDE, data$LONGITUDE))
  # remove the zero frequencies
  result = filter(result, Freq > 0)
  # rename the columns
  colnames(result) = c("lat", "long", "cases")

  result = geocodeToNumeric(result, "lat", "long")

  return(result)
}

#############
# Variograms
#############

inferTimestamp = function(data, timeCols){
  # TODO: add an is array type check function and use here
  # TODO: expand this to work with more insane possibilities
  # TODO: add a default md or dm. for now assumes ymd or dmy
  # see how many parts we have
  # TODO: Handle text days, months etc
  n = length(timeCols)
  rowCount = nrow(data)

  # default values - single values will automatically work with columns
  defaultYear = "1970"
  year = rep("1970", rowCount)
  month = rep("01", rowCount)
  day = rep("01", rowCount)


  for(i in 1:n){
    col = timeCols[i]
    avgLen = nchar(data[col]) / nrow(data[col])

    if(avgLen >= 3.5){
      # longest is year (YYYY only valid assumption)
      year = data[col]

      print(avgLen)
      print(i)
      print(col)

      # if we reach position e and find a year then assume DMYYYY
      if(i == 3){
        year = data[col]
        day = data[1]
      }
    }

    if(i == 2){
      # assume middle column is always month if available
      month = data[col]
    }
  }

  #ts = toTimestamp(paste(year, "-", month, "-", day, sep=""))
  ts = rep(NA, nrow(data))
  for(i in 1:nrow(data)){
    ts[as.Date(paste(year[i], month[i], day[i]))]
  }
  return(ts)
}

# todo: finish
# todo: remove need for YM column
STvarioplot = function(sfdata, sform = resid~1, cireps=10){
  # note the response variable
  response = all.vars(sform)[1]

  # calculate variograms per time bag
  bags = ntimes / 12
  bagSize = 12
  varios = list()
  cis = list()
  for(i in 0:(bags-1)){
    start = 1 + (i*bagSize)
    end = start + bagSize - 1

    print(paste(start, end))

    # get the data within the time group
    tdata = dplyr::filter(sfdata, ym >= start & ym <= end)

    # aggregate the data to remove temporal structure
    tdataagg = groupSummary(tdata, c("year"), summariseBy=response)

  #  meanResids = rowMeans(tdata)
  #  sregions$resid = meanResids

    # calculate just the variogram data, no plot
    vario = variogram(sform, data=tdata)


    # now calculate the confidence intervals on a per time bag basis
    ci = ggvarioConfidenceIntervals(vario=vario,
                                    sform=sform,
                                    sdata=tdata,
                                    nreps=cireps,
                                    makeRibbon=FALSE)
    # add the ci range to the variogram data
    vario$lb = ci$ci$lb
    vario$ub = ci$ci$ub

    # save it
    varios[[(i+1)]] = vario
  }

  # plot the variograms
  varios = bind_rows(varios, .id="time")
  varios$time = as.numeric(varios$time)
  varios$sig = rep(0, nrow(varios))
  varios$sig[isOutside(varios$gamma, varios$lb, varios$ub)] = outside(varios$gamma, varios$lb, varios$ub)

  # 2d plot of areas with autocorrelation exceeding null hypothesis
  stvario = plot_ly(data=varios, type="contour", x=~time, y=~dist, z=~sig, colorscale="Jet")
  return(stvario)
}

stvario = function(data, form, timeCols=c("year","month"), spaceCols=c("lat","long"), timestamp=c("ts")){
  stdatalong = makeSparse(data, c(spaceCols, timeCols))
  stdatalong = stdatalong[order(stdatalong[[timestamp]]),]

  spacecoords = SpatialPoints(unique(stdatalong[spaceCols]))
  timecoords = as.POSIXct(unique(stdatalong[[timestamp]]))

  stfdata = STFDF(spacecoords, timecoords, stdatalong)
  vario = variogramST(form, data=stfdata)
  return(stfdata)
}

###############################
# Models - convenience methods
###############################

# TODO: add this properly
chunkpredict = function(){

  predChunkSize = 120 * nlocs # months per location = 1 chunk
  preds = c()

  # make projections in chunks, combine later. Note this in discussions
  # as it will affect pred error

  # just get the whole number, remainder will add a whole iteration
  chunktimes = (n / predChunkSize)
  numchunks = chunktimes %/% 1
  chunkRemainder = chunktimes %% 1
  hasPartialEnd = chunkRemainder != 0 # seems to work, concerned about floating point integer coercion
  if(hasPartialEnd){numchunks = numchunks + 1}
  indexOffset = 0
  last = 0

  for(chunk in 1:numchunks){
    info("Projecting chunk",chunk)
    # chunk start and end indexes
    start = last + 1 #((chunk-2) * predChunkSize + 1) + indexOffset
    end = ((chunk-1) * predChunkSize) + indexOffset

    # chunk end, including if there is a non-full, first chunk
    if(hasPartialEnd && chunk == 1){
      info("truncated first run")
      indexOffset = as.integer(predChunkSize * chunkRemainder + 1)
      end = indexOffset
      start = 1
    }
    last = end

    info(chunk, start, end)
    #info(future[start-1,]$year, future[start,]$year)
    #info(future[end,]$year, future[end+1,]$year)

    # make predictions and append mean
    #futurePred = predict(model, future[start:end,], bestformula)
    #preds = c(preds, futurePred$mean)
  }

  future$cases = preds
}

# adds a column containing residuals from fitting bym models per year.
yearlyBYM = function(data, form, family="poisson"){
  # get all of the years
  years = sort(unique(data$year))
  # somewhere to store all of the models
  models = list()

  i = 1
  for(year in years){
    # first subset the data per year
    dataperyear = data[data$year==year,]
    dataperyear = dataperyear[order(dataperyear$geoid),]

    cat(paste(nrow(dataperyear), " rows. ", year, "--", sep=""))
    # add a model for each year to the models data structure
    model = inla(
      form,
      data=dataperyear,
      family=family,
      control.predictor=list(compute=TRUE),
      control.compute=list(cpo=TRUE, dic=TRUE)
    )
    models[[i]] = model
    print("--model complete")

    # increment the index
    i = i+1
  }

  # return just the column made, the local scope dataset was just to keep things aligned
  return(models)
}

getModelResiduals = function(model){
  return(model$residuals$deviance.residuals)
}

getYearIndex = function(year, years){
  # get the models index for a given year
  index = which(years==year)
  return(index)
}

# adds a column to the data from the models
bymToColumn = function(data, models, getFunction=getModelResiduals){
  # make a fresh column for all the residuals
  colname = "temp"
  data[colname] = rep(NA, nrow(data))
  # get all of the years
  years = sort(unique(data$year))

  for(i in 1:length(years)){
    # get the model and year
    model = models[[i]]
    year = years[i]
    # copy over the values for a given year into the new column
    data[data$year==year,][colname] = getFunction(model)
  }

  # return just the column made, the local scope dataset was just to keep things aligned
  return(data[[colname]])
}

######################
# Longform conversion
######################

#' @description Generates a list. Each entry will be indexed by a string name,
#' from the cols vector. The value of which will be either all of the unique
#' sorted values from that column in the data frame. OR, if an override with more
#' than one value is given, it will be substituted in.
#' the column names should be given in order of magnitude i.e. for date: year, month, day
listCombinations = function(data, cols, overrides=NULL){
  # Unless overridden with an enumeration/scale, take the unique values of each column
  combos = list()

  ncols = length(cols)
  n = 1

  for(i in 1:ncols){
    col = cols[i]
    # prioritise the overrides given - god help you if things aren't already sorted
    if(isset(overrides) & col %in% names(overrides)){
      combos[[col]] = overrides[[col]]
    }
    else{
      # looks odd because unique is a list with one element
      combos[[col]] = unique(data[[col]])
    }
    n = n*length(combos[[col]])
  }

  # now that we have an example of each unique column entries, inflate them to be
  # equal length
  before = 0
  after = n
  # slide along the multiplicative combinations repeating as defined by the other columns
  for(i in 1:ncols){
    combo = combos[[i]]
    currentLen = length(combos[[i]])
    # the columns after - rep that many times
    after = after / currentLen
    info("before", before)
    info("after", after)
    info("current", currentLen)

    # when repeating, don't forget to remove the number of current column combos
    combos[[i]] = rep(combo,
                      times=max(before,1),
                      each=max(after,1))
    # the columns before - each that many times
    before = max(before,1) * currentLen


    info("length",length(combos[[i]]))
  }
  return(combos)
}

#' @description Creates a list where each element contains every unique
#' combination of times. These are then scaled so that they have a matching
#' length. This happens by assuming the column names are given in descending
#' order of size. The first column has elements repeated, the final column has
#' the sequence repeated, the middle columns are a proportional mix.
#'
#' @param timeCols is a vector of strings which give the columns which are a part
#' of the time coordinates, such as year, month, day etc.
#'
#' @param overrides may be provided in case the unique values given in the table are
#' incomplete. The entire set of unique combinations will be replaced with the
#' value in overrides instead.
#'
#' @param copyCols Is a vector of column names (strings) specifying columns
#' which should take just the unique values from and not scale up with the
#' other time columns. This is specifically for values which already have
#' the correct number of combinations which will match with the expanded times
#' list.
expandT = function(data, timeCols, overrides, copyCols){
  # Firstly, expand the data into a list of all possible times
  times = listCombinations(data, timeCols, overrides)
  # get counts for each of the columns defined in the given list
  lengths = unlist(lapply(times, length))
  # get the number of time coordinates
  ntimes = prod(lengths)

  # expand columns as needed to match the total length

  # default case is to basically copy the column across by ignoring it here. op = 0
  ncols = length(timeCols)
  sumcols = prod(lengths)
  start = 1

  for(i in 1:ncols){
    col = timeCols[i]
    # combine with other columns - repeat the column to match ntimes
    # time has an order of precedence to determine how many times to repeat
    # each element (t) and how many times to repeat the sequence (n)
    n=1
    t=1
    if(i == start){
      t = prod(lengths[-1]) # remove start, don't self multiply
    }else if(i == ncols){
      n = prod(lengths[-ncols]) # remove end, don't self multiply
    }else{
      n = prod(lengths[1:(i-1)])
      t = prod(lengths[(i+1):ncols])
    }

    times[[col]] = rep(times[[col]], times=n, each=t)
  }

  # add in the copy cols
  if(isset(copyCols)){
    times = c(times, listCombinations(data, copyCols))
  }

  return(times)
}

#' @description Makes an expanded table from the given data frame. The
#' column names relating to space and time need to be specified, or will be assumed,
#' The expanded table contains all space time positions, but no data columns.
#'
#' Use makeSparse instead to make a longfrom version of a table with data columns
expandST = function(data, spaceCols=c("long","lat"), timeCols=c("month","year"),
                    timeOverride=NULL, spaceOverride=NULL, copyCols=c()){ #c(0,1:12)
  # enumerate all of the space-time variables
  coords = list()

  # Space
  # get the unique combinations - TODO: fix the spaceOverride, probably doesn't work as will need expansion
  #space = data.frame(listCombinations(data, spaceCols, spaceOverride))
  space = unique(select(data, all_of(spaceCols)))

  # removed sort for now, do it before calling the method, or after making sortdf work flawlessly with character columns
  # sort the space columns by the last given. Arbitrary, but can be exploited by user
  #lastCol = spaceCols[length(spaceCols)]
  #space = sortdf(space, lastCol)
  nlocs = nrow(space)

  # Time
  time = listCombinations(data, timeCols, timeOverride)
  ntimes = length(time[[1]])# all entries should be the same length

  # Space-Time
  coords = list()

  # Now expand space and time to match length for the new table
  for(scol in spaceCols){
    coords[[scol]] = rep(space[[scol]], times=ntimes, each=1)
  }
  for(tcol in c(timeCols, copyCols)){ # don't forget to include copied columns
    coords[[tcol]] = rep(time[[tcol]], times=1, each=nlocs)
  }

  # make all of the space-time coordinate columns
  table = data.frame(coords)

  # return the new table.
  return(table)
}


#' @description Returns a copy of the table expanded to have all space-time
#' coordinates possible, as defined by primary key (pk) columns: space cols,
#' time cols and copy cols. The specified cases column is aggregated. If the
#' column indicated does not already exist then it is assumed that each row
#' represents a single case and this is added.
#'
#' Note: Deduplication will occur using the primary key, so if there are
#' multiple numbers of cases reported at the same place in space and time then
#' these should have previously been aggregated. The primary key is the combination
#' of space cols, time cols and copy cols.
#'
#' An example of how to set an override: timeOverride=list("month"=1:12) for
#' if the data has some unrepresented months.
#'
#' additionally, any values of the primary key which contain NA will have
#' their rows removed as they do not represent a valid space time count.
makeSparse = function(originalTable,
                      spaceCols=c("long", "lat"), timeCols=c("year", "month"), copyCols=c(),
                      spaceOverride=NULL, timeOverride=NULL,
                      casesCol="cases", repNA=NULL){

  # Make the primary key - column names with values unique to each space-time coordinate
  pk=c(spaceCols,timeCols,copyCols)
  pk=pk[pk!=casesCol] # make sure that the cases column isn't here, even if it's being copied

  # make the sparse table of all space-time coordinates
  longTable = expandST(
    data = originalTable,
    spaceCols = spaceCols,
    timeCols = timeCols,
    copyCols = copyCols,
    spaceOverride = spaceOverride,
    timeOverride = timeOverride
  )


  # summarise the cases
  # remove duplicate rows and order by the pk so that future things align
  cases = drop_na(originalTable, pk)
  cases = arrange(cases,dplyr::across(dplyr::all_of(pk)))# sorting is needed for case aggregation

  # group the cases by sparse ID and count the cases
  caseCounts = groupSummary(cases, pk, casesCol, summaryFunction=sum)

  # remove duplicates now that cases have been counted
  cases = distinct(cases, dplyr::across(dplyr::all_of(pk)), .keep_all=TRUE)

  # replace the case column
  cases[casesCol] = caseCounts[casesCol]

  # merge cases into long table using primary key
  #a = merge(x=longTable, y=c(cases,copyCols), all.x=TRUE, by=pk)
  sparse = merge(x=longTable, y=cases, all.x=TRUE, by=pk)

  # finally, replace na cases with zero - don't do this by default, NA may be more meaningful
  if(isset(repNA)){
    sparse[casesCol] = replace_na(sparse[[casesCol]], repNA)
  }

  return(sparse)
}

#' @description  Aggregates a tables columns by the primary key provided. Columns
#' in the key will be collapsed into single instances, forming a grouping. Other
#' columns can by either averaged or summed in the resulting table.
#'
#' Ideally all of the columns in the table should appear as either part of the
#' key or aggregate parameters. No guarantees are made for other columns.
aggregate = function(originalTable, pk, sumCols=NULL, meanCols=NULL){
  # summarise the cases
  # remove duplicate rows and order by the pk so that future things align
  table = drop_na(originalTable, pk)
  table = arrange(table, dplyr::across(dplyr::all_of(pk)))# sorting is needed for case aggregation

  # group the cases by primary key and summerise the columns indicated
  isSum = isset(sumCols)
  isAvg = isset(meanCols)
  if(isSum){
    summed = groupSummary(table, pk, sumCols, summaryFunction=sum)
  }
  if(isAvg){
    averaged = groupSummary(table, pk, meanCols, summaryFunction=mean)
  }

  # remove duplicates after aggregations have been calculated
  table = distinct(table, dplyr::across(dplyr::all_of(pk)), .keep_all=TRUE)

  # replace the aggregated columns
  if(isSum){
    for(col in sumCols){
      table[col] = summed[col]
    }
  }
  if(isAvg){
    for(col in meanCols){
      table[col] = averaged[col]
    }
  }
  return(table)
}





####################
# Adding Covariates
####################

#' @description Makes a new blank dataframe with columns full of NA.
#' Either: geom is set to something like a simple feature, OR the n value
#' will be used to set the number of rows
blankdf = function(columns, n=NULL, geom=NULL){
  if(isset(geom)){
    n = nrow(geom)
  }

  m = length(columns)
  mdata = matrix(NA, nrow=n, ncol=m)

  df = data.frame(mdata)
  colnames(df) = columns

  if(isset(geom)){
    coords = sp::coordinates(geom)
    df$long = coords[,1]
    df$lat = coords[,2]
    df = st_as_sf(df, coords=c("long","lat"))
  }

  return(df)
}

mergecov = function(row, covnames, covData, tcols = c("month", "year"), scol="geometry"){
  # get the monthly data
  loc = row[[scol]]

  # chain filter the data
  covs = covData
  for(tcol in tcols){
    # covs[covs[["year"]] == 2015,]
    covs = dplyr::filter(covs, covs[[tcol]] == row[[tcol]])
  }
  if(nrow(covs) == 0){
    return(NA)
  }

  # find the nearest points in space
  nearestid = st_nearest_feature(loc, covs)
  # grab the matching row
  nearest = covs[nearestid,]
  # get the covariates
  result = c()
  for(cov in covnames){
    result = c(result, nearest[[cov]])
  }
  return(result)
}

#' @description add covariates to a dataframe
#'
#' NOTE: all covariates are automatically scaled by default
addCovariates = function(df, covData,
                         scols = c("long","lat"),
                         scols2 = c("long","lat"),
                         tcols = c("year","month"),
                         tcols2 = c("year","month"),
                         covcols=c("tas","huss","pr","prsn"),
                         renameCols=c(temperature="tas", humidity="huss", rain="pr", snowflux="prsn"),
                         scale=FALSE,
                         crsOf=NULL){
  # use vectorised applylookup to copy things over
  pk = c(scols, tcols)
  pk2 = c(scols2,tcols2)
  df = applyLookupv(df, pk, covData, pk2, colsToTransfer=covcols)

  # scale if requested
  if(isset(scale) & scale==TRUE){
    info("scaling")
    df = scaleDF(df, covcols)
  }

  # rename columns now
  if(isset(renameCols)){
    df = rename(df, all_of(renameCols))
  }

  if(isset(crsOf)){
    df = st_as_sf(df, coords=scols)
    st_crs(df) = st_crs(crsOf)
  }

  return(df)
}

nearest = function(points, reference){
  # find the nearest points in space
  nearestid = st_nearest_feature(points, reference)
  nearest = reference[nearestid]
  # get the covariates
  return(nearest)
}

within = function(points, reference){
  # find the nearest points in space
  contained = st_contains(points, reference)
  # get the covariates
  return(contained)
}

#' @description
#' adds the spatial coordinates of the nearest covariate point to the case
#' data. Aligning the spaces for future lookups
#' For simplefeatures. With sparse matrix, no missing times
#' or places.
alignGeom = function(cases, covs, alignFunction=nearest){
  # first, convert the coordinates to covariate space, one per location
  caselocs = st_geometry(cases)
  covlocs = st_geometry(covs)

  # get the nearest coord for each location
  matches = do.call(what=nearest,
                 args=list(points=caselocs, reference=covlocs))

  # repeat the ids in all of the future case data.
  # NOTE: the order of future should be fine, as just created. allcov order should not affect things as it was matched to the order in future.
  alignedCoords = st_coordinates(matches)
  cases$long=alignedCoords[,1]
  cases$lat=alignedCoords[,2]

  # REMOVED THE UNIQUE LOOKUP THEN REPEATS
  #repeats = nrow(cases) / nrow(geomlookup)
  #cases$long = rep(alignedCoords[,2], repeats)
  #cases$lat = rep(alignedCoords[,1], repeats)

  return(cases)
}

#' @description
#' Takes an sf with points and an areal sf. Points which fall in an area are given
#' the areas id. The geometry is dropped, regional mean calculated and returned
#' as a data frame with no geometry, but with the areal id.
#'
#' WARNING: if the data contains columns which are not in covCols then this will
#' form part of the key when taking the region average, rather than a column to be
#' averaged. This results in an overly large and undesired result.
groupGeom = function(regions, covs, scol, covCols=c("tas","huss","pr","prsn")){
  # first, get the unique spatial locations from the covariate dataset
  #covlocs = st_as_sf(data.frame(unique(st_coordinates(covs))), coords=c("X","Y"))
  #st_crs(covlocs) = st_crs(regions)
  columnNames = colnames(covs)

  # find out which region contains which points
  contained = st_contains(regions, covs)

  # ad an id to each location in the covariates, corresponding to it's region
  n = nrow(regions)
  covs[[scol]] = rep(NA, nrow(covs))
  for(i in 1:n){
    gid = regions[[scol]][i]
    print(gid)
    covs[contained[[i]],][[scol]] = gid
  }

  # drop the covariate geometry, it's been replaced with region id
  covs = st_drop_geometry(covs)

  # now condense the covs to take the mean values in a region
  pk = colnames(covs)[!colnames(covs) %in% c(covCols, scol)]
  groupedCovs = groupSummary(covs, c(scol, pk), covCols)

  return(groupedCovs)
}

#########
# PLOTS
#########

# Currently plotly won't show up in markdown HTML if generated inside a loop or function
# the alternative is to set results to "asis" and translate to HTML, here are some
# convenience methods for that

toHTMLlist = function(plots){
  html = htmltools::tagList()
  count = 1
  for(p in plots){
    html[[count]] = toHTML(p) # tagList(HTML(markdown::mark(text=paste0("\n\n#### ", count, "\n"))), p)
    count = count+1
  }
  return(html)
}

toHTML = function(plot, title){
  html = tagList(p)
  return(html)
}


################################

# Plot a matrix as a heatmap.
plotHeatmap = function(dataMatrix, useNames=TRUE){
  nx = nrow(dataMatrix)
  ny = ncol(dataMatrix)

  rowNames = rownames(dataMatrix)
  colNames = colnames(dataMatrix)

  xs = 1:nx
  ys = 1:ny

  if(useNames == TRUE & is.null(rowNames)){
    xs = rowNames
  }
  if(useNames == TRUE & is.null(colNames)){
    ys = ccolNames
  }

  heatmap = plot_ly(type="heatmap",
                    x=as.factor(xs),
                    y=as.factor(ys),
                    z=dataMatrix,
                    colors="Purples"
  )

  return(heatmap)
}

plotHeatmapDF = function(data, use="everything"){
  corr = reshape2::melt(round(cor(data,use=use),2))
  ggplot(corr, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile() +
    geom_text(aes(label=value), color="white", size=2.5) +
    scale_fill_distiller(palette="Purples") +
    xlab("") +
    ylab("") +
    ggtitle("Correlation Heatmap") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
}

# works with SF objects
plotRegion = function(regions, regId, idCol="gid"){
  regions$target = regions[[idCol]] == regId
  plt = ggplot(regions) +
          geom_sf(aes(fill=target), lwd=0) +
          scale_fill_discrete(type=c("grey","salmon"), guide="none") +
          ggtitle(paste("Region",regId)) +
          theme_bw()
  return(plt)
}

# Might be for spatialdataframes, deprecated because they are a pain
plotRegions = function(regions, fillBy){
  # convert the polygons into a full list indexed by something useful
  polys = fortify(regions)

  # augment said list taking columns from the original data and matching them
  # to all the relevant points in the polygon data
  lookupDF = data.frame(list(
    "id"=unique(polys$id),
    #"CC_2"=regions$CC_2,
    "fillBy"=regions[[fillBy]]
  ))


  polys$fillBy = lookupDF$fillBy[match(polys$id, lookupDF$id)]

  # plot the polygons
  plt = ggplot(data=polys, aes(x=long, y=lat, group=id)) +
    geom_polygon(aes(fill=fillBy)) +
    xlab("Longitude") +
    ylab("Latitude") +
    labs(fill=fillBy)
  theme_bw()

  # send the plot back
  return(plt)
}

plotNeighbours = function(targetID, regions){
  # get the neighbours of a particular municipality
  target = as.character(targetID)
  nn = names(which(adj[,target] > 0))

  targetMunicipalities = subset(municipalities, municipalities$CC_2 %in% as.integer(c(target, nn)))
  isTarget = targetMunicipalities$CC_2 == as.integer(target)

  return(plotRegions(targetMunicipalities, isTarget))
}


plotTimeSpace = function(data, column, columnRename){
  perDay = groupSummary(data, c("ym"), c(column), c("date", columnRename))
  perLoc = groupSummary(data, c("LONGITUDE","LATITUDE"), c(column), c("long","lat", columnRename))

  plots = list()

  plots[[1]] = plotly_build(plot_ly(type="bar", x=perDay$date, y=perDay[c(column)]))

  plots[[2]] = plotly_build(plot_ly(type="scatter", mode="markers",
                                    x=perLoc$lat,
                                    y=perLoc$long,
                                    marker = list(color = perLoc[c(column)], showscale=TRUE))
  )

  return(plots)
}

plotS = function(data, casesCol="cases", latCol="lat", longCol="long"){
  cases = data[[casesCol]]
  lat = as.numeric(data[[latCol]])
  long = as.numeric(data[[longCol]])
  fig = plot_ly(type="scatter", mode="markers",
                x=long,
                y=lat,
                color=cases,
                colors="Reds",
                marker=list(
                  opacity=cases,
                  size=2+normalise(cases)*5
                ),)
  return(fig)
}

plotT = function(data, casesCol="cases", time="t", location="s"){
  cases = data[[casesCol]]
  times = data[[time]]
  location = data[[location]]
  fig = plot_ly(type="scatter", mode="lines",
                x=times,
                y=cases,
                color=location,
                #colors="Reds",
                marker=list(
                  opacity=cases,
                  size=2+normalise(cases)*5
                ),)
  return(fig)
}

plotST = function(data, valCol="cases", frameCol=NULL, latCol="lat", longCol="long",
                  yearCol="year", monthCol="month", title=""){

  useDate = !isset(frameCol)
  frame = NULL

  if(useDate == TRUE){
    date = toDate(paste(data[[yearCol]], "-", data[[monthCol]], "-01", sep=""))
    frame = date
  }
  else{
    frame = data[[frameCol]]
  }

  cases = data[[valCol]]

  fig = plot_ly(type="scatter", mode="markers",
                x=as.numeric(data[[longCol]]),
                y=as.numeric(data[[latCol]]),
                color=cases,
                colors="Reds",
                marker=list(
                  opacity=cases,
                  size=2+normalise(cases)*5
                ),
                frame=as.numeric(frame))
  fig = animation_opts(fig, frame=25, transition=0, easing='linear')
  fig = animation_slider(fig, value=as.character(frame))
  fig = layout(fig, plot_bgcolor='gray', title=title)
  return(fig)
}

# Using SF and dataframes with geometry column
smap = function(data, targetYear, sf, column="cases"){
  cases = data[[column]]
  yearData = filter(data, year == targetYear & cases > 0)

  cases = yearData[[column]]

  plt = ggplot() +
    geom_sf(data=sf) +
    geom_sf(data=yearData, aes(color=cases)) +
    theme_bw()

  return(plt)
}

stmap = function(data, sf, column="cases", timecol="year"){
  cases = data[[column]]
  time = data[[timecol]]

  plt = ggplot() +
    geom_sf(data=sf) +
    geom_sf(data=data, aes(color=cases, frame=time)) +
    theme_bw()

  pltly = ggplotly(plt) %>%
    animation_opts(frame=25, transition=0, easing='linear')
  return(pltly)
}
#########


plotAdjacency = function(area, adj){
  coords = st_coordinates(st_centroid(area))
  x1 = c()
  y1 = c()
  x2 = c()
  y2 = c()
  for(i in 1:nrow(adj)){
    xfrom = coords[i,1]
    yfrom = coords[i,2]

    neighbours = adj[[i]]
    for(n in neighbours){
      xto = coords[n,1]
      yto = coords[n,2]

      x1 = c(x1, xfrom)
      y1 = c(y1, yfrom)
      x2 = c(x2, xto)
      y2 = c(y2, yto)
    }
  }
  id = 1:length(x1)
  neighbourLines = data.frame(id,x1,y1,x2,y2)

  ggplot(area) +
    geom_sf() +
    geom_segment(data=neighbourLines, aes(x=x1,y=y1,xend=x2,yend=y2), color=id) +
    theme_bw()
}
