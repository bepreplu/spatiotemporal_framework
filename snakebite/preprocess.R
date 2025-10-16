library(tidyverse)
library(sf)
sf_use_s2(FALSE)

verbose = TRUE

source("../../../utils/R/utils.R")

regionNumber = function(charlist){
  return(as.integer(str_match(charlist, "BRA\\.([0-9]+)_1")[,2]))
}

############
# CASE DATA
############

# define our spatiotemporal coordinates
tcols = c("year", "month")
scols = c("GID_1")

# load the ISIMIP augmented snakebite data - Monthly level
snakebites = read.csv("../data/CasosPorDiaMesAno.csv", dec=",")[,-1]
snakebites = dplyr::filter(snakebites, tipo==1) # only snakebites
snakebites = splitDates(snakebites, dateColname="diamesano")
snakebites = dplyr::filter(snakebites, year>=2007) # 2007 onwards

# add the appropriate spatial id, raw data includes CC_2. Need to align with regions
municipalities = st_read("../../Brazil/Brazil_2.json")
municipalities$CC_2 = as.integer(municipalities$CC_2)
snakebites = applyLookupv(snakebites, c("idmun6"), lookupTable=municipalities, lookupCol=c("CC_2"), colsToTransfer=c("GID_1"))

# take only the desired columns and rename as appropriate. make nice
snakebites = select(snakebites, c(tcols, scols, "totaldecasos")) # , "LATITUDE", "LONGITUDE"
snakebites = dplyr::rename(snakebites, cases=totaldecasos) # , lon=LONGITUDE, lat=LATITUDE

# results are still by day so we should aggregate temporally here to match with monthly covariates
snakebites = groupSummary(snakebites, groupBy=c(scols,tcols), summariseBy=c("cases"), summaryFunction=sum)

# add in missing values with zero case observations
snakebites = makeSparse(snakebites, spaceCols=scols, timeCols=tcols, repNA=0)

# note the metrics
ntimes = nrow(unique(snakebites[tcols]))
nlocs = nrow(unique(snakebites[scols]))

# order the dataset
snakebites = sortdf(snakebites, c(tcols, scols))

##########################################
# SF related stuff, add centroid lat long
##########################################

# load the country geo data - here is where we commit to aggregating by region
regions = st_read("../../Brazil/Brazil_1.json")

# make into an sf for later
geomlookup = select(regions, scols, geometry)
snakebites = applyLookupv(snakebites, scols, geomlookup, lookupCol=scols, colsToTransfer=c("geometry"))
snakebites = st_as_sf(snakebites)

# add centroids so we can look for spatial relationships - or via neighbourhood structure?
centroids = st_centroid(snakebites$geometry)
snakebites$long = unlist(lapply(centroids, function(item){return(item[1])}))
snakebites$lat = unlist(lapply(centroids, function(item){return(item[2])}))

# turn back into a csv, but we have lat and long for doing centroid distance calcs
snakebites = sfsuspend(snakebites)

#################
# Add covariates
#################

info("reading covariates")
scenario = "hist-nat"
forcing = "ipsl-cm6a-lr"
covspath = paste("../../ISIMIP/Python/csv/", scenario, "/", forcing, "/Brazil.csv", sep="")
allcovs = read.csv(covspath)[,-1]
allcovs = splitDates(allcovs, dateColname="time")
gcovs = st_as_sf(allcovs, coords=c("lon","lat"))
st_crs(gcovs) = st_crs(regions)

info("merging covariates")
regcovs = groupGeom(regions, gcovs, scol=scols)
snakebites = addCovariates(snakebites, regcovs, scols=scols, scols2=scols)

# Select covariates
covariateCols = c("temperature","humidity","rain")
snakebites = dplyr::select(snakebites, c(scols,tcols,covariateCols,"cases"))
# scaling removed for now
#snakebites = scaleDF(snakebites, covariateCols)

###################
# Add Population(s)
###################

# up to observed data

# mean of projections for baseline...or mid (ssp3?)

#######
# UIDs
#######

# add unique space and time ids, numeric
snakebites$gid = regionNumber(snakebites$GID_1)
snakebites = sortdf(snakebites, c(tcols,"gid"))
snakebites = uIndex(snakebites, tcols, "ym")
snakebites = sortdf(snakebites, c("ym","gid"))

#############################
# Adjacency Matrix / weights
#############################

# load the municipalities geoJSON and calculate the adjacenty matrix
areas = st_read("../../Brazil/Brazil_1.json")
# remove unused municipalities - i.e. not also in our data
areas = areas[(areas[[scols]] %in% unique(snakebites[[scols]])),]


adjacency = function(areas, buffer=0, lonelyfix=NULL){
  # handle the 'degenerate' vertices
  sf_use_s2(FALSE)
  # Calculate the adjacency matrix using a buffer and partial overlapping
  overlaps = st_overlaps(st_buffer(areas, dist=buffer), sparse=TRUE)
  # make it a proper [0,1] sparse matrix
  adj = as.matrix(overlaps) * 1

  # align the adjacency matrix to the data
  rownames(adj) = areas[[scols]]
  colnames(adj) = areas[[scols]]

  # manually add a link between the lonely island and it's mainland counterpart
  if(isset(lonelyfix)){ # TODO: make this flexible and move to utils
    actualNNid = 3550704
    lonely = 3520400
    adj[as.character(actualNNid),as.character(lonely)] = 1
    adj[as.character(lonely),as.character(actualNNid)] = 1
  }

  # return a numeric version of the adjacency matrix, not a logical matrix.
  result = +adj
  # removed this as mixedorder not found and the order is correct for regions. TODO: keep an eye on
  #result[mixedorder(rownames(result)), mixedorder(colnames(result))]
  return(result)
}

# set the buffer and make the adjacency matrix
# buffer = 0.000001
buffer=0.001
adj = loadOrGenerate("../data/adjacency", adjacency, areas, buffer, overwrite=FALSE)

##############
# SAVE OUTPUT
##############

# already sorted. have to use numeric cols for that. gid and ym
write.csv(snakebites, "../data/clean.csv")
print("Data saved to clean.csv")
