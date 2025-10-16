library(tidygeocoder)

source("../../../utils/R/utils.R")
unifyWD()
source("helpers.R")
install()

defaultSaveFolder=cacheFolder # covariates will save to cache

# helper functions
toAddress = function(village, country="Sverige"){
  # first just join up the village and country names
  locations = paste(village, country, sep=", ")

  # clean and tidy them up
  locations = stringr::str_replace_all(locations,"NA[, ]*", "")
  locations = stringr::str_replace_all(locations,"\\?", "")
  locations = stringr::str_replace_all(locations,"^[ ]*, ", "")
  locations = stringr::str_replace_all(locations,"[ ]+", " ")

  return(locations)
}

########
# Main
########


# Definitions
fullST = FALSE # all reported data, other combinations are unknown, not zero
redoGeocoding = FALSE
dataFolder = "../data"
csv = ".csv"
scols = c("long","lat")
tcols = c("year","month")
#covariateCols = c("temperature","humidity","rain","snowflux")

# load the data
raw = read.csv(path("raw", folder=dataFolder, extension=".csv"))

# each row is a single case
colnames(raw) = c("disease_unit",
                    "age",
                    "village_home",
                    "date_infection",
                    "date_notification",
                    "country_infection",
                    "village_infection")

# split into two subsets, one for notified dates and villages, one for infection dates and villages
notified = dplyr::select(raw, c("village_home", "date_notification"))
colnames(notified) = c("village", "date")
notified = splitDates(notified)
notified = sortdf(notified, tcols)

# Remove missing data
complete = na.omit(notified)

# There is a gap in the data, take everything after the anomoly
cutoffDate = toTimestamp("1998-01-01") # this removes a single 1997 value, just for scaling the remaining results
after = filter(complete, ts >= cutoffDate)

############
# Geocoding
############
# Process villages into addresses for the geolocator
after$address = toAddress(after$village)
# combine the locations
locations = after$address #c(after$address, infected$address)
# remove unspecified ones
locations = locations[!grepl("[uU]ppgift", locations)]
# get just the unique values for our lookup table
locations = unique(locations)
# turn it into a dataframe
locs = data.frame(list("address"=locations))
# do a bit more cleaning that is easier in this form - remove blanks
locs = filter(locs, !stringr::str_detect(address, "^[ ]*$"))

# geocode or load the address lookup
geocoded = loadOrGenerate(path("geolocations", folder=dataFolder, extension=csv), func=function(){
  coded = geocode(locs, address=address, full_results=TRUE)
  return(coded)
}, overwrite=redoGeocoding)

# use the geolocations as a lookup
notifiedGeo = applyLookupv(after, "address", geocoded, "address", c("lat", "long"))

# remove non geocoded
hanta = na.omit(notifiedGeo)
hanta = select(hanta, all_of(c(scols,tcols)))
hanta = sortdf(hanta, c(tcols,scols))

#
# Improve Geocoding
# has municipalities and typos etc

# So far we have one row per case, with duplicates for each individual
# Make the cases column and aggregate to monthly
hanta$cases = rep(1, nrow(hanta))
hanta = groupSummary(hanta, groupBy=c(scols,tcols),
                     summariseBy=c("cases"),
                     summaryFunction=sum)
hanta = sortdf(hanta, c(tcols,scols))
#
# Long table
#
if(fullST){
  hanta = makeSparse(hanta, spaceCols = scols,
                     timeCols = tcols,
                     timeOverride = list("month"=1:12), # one year month is missing
                     copyCols = c("cases"),
                     repNA=NA)
  # NA here is for point data, remove this as it's unreported. It isn't a zero cases
  # it is unknown?
}

#############
# Covariates
#############

sweden = st_read("../../Sweden/gadm36_SWE_0.shp")
sweden = st_transform(sweden, epsg)
cache(sweden)

info("reading covariates")
scenario = "hist-nat"

allcovs = loadCovariates()
allcovs = allcovs$data[[scenario]]
gcovs = tosf(allcovs, coords=c("long","lat"))
allcovs = dropSF(gcovs,latCol="lat",longCol="long") # transform the non-sf too

info("merging covariates-short")
ghanta = tosf(hanta, coords=scols)
hanta = dropSF(ghanta)
regcovs = alignGeom(ghanta, gcovs) # defaults to nearest
hanta$clong = regcovs$long
hanta$clat = regcovs$lat
hanta = addCovariates(hanta, allcovs, scols=c("clong","clat")) # note - scales by default

disease = tosf(hanta, coords=scols)
assigned = tosf(hanta, coords=c("clong","clat"))

# diagnostic plot to see where our disease and covariates align
ggplot() + geom_sf(data=gcovs, color="steelblue", alpha=.5) +
  geom_sf(data=assigned, color="red", alpha=.5) + theme_bw()



# We can now remove rows that fall outside our available covariate dataset
before = nrow(hanta)
hanta = na.omit(hanta)
after = nrow(hanta)

print(paste(before - after, " rows were removed because they had no covariate data"))


###############
# Save Outputs
###############
# order and add some unique ids
hanta = sortdf(hanta, c(tcols,scols))
hanta = uIndex(hanta, tcols, "ym")

# table
write.csv(hanta, "../data/clean.csv")
print("Data saved to clean.csv")


