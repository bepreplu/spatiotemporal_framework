#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 16:03:00 2023

@author: matt
"""
import re
import os
import cftime
import datetime
import pandas as pd
import geopandas as gpd
import numpy as np
import netCDF4 as nc
import xarray as xr
import regionmask
import matplotlib.pyplot as plt
from gui import GUI
from progress import Progress
from geoutil import GeoUtil
from datetime import date
from pathlib import Path

def plotatime(data, colname, timet):
    data[colname][timet,...].plot()

def tocsv(path, country, aggby, timeUnit=None):
    # read the file
    isimip = None
    
    # Can manually generate a time column if the dataset is causing errors
    if(timeUnit):
        isimip = xr.open_dataset(path, engine=('netcdf4'), decode_times=False)
        times = isimip['time'].to_numpy()
        
        # a catch-all workaround for weird date encodings.
        since = isimip.time.attrs['units'].split('since')[1]
        since = str.strip(since)
        
        timeFormat = "%Y-%m-%d %H:%M:%S"
        startTime = cftime.datetime.strptime(since, timeFormat)
        minTime = pd.Timestamp.strftime(pd.Timestamp.min, timeFormat)
        minTime = cftime.datetime.strptime(minTime, timeFormat)
        
        offset = minTime - startTime
        
        # easiest way to handle the weird mishandling of years is to make one more 
        # than needed then throw it away. Harder to increment start date by the years
        times = pd.date_range(start=startDate, periods=isimip.sizes['time']+1, freq=timeUnit)
        isimip['time'] = times[1:]
        
    else:
        isimip = xr.open_dataset(path, engine=('netcdf4'))
    
    
    # cut it down, convert to csv and save
    mask = regionmask.mask_geopandas(country, 
                                     isimip['lon'].to_numpy(),
                                     isimip['lat'].to_numpy())
    
    cropped = isimip.where(mask==0, drop=True)
    
    aggregated = None
    if aggby is None:
        aggregated = cropped
    else:
        aggregated = cropped.resample(time=aggby).mean()
    
    dfr = aggregated.to_dataframe().dropna().reset_index()
    
    return(dfr)

#testFile = "../PHP/ISIMIP/Downloads/ipsl-cm6a-lr_r1i1p1f1_w5e5_picontrol_pr_global_daily_2001_2010.nc4"
#test = tocsv(testFile, sweden, monthly)

###############
# CODE BEGINS
###############
# first load all of the shape files that we want to use to restrict the data to
brazilPath = "../../Brazil/Brazil_0.json"
swedenPath = "../../Sweden/gadm36_SWE_0.shp"
columbiaPath = "../../../Data/boundries/columbia.json"
brazil = gpd.read_file(brazilPath)
sweden = gpd.read_file(swedenPath) 
columbia = gpd.read_file(columbiaPath)

sweden.rename(columns={'NAME_0':'COUNTRY'}, inplace=True)

countries = [sweden, brazil]

# Prepare a simple progress gui
gui = GUI()

# make an instance of our little utility
util = GeoUtil()

# Setup based on data type

# # for ISIMIP data
# datasetname = "hist-nat/ipsl-cm6a-lr"
# isimipDataFolder = f"/home/matt/Sync/geostats/ISIMIP/PHP/ISIMIP/Downloads/{datasetname}"
# util.useISIMIPRegex()
# sampleTo = "1M"
# timeUnit = None
# pk = ['lat','lon','time']
# separateVars = False

# for population data
# datasetname = "pop"
# isimipDataFolder = "/home/matt/Sync/Data/population"
# util.usePopulationRegex()
# sampleTo = None
# timeUnit = None
# pk = ['lat','lon','year']
# separateVars = True

# for Jose
# datasetname = "hist-nat/gfdl-esm4"
# isimipDataFolder = f"/home/matt/Sync/geostats/ISIMIP/PHP/ISIMIP/Downloads/{datasetname}"
# util.useISIMIPRegex()
# sampleTo = "1M"
# timeUnit = None
# pk = ['lat','lon','time']
# separateVars = False

# Populations
datasetname = "pops"
isimipDataFolder = f"/home/matt/Sync/geostats/ISIMIP/PHP/ISIMIP/Downloads/{datasetname}"
util.filenameVarRegex = "population_([a-z0-9]+)_"
sampleTo = "1Y"
timeUnit = "YS"
pk = ['lat','lon','time']
separateVars = False

# make a manifest of the available files by variable
manifest = util.manifest(isimipDataFolder)

# set the output locations
outFolder = f"./csv/{datasetname}"

# how many calculations need to be done
nsteps = len(countries) * util.manifestLength(manifest)

# make the progress bar
progress = Progress(["Total"], [Progress.GREEN], barMax=nsteps)
gui.add(progress)       
txtvar = ""
gui.add(txtvar)

#
# Main loop
#
for country in countries:
    # make a dataset for each country
    countryData = None
    countryName = country['COUNTRY'][0]
    # add each variable to it from the manifest
    for var in manifest.keys():
        txtvar = var
        # Track each data by variable
        varData = None
        # Process each file - one per time period (hopefully no overlap)
        for filename in manifest[var]:
            # Get the csv for this var/country/space/time
            data = tocsv(filename, country, sampleTo, timeUnit=timeUnit)
            # combine it per country into one big csv
            if varData is None:
                varData = data
            else:
                # add rows to the table (hopefully no overlaps)
                varData = pd.concat([varData, data])
                    
                
            # update the UI
            progress.increment()
            gui.draw()

        # Disable the writing of partial files, more for debugging now.
        if(separateVars):
            # save the combined csv for the variable
            folder = f"{outFolder}/{countryName}"
            os.makedirs(folder, exist_ok=True)
            varData.to_csv(f"{outFolder}/{countryName}/{var}.csv")
        
        # combine the var data by country
        if countryData is None:
            countryData = varData
        else:
            countryData = pd.merge(countryData, varData, 
                                   how='outer', 
                                   on=pk)
        
    if(not separateVars):
        Path(outFolder).mkdir(parents=True, exist_ok=True)
        countryData.to_csv(f"{outFolder}/{countryName}.csv")
        
        