#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 16 13:12:58 2023

@author: matt
"""
import os
import re
import numpy as np
import pandas as pd
import geopandas as gpd
import netCDF4 as nc
import rasterio
from shapely import wkt
from shapely.geometry import Point, Polygon
from pyproj import Transformer
from datetime import date

class GeoUtil:
    
    #
    # Geo tables 
    #
    
    def unique(self, data, cols):
        counts = data[cols].value_counts().reset_index()
        return counts[cols]
    
    def toTuples(self, lat, long):
        coords = list(zip(lat, long))# make into list of tuples
        return coords
    
    #
    # Raster helpers
    #

    def project(self, lat, long):
        # define our crs for UTM 24S 
        utm24s = 'epsg:32724'
        wsg84 = "epsg:4326"
        
        # project the points into the correct projection
        transformer = Transformer.from_crs(wsg84, utm24s)
        coords = transformer.transform(lat.tolist(), long.tolist())# list from pd column
        return coords[0], coords[1]

    # Samples a raster at a given set of lat/long coordinates.
    # These will be projected onto UTM 24S, which is what the raster should be in.
    # Note: Only works with lat long, even though the crs is 24S.
    def sampleRaster(self, coords, imagePath):
        
        utm = rasterio.crs.CRS({'init': 'epsg:32724'})

        #print(f"Loading {imagePath}.")
        with rasterio.open(imagePath) as img:
            #print(img.crs)
            
            # sample the tif using the given coordinates
            results = img.sample(coords)
            values = []
            for band in results:
                values.append(band[0])# band 0 is the only one
        
        return values
    
    #
    # Dates
    # 
    
    def toDate(self, row, dateCol='mesano'):
        monthYear = str(row[dateCol])
        parts = re.split("(?=[0-9]{4}$)", monthYear)
        month = parts[0]
        year = parts[1]
        
        return(date(year=int(year), month=int(month), day=1))

    def rowDate(self, row, dateCol='diamesano'):
        row[dateCol] = self.strDate(row[dateCol])
        return(row)

    def strDate(self, stringVal):
        startDateString = re.search('[0-9]{4}.[0-9]{1,2}.[0-9]{1,2}', stringVal)[0] 
        tsep = startDateString[4] # character after year
        tparts = startDateString.split(tsep)
        return(date(year=int(tparts[0]), month=int(tparts[1]), day=int(tparts[2])))
    
    #
    # fileinfo stuff - ncdf meta data parsing largely
    #
    
    def fileInfo(self, filePaths):
        
        info = {}
        # Open the file
        file = nc.MFDataset(filePaths, 'r')
        
        # We need to know what time period the file covers, as well as the extent
        # of the spatial dimensions - required to convert between dataset coorinates
        time = file['time']
        maxt = max(time[0:len(time)])
        mint = min(time[0:len(time)])
        
        lons = file['lon']
        minlon = min(lons[0:len(lons)])
        lonstep = abs(lons[1] - lons[0])

        lats = file['lat']
        minlat = min(lats[0:len(lats)])
        latstep = abs(lats[1] - lats[0])
        
        # now taken from filename and provided elsewhere
        #variables = file.variables.keys() - file.dimensions.keys()
        
        # first datelike match in the string description of the time variable
        tstart = self.strDate(file['time'].units)
        
        # put the values into a dictionary
        info['file'] = file
        info['time'] = time
        info['maxt'] = maxt
        info['mint'] = mint
        info['tstart'] = tstart
        info['lons'] = lons
        info['minlon'] = minlon
        info['lonstep'] = lonstep
        info['lats'] = lats
        info['minlat'] = minlat
        info['latstep'] = latstep
        
        return info
    
    def hashIndex(self, minimum, step, value):
        # value = min + (n * step) therefore:
        n = (float(value) - minimum) / step
        # remainder is essentially distance to each of the nearest n
        # for now just round to nearest integer
        return round(n)

    def timeIndex(self, fileInfo, time):
        tDiff = (time - fileInfo['tstart']).days # for data after tstart no abs needed
        tIndex = self.hashIndex(fileInfo['mint'], 1, tDiff)# 1 day increments. Days from start date
        return tIndex


    # Not currently used
    def getValue(self, fileInfo, variable, time, lat, lon):
        # get the value from the file info and variable provided
        # converting coordinates and times as appropriate

        # convert these to indexes in the eco data
        latIndex = self.hashIndex(fileInfo['minlat'], fileInfo['latstep'], lat)
        lonIndex = self.hashIndex(fileInfo['minlon'], fileInfo['lonstep'], lon)
        tIndex = self.timeIndex(fileInfo, time)
       
        value = None # default if an error pops up
        try:
            # get the value from the nc4 file data
            value = fileInfo['file'][variable][tIndex, latIndex, lonIndex]
        except(IndexError):
            print(f"No value found for {variable} at: [{time},{lat},{lon}]")
            
        return value
    
    
    #
    # Make a manifest from a whole folder
    #
    filenameVarRegex = "_([a-z]+)_global_daily"
    
    def useISIMIPRegex(self):
        self.filenameVarRegex = "_([a-z]+)_global_daily"
        
    def usePopulationRegex(self):
        self.filenameVarRegex = "_(ssp[a-z0-9]+)_"
        
    def useRegex(self, regex):
        self.filenameVarRegex = regex
    
    # Get just the files in the given folder
    def manifest(self, folder):
        manifest = {}
        for filename in os.listdir(folder):
            path = f"{folder}/{filename}"
            shouldAdd = os.path.isfile(path) and path.endswith((".nc",".nc4"))
            if shouldAdd:
                variable = re.search(self.filenameVarRegex, filename)[1]
                
                if variable not in manifest:
                    manifest[variable] = []
                
                manifest[variable].append(path)
        return manifest
    
    def manifestLength(self, manifest):
        length = 0
        for var in manifest.keys():
            length = length + len(manifest[var])
        return length