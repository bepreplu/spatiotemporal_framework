#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 16 13:16:31 2023

@author: matt
"""
import re
import os
import pandas as pd
import numpy as np
import netCDF4 as nc
from gui import GUI
from progress import Progress
from datetime import date
from geoutil import GeoUtil

util = GeoUtil()
    

# global cache for getting monthly values
monthly = {}

def getMonthlyValue(fileInfo, variable, time, lat, lon):
    # get the value from the file info and variable provided
    # converting coordinates and times as appropriate

    # convert these to indexes in the eco data
    latIndex = hashIndex(fileInfo['minlat'], fileInfo['latstep'], lat)
    lonIndex = hashIndex(fileInfo['minlon'], fileInfo['lonstep'], lon)
    start = util.timeIndex(fileInfo, date(time.year, time.month, 1))
    end = util.timeIndex(fileInfo, date(time.year if time.month < 12 else time.year+1, 
                                   time.month+1 if time.month < 12 else 1, # loop from dec to jan
                                   1)) # one beyond the end (start of next month)
   
    value = None # default if an error pops up
    
    # either get from cache or do the whole averaging thing
    key = f"{variable}-{latIndex}-{lonIndex}-{start}"
    if key in monthly:
        value = monthly[key]
    else:
        total = 0
        n = 0
        try:
            # get the monthly average for the value
            tIndex = start
            while(tIndex < end): # 1 beyond the end indexing (don't add next month day 1 to average)
                total = total + fileInfo['file'][variable][tIndex, latIndex, lonIndex]
                n += 1
                tIndex += 1
            
        except(IndexError):
            print(f"No value found for {variable} at: [{time},{lat},{lon}]")
            
        value = total / n # work out the monthly average for the value
        # cache the value
        monthly[key] = value
        
    # return the final value found (monthly average from cache, caluclation or NONE)
    return value

def longTableIndex(locs, years, lat, long, year, month):
    
    # check the dimensions
    nlocs = locs.shape[0]
    
    # get the longtable index from the values of a row
    locIndex = locs[(locs['LONGITUDE']==long) & (locs['LATITUDE']==lat)].index.values[0] # first match
    monthIndex = month-1
    yearIndex = years.index(year)
    
    # go to start of correct year. years are repeated for every location per month
    startOfYear = nlocs * 12 * yearIndex
    startOfMonth = nlocs * monthIndex # zero-based index
    rowIndex = startOfYear + startOfMonth + locIndex
    
    return rowIndex

# Expand the table given to have every space time coordinate available
# as well as all of the columns in the given table, but without any data.
# This needs to be added later using another approach.
def expandTable(table, spaceCols, timeCol, addCols):
    # get all of the unique spatial coordinates
    locs = util.unique(cases, spaceCols)
    
    # spatial
    nlocs = locs.shape[0]

    # temporal
    years = list(pd.DatetimeIndex(table[timeCol]).year.value_counts().reset_index()['index'])
    years.sort()
    months = [1,2,3,4,5,6,7,8,9,10,11,12] # need an explicit list
    nmonths = 12
    ntimes = len(years) * nmonths

    # total
    n = nlocs * ntimes

    # make a new table with the time columns (more fiddly)
    longTable = pd.DataFrame({
        'year' : np.repeat(years, nlocs*nmonths),
        'month' : list(np.repeat(months,nlocs)) * (len(years)),
        })
    # don't forget the time column itself - bad to assume a date column?
    # This is the monthly aggregation date, not the original. Matches up with the aggregation later
    longTable['date'] = pd.to_datetime({'year':longTable['year'],'month':longTable['month'],'day':[1]*longTable.shape[0]}, yearfirst=True)
    
    # add the spatial columns
    for col in spaceCols:
        longTable[col] = list(locs[col]) * ntimes
    
    # For columns which are not part of the expansion - add with defaults
    leaveOutCols = spaceCols + [timeCol]
    for col in list(cases.columns) + list(addCols):
        # for any columns not part of the space time expansion, put in
        # sensible defaults to override later
        if not col in leaveOutCols:
            longTable[col] = [''] * n
            
    # send back the long form blank of the table
    return [longTable, locs, years]


def copyData(table, longTable, latCol, lonCol, timeCol, sumCols, avgCols):
   # TODO - make this as vectorised as possible
    return None
    

# includes elements of expand and copy table into a function which samples
# environmental data and aggregates to monthly averages.
def processFile(manifest, cases, timeCol, latCol, lonCol, progress, gui):
    
    # define our aggregation - and some data structures too
    sumCols = ['totaldecasos']
    avgs = {}

    # expand the table into long form
    spaceCols = [latCol,lonCol,'idmun','GEOCODIGO_MUNICIPIO','NOME_MUNICIPIO','idmun6']
    expansion = expandTable(cases, spaceCols, timeCol, manifest.keys())
    longTable = expansion[0]
    places = expansion[1]
    years = expansion[2]
    
    # note the columns which are not part of the space-time calculation or environmental data
    skipCols = list(manifest.keys()) + spaceCols + [timeCol]
    copyCols = list(set(cases.columns) - set(skipCols))
    
    # metrics for iterating through the data - used to be cases, now longTable
    it = longTable.iterrows()
    ncases = longTable.shape[0]
    casesPercent = ncases / 100
    
    # open all of the files in the manifest and store them in a similar
    # structure. Also create the result columns to be filled
    newCols = {}
    files = {}
    for varName in manifest:
        files[varName] = fileInfo(manifest[varName])
        newCols[varName] = [None] * ncases
        
    # Augment every row in the case data
    finished = False
    row = None
    i = -1 # immediately gets bumped to zero based index on first iteration
    while finished == False:
        try:
            # User feedback setup
            progress.increment()
            gui.draw()
            
            # get the next row, an exception is thrown when no more elements are available
            i = i+1
            row = next(it)
            
            # get the lat long and time 
            lon = row[1][lonCol]
            lat = row[1][latCol]
            time = row[1]['date'] # added during expansion - already aggregated to year month
            year = time.year
            month = time.month
            
            # get the long table index
            tableIndex = longTableIndex(places, years, lat, lon, year, month)
            
            # sample each available variable from the eco data
            for variable in manifest.keys():
                # add the value to the appropriate row
                value = getMonthlyValue(files[variable], variable, time, lat, lon)
                newCols[variable][i] = value
                
                # update the averages information for each space-time-variable
                stvKey = str(tableIndex) + "-" + variable
                
                if stvKey not in avgs:
                    # the pair is the total and the number of elements contributing
                    avgs[stvKey] = [value,1]
                else:
                    # add to the total an increment the count
                    old = avgs[stvKey]
                    avgs[stvKey] = [old[0] + value, old[1] + 1]
                
                # replace the current monthly average for each variable in long table
                newAvg = avgs[stvKey][0] / avgs[stvKey][1]
                longTable.loc[tableIndex,variable] = newAvg
            
            # copy the remaining columns over to the long table
            for col in copyCols:
                # some columns should be simply added when being copied over
                # to become monthly totals.
                if col in sumCols:
                    if type(longTable.loc[tableIndex,col] == str):
                        longTable.loc[tableIndex,col] = row[1][col]
                    else:
                        longTable.loc[tableIndex,col] = longTable.loc[tableIndex,col] + row[1][col]
                else:
                    # most columns can just be copied over
                    longTable.loc[tableIndex,col] = row[1][col]
        except(StopIteration):
            finished = True
        
    # add every column to the dataset
    for variable in manifest.keys():
        longTable[variable] = newCols[variable]
        
    return [cases, longTable]
        


###############
# CODE BEGINS
###############
# Prepare a simple progress gui
gui = GUI()

# make an instance of our little utility
util = GeoUtil()

# define some columns
timeCol = "diamesano"
latCol = "LATITUDE"
lonCol = "LONGITUDE"

# Load the case data in
caseDataPath = "../../geostats/snakebite/data/CasosPorDiaMesAno.csv"
cases = pd.read_csv(caseDataPath, encoding="ISO-8859-1", decimal=',', 
                    index_col=0, dtype={'LATITUDE':str, 'LONGITUDE':str})

# pre-process the data
cases[latCol] = cases[latCol].str.replace(",",".")
cases[lonCol] = cases[lonCol].str.replace(",",".")
cases = cases.loc[cases['tipo']==1] # only snakebites
cases[timeCol] = pd.to_datetime(cases[timeCol]).dt.date
#cases = cases.apply(rowDate, axis=1)
cases = cases.loc[cases[timeCol]>=util.strDate("2007-01-01")]
cases.reset_index(inplace=True)
cases = cases.drop('index', axis=1)

# calculate the length of the longTable
#ncases = cases.shape[0]
nlocs = util.unique(cases, [latCol,lonCol]).shape[0]
ntimes = 12 * 14 # year month combo, not calculated yet
ncases = nlocs * ntimes

# set the folder we will use to pull eco data from
isimipDataFolder = "../../PHP/ISIMIP/Downloads"                                                                                                  

# make the progress bar
progress = Progress(["Total"], [Progress.BLUE], barMax=ncases)
gui.add(progress)


# Get just the files in the given folder
manifest = {}
for filename in os.listdir(isimipDataFolder):
    path = f"{isimipDataFolder}/{filename}"
    if os.path.isfile(path):
        variable = re.search("_([a-z]+)_global_daily", filename)[1]
        
        if variable not in manifest:
            manifest[variable] = []
        
        manifest[variable].append(path)

# process all of the files
tables = processFile(manifest, cases, timeCol, latCol, lonCol, progress, gui)

cases = tables[0]
longTable = tables[1]

cases.to_csv("./cases.csv")
longTable.to_csv("./cases_longform.csv")