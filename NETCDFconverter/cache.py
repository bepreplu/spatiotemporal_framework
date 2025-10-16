# A few handy little utilities

# import the needed libraries
import pandas as pd
from os.path import exists
from os import rename
from os import remove


# A cache helper for pandas dataframes. can be disabled so that all future
# calls will be ignored until enabled again, this can reduce the number of 
# checks required by code which uses the cache. Assumes well-behaved csv files.
class Cache:
    
    # the root folder which we will cache everything in
    folder = None
    
    # A flag which controls whether the cache will do anything when methods are called
    enabled = True
    
    # A flag for setting whether the cache should output progress to the console.
    verbose = True
    
    # A simple constructor which sets the cache folder
    def __init__(self, folder):
        self.folder = folder
        
        
        
    # We can disable the cache to that future calls will be ignored.
    # This lets us turn the cache on and off from other code without having
    # to do any tests.
    def setEnabled(self, enabled):
        self.enabled = enabled
        
    # Changes whether the cache will output progress updates to the console
    def setVerbose(self, verbose):
        self.verbose = verbose
        
        
    
    # Adds a file to the cache, overwriting is off by default.
    # tries to be as atomic as possible to prevent partial writes
    def cache(self, data, filename, overwrite=False):
        
        # make the full path
        tempPath = f"{self.folder}/__tempoutput__"
        path = f"{self.folder}/{filename}.csv"
        
        # write the file if appropriate
        shouldWrite = overwrite or not exists(path)
        if self.enabled and shouldWrite:
            # begin writing the file (non-atomic so interrups result in partial output)
            data.to_csv(tempPath)
            # rename the file to the correct one (atomic generally if new file doesn't exist)
            if exists(path):
                remove(path)
            
            # now do the atomic rename
            rename(tempPath, path)
            
            if self.verbose:
                print(f"Cached file: {path}")
            
            
            
    # reads a file from the cache, returns none if the file does not exist
    # or the cache is currently disabled.
    def decache(self, filename):
        
        # make the full path
        path = f"{self.folder}/{filename}.csv"
        
        # return None if we shouldn't or can't read the file
        if not self.enabled or not exists(path):
            return None
        # or read the file and return the dataframe
        else:
            if self.verbose:
                print(f"Loaded from cache: {path}")
                
            return pd.read_csv(path, index_col=0, header=0)# remove the rownums
            