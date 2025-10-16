#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
A group of progress bars with the same maxiumum value, useful
for showing overall progress with a breakdown of other sub-processes.
i.e. showing overall progress as well as successes and failures.

Created on Wed Nov 30 14:13:46 2022

@author: matt
"""
import ansi
from ansi import *
from ansi.colour import fg, bg, rgb
from ansi.colour.fx import *
from ansi.colour.base import Graphic
import math

class Progress:
    # Some common convenience colours
    WHITE = ansi.colour.rgb.rgb256(255, 255, 255)
    BLACK = ansi.colour.rgb.rgb256(0, 0, 0)
    RED = str(ansi.colour.fg.truecolor(200, 50, 50))
    GREEN = str(ansi.colour.fg.truecolor(50, 200, 50))
    BLUE = str(ansi.colour.fg.truecolor(50, 50, 200))
    
    bars = None # string names for each bar
    numBars = None # the number of progress bars
    progress = None # the current progress of each bar
    barMax = None # the maximum value of all bars
    width = 100 # the "pixel" size. How wide each chunk of progress is.
    colours = None
    fillChar = '#'
    
    # Makes a number of progress bars, defined by their display names, with
    # the same maximum value. [0,max]. defaults to a single total bar [0,100]
    def __init__(self, barNames=["Total"], colours=[WHITE], barMax=100):
        
        self.bars = barNames
        self.numBars = len(self.bars)
        self.progress = [0]*self.numBars
        self.barMax = barMax
        
        if len(colours) == self.numBars:
            self.colours = colours
        else:
            self.colours = [self.WHITE]*self.numBars
            print("Error: incorrect number of colours given, defaulting to white")
        
        self.update([0]*self.numBars)
        
    # resets the current progress of all bars to zero
    def reset(self):
        self.progress = [0]*self.numBars
        
    # updates the progress values of each bar
    def update(self, progress=0):
        # if a single number is given, marshall into a list
        if isinstance(progress, int):
            progress = [progress]
            
        found = len(progress)
        if found != self.numBars:
            print(f"Error: the wrong number of progress values was given in update(). {self.numBars} expected, {found} found.")
            
        # overwrite the progress
        self.progress = progress
        
    # update the first progress bar by incrementing by 1 (int)
    def increment(self):
        updated = []
        for bar in range(0, self.numBars):
            updated.append(self.progress[bar] + 1)
        self.update(updated)
        
    # Returns the current progress bars as a single string, used for printing
    def __str__(self):
        text = ""
        for i in range(0,self.numBars):
            progress = self.progress[i]
            percent = (progress / self.barMax) # range [0,1]
            filled = min(self.width, math.floor(self.width * percent)) # same percent of pixels as progress
            empty = self. width - filled
            
            text += self.bars[i] + "\t[" + self.colours[i] + self.fillChar*filled + str(self.WHITE) + " "*empty + "]\t" + f"{progress}/{self.barMax} ({math.floor(percent*100)}%)\n"
            
        return text
    
    # Makes a printable colour object which can be used to set a progress bar's colour
    def rgb(r, g, b):
        return str(ansi.colour.fg.truecolor(r, g, b))
    
    # Sets the colour of a specific bar. zero based index
    def colour(self, index, colour):
        self.colours[index] = colour