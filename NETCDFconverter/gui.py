#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Provides a simple UI, based on the CLS ANSI.
Will show a string based gui, redrawn on command or using print.
Any updating component can be added to the screen redrawing, provided that
it implements __str__.

Created on Wed Nov 30 17:36:12 2022

@author: matt
"""
from progress import Progress
import time
import math

class GUI:
    
    CLS = "\u001b[2J" # clears the console so that we can draw the whole ui
    elements = None # holds all of the elements of the ui in draw order.
    ui = "" # a cached single string version of the ui
    
    # Makes a new simple text gui - made for Ipython in spyder
    def __init__(self):
        self.elements = [GUI.CLS]
        
    # Adds a new element to the drawn elements. These may be components that
    # update on every draw, or static text, some ansi commands are also supported ootb.
    def add(self, element):
        self.elements.append(element)
        self.draw()
    
    def draw(self):
        for element in self.elements:
            print(element, end="", flush=True)
            
            
    def defaultProgress(self, maximum):
        return Progress(barNames=["Total", "Success", "Failure"], 
                     colours=[Progress.WHITE, Progress.GREEN, Progress.RED], barMax=maximum)
        