# Need to pull out sanction variable for use in analysis
# Clearing workspace
rm(list=ls())

# Setting working directory
if(Sys.info()["user"]=="janus829")
{pathMain="~/Desktop/Research/Magnesium/R";
	pathGraphics="~/Dropbox/Research/Magnesium/Graphics";
	pathFunctions="~/Desktop/Prog Notes/R Functions";
	pathData="~/Dropbox/Research/Magnesium/Data"}

# Loading libraries and functions
library(ggplot2)
theme_set(theme_bw())
library(reshape)
library(foreign)