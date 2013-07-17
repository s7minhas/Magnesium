# Need to pull out sanction variable for use in analysis
# Clearing workspace
rm(list=ls())
# Setting working directory
if(Sys.info()["user"]=="janus829")
{pathMain="~/Desktop/Research/Magnesium/R";
	pathGraphics="~/Dropbox/Research/Magnesium/Graphics";
	pathFunctions="~/Desktop/Prog Notes/R Functions";
	pathData="~/Dropbox/Research/Magnesium/Data"}


if(Sys.info()["user"]=="cassydorff")
{pathMain="~/ProjectsGit/Magnesium/R";
	pathGraphics="~/Dropbox/My Research/Magnesium";
	pathData="~/Dropbox/My Research/Magnesium/Data"}

# Loading libraries and functions
require(ggplot2)
theme_set(theme_bw())
require(reshape)
require(foreign)
require(cshapes)
require(countrycode)

# Additional functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)