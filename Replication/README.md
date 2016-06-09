Replication file for "When Do States Say Uncle? Network Dependence and Sanction Compliance". Code to replicate analysis presented in paper is shown in the following files:

* replicate.sh - this is a shell script that will run each of the R scripts necessary for the replication.
* Setup.R - this file loads in libraries and functions necessary to run the R scripts.
* sancNet.R - this script generates the sanction network and geographic legend shown in figure 1 of the paper.
* recipNet.R - this script generates the compliance reciprocity plots for select countries in 1972, 1992, and 2012 shown in figure 2 of the paper.
* durModels.R - this script generates the regression tables, summary statistic tables, and survival plots presented in the results section of the paper.
* crossval.R - this script generates the crossvalidation and out of sample performance analysis presented in the results section.
* durModels_nonEconSanctions.R - this script runs a duration model on a database focusing on sanctions related to security issues.

Files necessary to run each of these scripts are stored in the data directory.
