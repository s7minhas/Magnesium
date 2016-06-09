echo '\nStarting replication, clearing out existing files.\nAll output will be stored in Graphics directory.\nIn parentheses, we list the location of the graphic in the paper.'

rm -rf Graphics/*.pdf 
rm -rf Graphics/*.tex

echo '\nRecreating Figure 1 in paper. Output stored in: \n\t84net.pdf and \n\tMapLegend.pdf (Figure 1)'

Rscript sancNet.R 2>&1 >/dev/null

echo '\nRecreate plots for Figure 2. Output stored in: \n\tcompNet_1972.pdf (Figure 2a), \n\tcompNet_1992.pdf (Figure 2b), \n\tand compNet_2012.pdf (Figure 2c).'

Rscript recipNet.R 2>&1 >/dev/null

echo '\nRun duration analysis on original and imputed data. \n\tRegression table based on:  \n\t\timputed data stored in durModelResults_all.tex (Table 2)  \n\t\tand original data in durModelResultsNoImp_all.tex (Table 3). \n\tAdditionally, descriptive statistics: \n\t\tfor analysis with imputed data stored in summStatsImp_all.tex (Table 4) \n\t\tand for analysis with original data stored in summStatsNoImp_all.tex (Table 5). \n\tSurvival plot for: \n\t\tGDP per capita stored in gdpSurv_all.pdf (Figure 3), \n\t\tnumber of senders stored in nosSurv_all.pdf (Figure 4), \n\t\tcompliance reciprocity stored in compRecSurv_all.pdf (Figure 5a), \n\t\tand sanction reciprocity stored in sancRecSurv_all.pdf (Figure 5b).'

Rscript durModels.R 2>&1 >/dev/null

echo '\nRun cross-validation and out-of-sample performance analysis. \n\tCrossvalidation results stored in crossvalCoef.pdf (Figure 6). \n\tOut-of-sample performance results stored in crossvalPerf.pdf (Figure 7).'

Rscript crossval.R 2>&1 >/dev/null

echo '\nRun duration model on non-economic sanctions. \n\tRegression table stored in durModelResultsNoImp_secur.tex (Table 6).\n\n'

Rscript durModels_nonEconSanctions.R 2>&1 >/dev/null

rm -rf Rplots.pdf

echo 'Replication complete, check Graphics directory for outputs.\n'