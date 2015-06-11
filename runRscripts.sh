echo 'Building data for sanction compliance duration models'

echo '	...Cleaning sanction data'

Rscript R/Data/SanctionDataCleaner.R

echo '	...Building sanction network'

Rscript R/Data/DatabuilderSanctionNetwork.R

echo '	...Building compliance network'

Rscript R/Data/DatabuilderComplianceNetwork.R

echo '	...Calculating SRM measures'

Rscript R/Data/srmSanc.R

echo '	...Building duration data'

Rscript R/Data/DatabuilderDuration.R

echo '	...Reciprocity plots'

Rscript R/Analysis/recipPlot.R

echo 'Running sanction compliance analysis'

echo '	...Running models'

Rscript R/Analysis/durModels.R

echo '	...Running performance analysis'

Rscript R/Analysis/durModelsPerf.R

echo '	...Running cross-validation'

Rscript R/Analysis/crossval.R