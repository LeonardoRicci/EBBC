# Example pipeline
# First, the example dataset stored in dataset_alpha.dat is analyzed and used to train a Bayesian classifier.
# Second, the example dataset stored in dataset_beta.dat is analyzed and classified.

# First of all, load the EBBC package:

library(EBBC)

##########################################
## First part: training of a classifier ##
##########################################

# Loading dataset alpha
# A simple "read.table" does the job:
datasetAlpha <- read.table(file="dataset_alpha.dat", header=TRUE)

# Preprocessing
# The ebbc_preprocess function computes sample mean and standard deviation of multiplets.
preprocessedDatasetAlpha <- ebbc_preprocess(datasetAlpha, multipletSize=3)

# Outlier removal
# The ebbc_assessCriticalSigma function assesses the critical standard deviation for outlier removal.
criticalSigmaValuesAlpha <- ebbc_assessCriticalSigma(preprocessedDatasetAlpha, significanceLevel=0.05, outputFileName="criticalSigma_datasetAlpha.dat")
# The ebbc_removeOutliers function removes any outlier from a dataset according to the assessed critical values.
purifiedDatasetAlpha <- ebbc_removeOutliers(preprocessedDatasetAlpha, criticalSigmaValuesAlpha)

# Feature analysis
# The Target and Versus sets first have to be defined.
Target <- c("A")
Versus <- c("B", "C")
# The ebbc_bayes function, without any feature list, runs in 'Analysis mode' and carries out the analysis of all features.
statisticsAlpha <- ebbc_bayes(purifiedDatasetAlpha, inputTargetList=Target, inputVersusList=Versus, outputFileBasename="featureAnalysis_datasetAlpha")
# The same as the last call, but using "FZ" as normalizer.
statisticsAlphaNorm <- ebbc_bayes(purifiedDatasetAlpha, inputTargetList=Target, inputVersusList=Versus, inputFeaturesList=c("FZ"), outputFileBasename="featureAnalysisNorm_datasetAlpha")
# The analysis reveals a possible set of features/coefficients to be used for the classification:
featuresToUse <- c("FX", "FZ")
coefficientsToUse <- c(1.0, -1.0)

# Training of Bayesian classifier
# The ebbc_bayes function is run in 'Training mode'.
thresholdValues <- ebbc_bayes(purifiedDatasetAlpha, inputTargetList=Target, inputVersusList=Versus, inputFeaturesList=featuresToUse, coeffList=coefficientsToUse, outputFileBasename="threshold_datasetAlpha")

##############################################
## Second part: classification of a dataset ##
##############################################

# Loading dataset beta
datasetBeta <- read.table(file="dataset_beta.dat", header=TRUE)
# Preprocessing
preprocessedDatasetBeta <- ebbc_preprocess(datasetBeta, multipletSize=3)

# Critical sigma values are loaded from the previously generated file
criticalSigmaValues <- ebbc_loadCriticalSigma("criticalSigma_datasetAlpha.dat")
# Outlier removal
purifiedDatasetBeta <- ebbc_removeOutliers(preprocessedDatasetBeta, criticalSigmaValues)

# Classification
# Threshold values are loaded from the previously generated file
thresholdValues <- ebbc_loadThresholds("threshold_datasetAlpha.txt")
# Classifier parameters are defined in the same way as for training:
featuresToUse <- c("FX", "FZ")
coefficientsToUse <- c(1.0, -1.0)
# Classification is carried out by the ebbc_classifyDataset function:
classifiedBeta <- ebbc_classifyDataset(purifiedDatasetBeta, inputFeaturesList=featuresToUse, coeffList=coefficientsToUse, inputThreshold=thresholdValues, outputFileBasename="classified_datasetBeta")
