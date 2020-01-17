## --------------------------------------------------------------------------
##
## This file is part of the EBBC software package.
##
## Version 1.0 - January 2020
##
##
## The EBBC package is free software; you can use it, redistribute it,
## and/or modify it under the terms of the GNU General Public License
## version 3 as published by the Free Software Foundation. The full text
## of the license can be found in the file LICENSE.txt at the top level of
## the package distribution.
##
## Authors:
##	Michele Castelluzzo (1), Alessio Perinelli (1),
##		Michela A. Denti (2) and Leonardo Ricci (1,3)
##	(1) Department of Physics, University of Trento, 38123 Trento, Italy
##	(2) Department of Cellular, Computational and Integrative Biology
##		(CIBIO), University of Trento, 38123 Trento, Italy
##	(3) CIMeC, Center for Mind/Brain Sciences, University of Trento,
##		38068 Rovereto, Italy
##
##	michele.castelluzzo@unitn.it
##	alessio.perinelli@unitn.it
##	michela.denti@unitn.it
##	leonardo.ricci@unitn.it
##	https://github.com/LeonardoRicci/
##	https://nse.physics.unitn.it/
##
##
## If you use the EBBC package for your analyses, please cite:
##
##	L. Ricci, V. Del Vescovo, C. Cantaloni, M. Grasso, M. Barbareschi and
##	M. A. Denti, Statistical analysis of a Bayesian classifier based on the
##	expression of miRNAs, BMC Bioinformatics 16:287 (2015).
##	DOI: 10.1186/s12859-015-0715-9
##
##
## --------------------------------------------------------------------------

#' Removal of dataset outliers.
#'
#' This function removes outliers from a given dataset according to a set of critical sigma values.
#'
#' @param inputDataset Dataset (data frame) to be cleaned of outliers. The data frame must comply with the output format of the ebbc preprocessing function (ebbc_preprocess), thus containing the columns 'Subject', 'Feature', 'Mean', 'StdDev', 'SampleSize' and possibly 'Class'. Any other column is ignored, and any missing column forbids execution.
#' @param criticalSigmaValues Critical sigma values (data frame) to be used. The data frame must comply with the output format of the ebbc function for critical sigma assessment (ebbc_assessCriticalSigma), thus containing the columns 'Feature' and 'SigmaMax'. Any other column is ignored, and any missing column forbids execution.
#'
#' Beware! Entries of the dataset for which 'Feature' is not present in the data frame of critical sigma values are copied in output without any filtering.
#'
#' @return A data frame corresponding to a copy of the input dataset devoid of outliers. The output data frame thus contains the columns 'Subject', 'Feature', 'Mean', 'StdDev', 'Variance', 'SampleSize' and possibly 'Class'.

#' @export
ebbc_removeOutliers <- function(inputDataset, criticalSigmaValues) {

	cat("\nLOG: ebbc_removeOutliers() called.\n")

	if (!(("Subject" %in% colnames(inputDataset)) & ("Feature" %in% colnames(inputDataset)) & ("Mean" %in% colnames(inputDataset)) & ("StdDev" %in% colnames(inputDataset)) & ("SampleSize" %in% colnames(inputDataset))))  {
		cat("ERROR: unsuitable dataset format. Dataset must contain columns 'Subject', 'Feature', 'Mean', 'StdDev', 'SampleSize'.\n")
		return(NULL)
	}

	if (!(("Feature" %in% colnames(criticalSigmaValues)) & ("SigmaMax" %in% colnames(criticalSigmaValues))))  {
		cat("ERROR: unsuitable criticalSigmaValues format. Data frame must contain columns 'Feature', 'SigmaMax'.\n")
		return(NULL)
	}

	if (length(inputDataset[1,]) > 7) {
		cat("WARNING: more than 7 dataset columns. Columns other than 'Subject', 'Feature', 'Mean', 'StdDev', 'SampleSize', 'Class' will not be present in output.\n")
	}

	if ("Class" %in% colnames(inputDataset)) {
		selectedDataset <- subset(inputDataset, select=c("Subject", "Feature", "Mean", "StdDev", "SampleSize", "Class"))
	} else {
		selectedDataset <- subset(inputDataset, select=c("Subject", "Feature", "Mean", "StdDev", "SampleSize"))
	}

	setOfFeatures <- unique(criticalSigmaValues$Feature)
	setOfFeatures <- setOfFeatures[order(setOfFeatures)]
	selectedDataset <- selectedDataset[selectedDataset$Feature %in% setOfFeatures, ]

	for(i in seq (1,nrow(criticalSigmaValues))) {
		feature <- criticalSigmaValues$Feature[i]
		sigmaLimit <- as.numeric(as.vector(criticalSigmaValues$SigmaMax[i]))
		selectedDataset <- selectedDataset[!(selectedDataset$Feature == feature & selectedDataset$StdDev > sigmaLimit),]
	}

	return(selectedDataset)
}
