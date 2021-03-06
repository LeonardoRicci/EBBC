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

#' Pre-processing of datasets.
#'
#' This function carries out the pre-processing required by the other functions of the ebbc package.
#'
#' @param inputDataset Dataset (data frame) to be pre-processed. The data frame must contain the columns 'Subject', 'Feature', 'Value' and possibly 'Class'. Any other column is ignored, and any missing column forbids execution.
#' @param multipletSize Size of the multiplets to be considered. Any multiplet of different size is ignored.
#'
#' @return A pre-processed data frame, containing the columns 'Subject', 'Feature', 'Mean', 'StdDev', 'SampleSize', and possibly 'Class'.

#' @export
ebbc_preprocess <- function(inputDataset, multipletSize) {

	cat("\nLOG: ebbc_preprocess() called.\n")

	if (!(("Feature" %in% colnames(inputDataset)) & ("Subject" %in% colnames(inputDataset)) & ("Value" %in% colnames(inputDataset))))  {
		cat("ERROR: unsuitable dataset format. Dataset must contain at least columns 'Feature', 'Subject', 'Value'.\n")
		return(NULL)
	}

	if (length(inputDataset[1,]) > 4) {
		cat("WARNING: more than 4 dataset columns. Columns other than 'Feature', 'Subject', 'Value', 'Class' will be ignored.\n")
	}

	if ("Class" %in% colnames(inputDataset)) {
		selectedDataset <- subset(inputDataset, select=c("Subject", "Feature", "Value", "Class"))
		columnClassExists <- TRUE
	} else {
		selectedDataset <- subset(inputDataset, select=c("Subject", "Feature", "Value"))
		columnClassExists <- FALSE
	}

	selectedDataset$Value <- as.numeric(as.vector(selectedDataset$Value))
	selectedDataset <- selectedDataset[is.numeric(selectedDataset$Value) & !is.na(selectedDataset$Value), ]

	setOfSubjects <- unique(as.character(selectedDataset$Subject))

	for (subject in setOfSubjects) {
		singleSubjectFrame <- subset(selectedDataset, selectedDataset$Subject == subject)
		if (length(unique(singleSubjectFrame$Class)) > 1) {
			cat("ERROR: multiple classes for some of the subjects. Each subject's entry must report the same class.\n")
			return(NULL)
		}
	}

	setOfFeatures <- unique(selectedDataset$Feature)
	setOfFeatures <- setOfFeatures[order(setOfFeatures)]

	for (feature in setOfFeatures) {
		singleFeatureFrame <- subset(selectedDataset, selectedDataset$Feature == feature)

		columnSubjectFeatures <- cbind(sort(unique(as.character(singleFeatureFrame$Subject))), rep(feature, length(unique(singleFeatureFrame$Subject))))
		columnValueMean <- as.vector(tapply(singleFeatureFrame$Value, as.character(singleFeatureFrame$Subject), mean))
		columnValueStd <- as.vector(tapply(singleFeatureFrame$Value, as.character(singleFeatureFrame$Subject), stats::sd))
		columnSampleSize <- as.vector(tapply(singleFeatureFrame$Value, as.character(singleFeatureFrame$Subject), length))

		if (columnClassExists) {
			columnClass <- as.vector(tapply(as.character(singleFeatureFrame$Class), as.character(singleFeatureFrame$Subject), unique))
			tempRows <- cbind(columnSubjectFeatures, columnValueMean, columnValueStd, columnSampleSize, columnClass)
		} else {
			tempRows <- cbind(columnSubjectFeatures, columnValueMean, columnValueStd, columnSampleSize)
		}

		if (!exists("tempFrame"))
			tempFrame <- tempRows
		else
			tempFrame <- rbind (tempFrame, tempRows)
	}
	processedDataset <- data.frame(tempFrame)

	if (columnClassExists) {
		names(processedDataset) <- c("Subject", "Feature", "Mean", "StdDev", "SampleSize", "Class")
	} else {
		names(processedDataset) <- c("Subject", "Feature", "Mean", "StdDev", "SampleSize")
	}

	processedDataset$Mean <- as.numeric(as.character(processedDataset$Mean))
	processedDataset$StdDev <- as.numeric(as.character(processedDataset$StdDev))
	processedDataset$SampleSize <- as.numeric(as.character(processedDataset$SampleSize))

	processedDataset$Mean <- round(processedDataset$Mean,4)
	processedDataset$StdDev <- round(processedDataset$StdDev,4)
	processedDataset$SampleSize <- round(processedDataset$SampleSize,0)

	processedDataset <- processedDataset[processedDataset$SampleSize == multipletSize, ]
	processedDataset <- data.frame(processedDataset[with(processedDataset, order(Subject)),])

	return(processedDataset)

}
