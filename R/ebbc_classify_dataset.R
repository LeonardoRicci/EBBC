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

#' Classification of a dataset.
#'
#' This function classifies the entries of the input dataset as either target or versus by using the chosen classifier and given the corresponding threshold value.
#'
#' @param inputDataset Dataset (data frame) to be classified. The data frame must comply with the output format of the ebbc preprocessing functions (ebbc_preprocess and ebbc_removeOutliers), thus containing the columns 'Subject', 'Feature', 'Mean', 'StdDev', 'SampleSize'. Any other column is ignored, and any missing column forbids execution.
#' @param inputFeaturesList List of features to be used by the classifier. The chosen features must be present in the 'Feature' column of the inputDataset.
#' @param coeffList List of coefficients for the classifier. The number of coefficients must be the same as the number of used features and listed in the same order.
#' @param inputThreshold Threshold data frame for the classifier. The data frame must comply with the output format of the ebbc Bayes training function (ebbc_bayes), thus containing the columns 'Threshold', 'DeltaThreshold', 'ChiUp', 'DChiUp', 'ChiDown', 'DChiDown'. Any other column is ignored.
#' @param outputFileBasename Name of the output file where the classification results are to be stored. If not assigned, a filename is automatically generated.
#' @param sep Field separator character for the output file; the default is tabulation.
#' @param plotFormat String specifying the format of generated graphic files (plots): can either be "pdf" (default) or "png".
#'
#' @return A data frame containing the columns 'Subject', 'Classification' and 'Score'.

#' @export
ebbc_classifyDataset <- function(inputDataset, inputFeaturesList, coeffList, inputThreshold, outputFileBasename="", sep='\t', plotFormat="pdf") {

	cat("\nLOG: ebbc_classifyDataset() called.\n")

	## Input validation and preprocessing

	if (!(("Subject" %in% colnames(inputDataset)) & ("Feature" %in% colnames(inputDataset)) & ("Mean" %in% colnames(inputDataset)) & ("StdDev" %in% colnames(inputDataset)) & ("SampleSize" %in% colnames(inputDataset))))  {
		cat("ERROR: unsuitable dataset format. Dataset must contain columns 'Subject', 'Feature', 'Mean', 'StdDev', 'SampleSize'.\n")
		return(NULL)
	}

	if (length(inputDataset[1,]) > 6) {
		cat("WARNING: more than 6 dataset columns. Columns other than 'Subject', 'Feature', 'Mean', 'StdDev', 'SampleSize', 'Class' will be ignored.\n")
	}

	if ("Class" %in% colnames(inputDataset)) {
		selectedDataset <- subset(inputDataset, select=c("Subject", "Feature", "Mean", "StdDev", "SampleSize", "Class"))
	} else {
		selectedDataset <- subset(inputDataset, select=c("Subject", "Feature", "Mean", "StdDev", "SampleSize"))
	}

	if (sep == "") {
		sep <- " "
	}

	# Input validation: check Features
	availableFeatures <- unique(selectedDataset$Feature)
	listOfFeature <- unique(inputFeaturesList)
	if (length(listOfFeature) != length(inputFeaturesList)){
		cat("WARNING: feature list presents some duplicates which will be ignored.\n")
	}
	inputFeaturesList <- unique(inputFeaturesList)
	listOfFeature <- intersect(listOfFeature, availableFeatures)
	if (length(listOfFeature) != length(inputFeaturesList)){
		cat("ERROR: some selected features are not present in the dataset.\n")
		return(NULL)
	}
	if (length(listOfFeature) != length(coeffList)) {
		cat("ERROR: unsuitable function arguments. Size of feature list and coefficient list do not match.\n")
		return(NULL)
	}

	# Input validation: check Subjects
	listOfSubjects <- unique(selectedDataset$Subject)
	for (subject in listOfSubjects) {
		subjectFrame <- selectedDataset[selectedDataset$Subject == subject,]
		availableSubjectFeatures <- unique(subjectFrame$Feature)
		if (length(listOfFeature) != length(intersect(availableSubjectFeatures, listOfFeature))) {
			if (!exists("subjectsToRemove")) {
				subjectsToRemove <- subject
			} else {
				subjectsToRemove <- rbind(subjectsToRemove, subject)
			}
		}
	}
	if (exists("subjectsToRemove")) {
		subjectsToRemove <- unique(subjectsToRemove)
		correctSubjects <- setdiff(listOfSubjects, subjectsToRemove)
	} else {
		correctSubjects <- listOfSubjects
	}
	if (length(correctSubjects) == 0) {
		cat("ERROR: no available subjects for this classifier.\n")
		return(NULL)
	}
	availableDataset <- selectedDataset[selectedDataset$Subject %in% correctSubjects, ]
	availableDataset <- availableDataset[availableDataset$Feature %in% listOfFeature, ]

	# Bookkeeping
	for (feature in listOfFeature) {
		columnSubjectMean <- availableDataset[availableDataset$Feature == feature,]
		columnSubjectMean <- subset(columnSubjectMean, select=c("Subject", "Mean"))
		if (!exists("featureFrame")) {
			featureFrame <- columnSubjectMean
		} else {
			featureFrame <- merge(featureFrame, columnSubjectMean, by = "Subject")
		}
	}
	names(featureFrame) <- c("Subject", listOfFeature)

	## Actual classification

	threshold <- as.numeric(as.vector(inputThreshold$Threshold))

	# Compute score as linear combination of features
	dataFrameTemp <- featureFrame[2:(1+length(listOfFeature))]
	for (feature in listOfFeature) {
		dataFrameTemp[,feature] <- dataFrameTemp[,feature] * as.numeric(coeffList[which(listOfFeature == feature)])
	}
	classifierDataFrame <- cbind(featureFrame, Score=rowSums(dataFrameTemp))
	classifierDataFrame <- data.frame(classifierDataFrame[with(classifierDataFrame, order(Score)),])

	# Create new classification columns and writes "target" or "versus" at each row, then join all together
	targetFrame <- classifierDataFrame[classifierDataFrame$Score > threshold, ]
	targetFrame <- cbind(targetFrame, Classification=rep("target", length(targetFrame[,1])))

	versusFrame <- classifierDataFrame[classifierDataFrame$Score <= threshold, ]
	versusFrame <- cbind(versusFrame, Classification=rep("versus", length(versusFrame[,1])))

	outputDataFrame <- rbind(targetFrame, versusFrame)
	outputDataFrame <- subset(outputDataFrame, select=c("Subject", "Classification", "Score", listOfFeature))
	outputDataFrame <- outputDataFrame[with(outputDataFrame, order(Score, decreasing=FALSE)),]

	# Remove bias outliers
	nt <- length(outputDataFrame$Score[outputDataFrame$Classification == "target"])
	xt <- mean(outputDataFrame$Score[outputDataFrame$Classification == "target"])
	st <- stats::sd(outputDataFrame$Score[outputDataFrame$Classification == "target"])

	nv <- length(outputDataFrame$Score[outputDataFrame$Classification == "versus"])
	xv <- mean(outputDataFrame$Score[outputDataFrame$Classification == "versus"])
	sv <- stats::sd(outputDataFrame$Score[outputDataFrame$Classification == "versus"])

	scoreColumnTarget <- subset(outputDataFrame[outputDataFrame$Classification == "target",], select="Score")
	scoreColumnVersus <- subset(outputDataFrame[outputDataFrame$Classification == "versus",], select="Score")

	biasOutlierIndex_t <- 1 - stats::pt(sqrt(((scoreColumnTarget$Score - xt) / st)^2), nt)
	biasOutlierIndex_v <- 1 - stats::pt(sqrt(((scoreColumnVersus$Score - xv) / sv)^2), nv)
	biasOutlierIndex <- c(biasOutlierIndex_t, biasOutlierIndex_v)

	outputDataFrame <- outputDataFrame[which(biasOutlierIndex > 0.001), ]
	processedFrameToWrite <- subset(outputDataFrame, select=c("Subject", "Classification", "Score"))

	## Output

	# Write classified dataset to file
	classifierLabel <- ""
	for (i in 1:length(coeffList)) {
		classifierLabel <- paste(classifierLabel, "_", coeffList[i], "_", listOfFeature[i], sep="")
	}
	if (outputFileBasename == "") {
		outputFileBasename <- paste("classified_dataset", classifierLabel, sep="")
		plotFileName <- paste("classified_dataset", classifierLabel, sep="")
	} else {
		plotFileName <- outputFileBasename
	}
	outputFile <- paste(outputFileBasename, ".dat", sep="")

	if (file.exists(outputFile) & file.access(outputFile, mode=2)) {
		cat("ERROR: cannot write ", outputFile, ". Check write permission.\n")
		return(NULL)
	}

	options(warn=-1)
	utils::write.table(format(processedFrameToWrite, drop0trailing=FALSE), file=outputFile, sep=sep, row.names=FALSE, quote=FALSE)
	options(warn=0)

	cat("LOG:\tClassified dataset written to ", outputFile, " successfully.\n", sep="")

	# Plot Scores
	thresholdPlot <- ebbc_plotThresholds(outputDataFrame, inputThreshold, plotFileName, plotFormat=plotFormat)

	cat("LOG:\tScore classification plot saved to ", plotFileName, "_score.", plotFormat, " successfully.\n", sep="")

	return(processedFrameToWrite)
}
