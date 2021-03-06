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

#' Assessment of critical sigma values.
#'
#' This function assesses a set of critical sigma values, one for each feature, out of a dataset.
#'
#' @param inputDataset Dataset (data frame) to be used for the assessment. The data frame must comply with the output format of the ebbc preprocessing function (ebbc_preprocess), thus containing the columns 'Subject', 'Feature', 'Mean', 'StdDev', 'SampleSize' and possibly 'Class'. Any other column is ignored, and any missing column forbids execution.
#' @param significanceLevel Significance level to be used for the assessment (must be greater than zero and less than one). Default is 0.05 (i.e. 5 percent).
#' @param outputFileName Name of the output file where the critical sigma values are to be stored. If not assigned, a filename is automatically generated.
#' @param sep Field separator character for the output files; the default is tabulation.
#'
#' @return A data frame of critical sigma values, containing the columns 'Feature' and 'SigmaMax'.

#' @export
ebbc_assessCriticalSigma <- function(inputDataset, significanceLevel=0.05, outputFileName="", sep='\t') {

	cat("\nLOG: ebbc_assessCriticalSigma() called.\n")

	## Input validation

	if (!(("Subject" %in% colnames(inputDataset)) & ("Feature" %in% colnames(inputDataset)) & ("Mean" %in% colnames(inputDataset)) & ("StdDev" %in% colnames(inputDataset)) & ("SampleSize" %in% colnames(inputDataset))))  {
		cat("ERROR: unsuitable dataset format. Dataset must contain columns 'Subject', 'Feature', 'Mean', 'StdDev', 'SampleSize'.\n")
		return(NULL)
	}

	if (length(inputDataset[1,]) > 7) {
		cat("WARNING: more than 7 dataset columns. Columns other than 'Subject', 'Feature', 'Mean', 'StdDev', 'SampleSize', 'Class' will be ignored.\n")
	}

	if ("Class" %in% colnames(inputDataset)) {
		selectedDataset <- subset(inputDataset, select=c("Subject", "Feature", "Mean", "StdDev", "SampleSize", "Class"))
		columnClassExists <- TRUE
	} else {
		selectedDataset <- subset(inputDataset, select=c("Subject", "Feature", "Mean", "StdDev", "SampleSize"))
		columnClassExists <- FALSE
	}

	if (sep == "") {
		sep <- " "
	}

	multipletSize <- unique(selectedDataset$SampleSize)
	if (length(multipletSize) > 1) {
		cat("ERROR: multiple sample sizes detected. Dataset must contain a unique value of SampleSize.\n")
		return(NULL)
	}
	multipletSize <- as.numeric(as.vector(multipletSize))

	## Define custom ks-test function

	ksTestPValue <- function(StdDev, varianceVector){
		res <- stats::ks.test(varianceVector/(StdDev*StdDev)*(multipletSize-1), stats::pchisq, df=multipletSize-1, alternative = "t")
		return(res[[2]])
	}

	## Assessment of critical sigma

	setOfFeatures <- unique(selectedDataset$Feature)
	setOfFeatures <- setOfFeatures[order(setOfFeatures)]
	for (feature in setOfFeatures) {
		tempFrame <- subset(selectedDataset, selectedDataset$Feature == feature)
		varianceColumn <- tempFrame$StdDev*tempFrame$StdDev
		varianceColumn <- data.frame(varianceColumn)
		names(varianceColumn) <- "Variance"
		tempFrame <- cbind(tempFrame, varianceColumn)

		# Find the value of StdDev in interval that gives maximum value of ksTestPValue
		options(warn=-1)
		ks_v <- as.numeric(stats::optimize(ksTestPValue, varianceVector=tempFrame$Variance, interval=c(0, 0.5), maximum=TRUE))
		options(warn=0)

		sigmaLimit <- ks_v[1] * sqrt(-2*log(significanceLevel))

		tempRow <- cbind(feature, round(sigmaLimit,2))
		if (!exists("criticalValuesTemp")) criticalValuesTemp <- tempRow
		else criticalValuesTemp <- rbind (criticalValuesTemp, tempRow)
	}

	criticalValuesFrame <- data.frame(criticalValuesTemp)
	names(criticalValuesFrame) <- c("Feature", "SigmaMax")

	## Output

	if (outputFileName == "") {
		outputFileName <- paste("critical_sigma_m_", multipletSize, "_significance_", significanceLevel, ".dat", sep="")
	}
	if (file.exists(outputFileName) & file.access(outputFileName, mode=2)) {
		cat("ERROR: cannot write ", outputFileName, ". Check write permission.\n")
		return(NULL)
	}
	cat(c("# multiplet size =", multipletSize, "\n"), file=outputFileName)
	cat(c("# significance level =", significanceLevel, "\n"), file=outputFileName, append=TRUE)

	options(warn=-1)
	utils::write.table(format(criticalValuesFrame, drop0trailing=FALSE), file=outputFileName, append=TRUE, sep=sep, col.names=TRUE, row.names=FALSE, quote=FALSE)
	options(warn=0)

	cat("LOG:\tCritical sigma values written to ", outputFileName, " successfully.\n", sep="")

	return(criticalValuesFrame)
}
