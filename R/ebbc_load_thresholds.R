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

#' Load classifier threshold values.
#'
#' This function loads from file a data frame containing the threshold values of a trained classifier.
#'
#' @param inputFileName Name of the file to be loaded. The file has to contain at least the columns 'Threshold', 'DeltaThreshold', 'ChiUp', 'DChiUp', 'ChiDown', 'DChiDown' (not necessarily in this order).
#' @param sep Field separator character; the default is any white space (one or more spaces or tabulations).
#'
#' @return A data frame containing all the columns present in the file.

#' @export
ebbc_loadThresholds <- function(inputFileName, sep="") {

	cat("\nLOG: ebbc_loadThresholds() called.\n")

	if (!file.exists(inputFileName)) {
		cat("ERROR: cannot read ", inputFileName, ". No such file or directory.\n")
		return(NULL)
	} else if (file.access(inputFileName, mode=4)) {
		cat("ERROR: cannot read ", inputFileName, ". Check read permission.\n")
		return(NULL)
	}

	if (sep == "") {
		readFrame <- utils::read.table(file=inputFileName, fileEncoding="UTF-8", header=T)
	} else {
		readFrame <- utils::read.table(file=inputFileName, sep=sep, fileEncoding="UTF-8", header=T)
	}

	if (!(("Threshold" %in% colnames(readFrame)) & ("DeltaThreshold" %in% colnames(readFrame)) & ("ChiUp" %in% colnames(readFrame)) & ("DChiUp" %in% colnames(readFrame)) & ("ChiDown" %in% colnames(readFrame))  & ("DChiDown" %in% colnames(readFrame)) ))  {
		cat("ERROR: unsuitable dataset format. Dataset must contain columns 'Threshold', 'DeltaThreshold', 'ChiUp', 'DChiUp', 'ChiDown', 'DChiDown'.\n")
		return(NULL)
	}

	cat("LOG:\tThreshold values loaded from ", inputFileName, " successfully.\n", sep="")

	return(readFrame)
}
