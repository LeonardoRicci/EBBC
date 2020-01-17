# BCMR package

An R package to carry out training and classification analyses on datasets containing multiplets of values of a given measured feature. This package contains a set of functions that implement the analysis algorithm described in
> L. Ricci, V. Del Vescovo, C. Cantaloni, M. Grasso, M. Barbareschi and M. A. Denti, _Statistical analysis of a Bayesian classifier based on the expression of miRNAs_, BMC Bioinformatics __16__:287, 2015.  
DOI: [10.1186/s12859-015-0715-9](https://dx.doi.org/10.1186/s12859-015-0715-9)

## License

This package is free software. It is distributed under the terms of the GNU General Public License (GPL), version 3.0 - see the `LICENSE.txt` file for details.
This package requires the R environment, which is free software released under the terms of GPL (see https://www.r-project.org/ for further details).
This package requires the packages `stats`, `utils`, `tools`, `pROC` and `ggplot2` (the latter two for plotting purposes). The package `devtools` is necessary if the package is installed from source.

## Authors

- Michele Castelluzzo (1) michele.castelluzzo@unitn.it
- Alessio Perinelli (1) alessio.perinelli@unitn.it
- Michela Denti (2) michela.denti@unitn.it
- Leonardo Ricci (1,3) leonardo.ricci@unitn.it

(1) Department of Physics, University of Trento, 38123 Trento, Italy.  
(2) Department of Cellular, Computational and Integrative Biology (CIBIO), University of Trento, 38123 Trento, Italy.  
(3) CIMeC, Center for Mind/Brain Sciences, University of Trento, 38068 Rovereto, Italy.

If the package turns out to be useful for your research, please cite our [paper](https://dx.doi.org/10.1186/s12859-015-0715-9):  
L. Ricci, V. Del Vescovo, C. Cantaloni, M. Grasso, M. Barbareschi and M. A. Denti, _Statistical analysis of a Bayesian classifier based on the expression of miRNAs_, BMC Bioinformatics __16__:287, 2015.  

## Package info

The package consists in a set of functions for the R environment. See the user manual `/docs/manual.pdf` for details on the programs functionalities. All source code is under `/R/`. Example code and datasets are under `/examples`.

The package setup file, as well as details on how to install it, can be found within `/setup/`. Setup information is also reported in the user manual `/docs/manual.pdf`.
