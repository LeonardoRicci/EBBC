## Requirements

The package requires R version 3.2 or later.
It also requires the following packages to be installed:
	`stats`, `utils`, `tools`, `ggplot2`, `pROC`
To install any of these packages, just type (from within R)
	`install.packages("package_name")`

## Setup

The package can be installed manually from the compressed archive stored in this directory. The package does not need compilation.

On all platforms, launch R in this directory and type:
```
	install.packages("EBBC_1.0.tar.gz", type="source")
```
Please note that you will need permissions to write in R's library folder in order to install the package.
Alternatively, from within this directory, open a terminal and type:
```
	R CMD INSTALL EBBC_1.0.tar.gz
```
Again, you will need permissions to write in R's library folder.

For further information, please refer to the CRAN R Installation and Administration guide.
`https://cran.r-project.org/doc/manuals/R-admin.html`

If the package cannot be installed from the `.tar.gz` archive, the package can be rebuilt from source. This procedure requires the `devtools` package to be installed in order to produce the function documentation. Open R from within the package root directory and type
```
	library(devtools)
	document()
	build()
```
A `.tar.gz` file will be produced in the parent directory. Then, the package can be installed through the `install.packages()` function within R.
