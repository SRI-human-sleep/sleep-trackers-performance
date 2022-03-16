# Analytical pipeline and functions for testing the performance of sleep-tracking technology

The repository includes a set of functions based on the R environment that implement the analytical steps described in the article:

Menghini, L., Cellini, N., Goldstone, A., Baker, F. C., & de Zambotti, M. (2021). A standardized framework for testing the performance of sleep-tracking technology: step-by-step guidelines and open-source code. *Sleep, 44*(2), zsaa170. https://dx.doi.org/10.1093%2Fsleep%2Fzsaa170

<br>

## Analytical pipeline
The **analytical pipeline** for applying the functions to a dataset is depicted at [this page](https://sri-human-sleep.github.io/sleep-trackers-performance/AnalyticalPipeline_v1.0.0.html).

<br>

## R functions
To use the functions, it is required to (1) [download and install R](https://cran.r-project.org/), (2) prepare a dataset of epoch-by-epoch data (as described in the [article](https://dx.doi.org/10.1093%2Fsleep%2Fzsaa170), and (3) load both the dataset and the functions in R.

- To **read a dataset**, functions such as `read.csv()` can be used depending on the file format (type `?read.csv()` in R for details). The command `read.csv(file.choose())` allows to open a dialog box presented to the user to browse the file in the computer.
- To **read the functions**, the files should be downloaded from the `Functions` folder in this repository, and pasted in the directory folder used by R (the command `getwd()` in R allows to visualize the current directory). The command `source("function_name.R")` allows to read the function file. The command `source(file.choose())` allows to open a dialog box presented to the user to browse the file in the computer.
- The first time that a function is used, it automatically installs all the required packages (might take a while). Alternatively, R packages can be installed using the command `install.packages("package_name")`.
