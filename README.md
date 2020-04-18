# Testing the performance of sleep-tracking technology
Analytical pipeline and functions for testing the performance of sleep-tracking technology.

The repository includes a set of functions based on the R environment that implement the analytical steps described in the article "A standardized framework for testing the performance of sleep-tracking technology: Step-by-step guidelines and open-source code" (Menghini, Cellini, Goldstone, Baker, de Zambotti, 2020).

The **analytical pipeline** for applying the functions to a dataset is depicted at https://sri-human-sleep.github.io/sleep-trackers-performance/AnalyticalPipeline.html

To use the functions, it is required to download and install R (https://cran.r-project.org/), and eventually RStudio (https://rstudio.com/), to prepare a dataset of epoch-by-epoch data (as described in the main article), and to load both the dataset and the functions in R.

To read a dataset, functions such as read.csv() can be used depending on the file format (type *?read.csv()* in R for details). The command *read.csv(file.choose())* allows to open a dialog box presented to the user to browse the file in the computer.

To read the functions, the files should be downloaded from the **Functions** folder in this repository, and pasted in the directory folder used by R (the command *getwd()* allows to visualize the current directory). The command *source("function_name.R")* allows to read the function file. The command *source(file.choose())* allows to open a dialog box presented to the user to browse the file in the computer.

The first time that a function is used, it automatically installs all the required packages (it might take a while). ALternatively, R packages can be installed using the command *install.packages("package_name")*.
