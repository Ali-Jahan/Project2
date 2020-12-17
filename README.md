<!-- badges: start -->
[![R-CMD-check](https://github.com/Ali-Jahan/Project2/workflows/R-CMD-check/badge.svg)](https://github.com/Ali-Jahan/Project2/actions)
[![Codecov test coverage](https://codecov.io/gh/Ali-Jahan/Project2/branch/master/graph/badge.svg)](https://codecov.io/gh/Ali-Jahan/Project2?branch=master)
<!-- badges: end -->

# Introduction
This is a R package which implements T-test, Linear regression, K-Neighrest neighbors with cross validation, and Random Forest with cross validation. As a part of this, you also have access to the gapminder data set (check documentation for more information).
# Installation instructions
Use the following command to install the package:

```{r, eval = FALSE}
devtools::install_github("Ali-Jahan/Project2", build_vignettes = TRUE, build_opts = c())
```
Load the package using:
```{r, eval = FALSE}
library(Project2)
```
Use the following to load the vignette:
```{r, eval = FALSE}
# View the html
help(package = "Project2", help_type = "html")
# view in browser
utils::browseVignettes(package = "Project2")
```
