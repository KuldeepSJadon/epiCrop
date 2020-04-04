# epiCrop
Set of functions and data for crop disease epidemiology work

# What is in it
BlightR: PLB model 
IrishRules: PLB model
weather: Example weather data needed to run the model. 

# How to instal
The package is not on CRAN so it can be installed directly from this repository.  
1. Restart r sesion (Ctrl/Cmd+Shift+F10). 
2. Install `devools` package. 
3. Install the `BlifgtR` package. 

Or simply run the following code:
``` r
.rs.restartR()

if (!"devtools"%in% installed.packages()) {
  install.packages("devtools", repos = "http://cran.rstudio.com/")
  library("devtools")
}

install_github("mladencucak/epiCrop", build_vignettes = TRUE)
```
Possible problems: 
- Make sure yoyr R version is up to date (>=3.4.0)
- You might need to update some packages. Currently `glue` package might cause some troubles.   
``` r
install.packages("glue",type = "source")
```

# Vignette
Vignette "run_BlightR" explains how to import data, run the model and visualise the results. 
