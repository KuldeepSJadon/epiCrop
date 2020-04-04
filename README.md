# epiCrop
Set of functions and data for crop disease epidemiology work

# What is in it
BlightR: PLB model  
IrishRules: PLB model  
weather: Example weather data needed to run the model. 

# How to instal
The package is not on CRAN so it can be installed directly from this repository. 
1. Restart r sesion (Ctrl/Cmd+Shift+F10). 
2. Install `remotes` package. 
3. Install the `BligtR` package. 

This could be accomplisehed by running the following code:
``` r
.rs.restartR()

if (!"remotes"%in% installed.packages()) {
  install.packages("remotes", repos = "http://cran.rstudio.com/")
  library("remotes")
}

remotes::install_github("mladencucak/epiCrop",dependencies = TRUE)
```
Installing packages that are not on CRAN can be a pain, so there are a  few general notes on what to do.  
Generally there are always packages that are not If you get an error looking like this while updating your packages: 
``` r
Error: (converted from warning) cannot remove prior installation of package ‘name_of_the_package’
```
You might need to update some packages manually to match the *source* version. So, just replace the `name_of_the_package` to match the name of the pacage you are missing/need to update.   
``` r
install.packages("name of the package",type = "source")
```
After try run the code snipept from the beginig of this section. 
# Vignette
Vignette "run_BlightR" explains how to import data, run the model and visualise the results. 
