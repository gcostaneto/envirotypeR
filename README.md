# envirotypeR
*A Framework for Plant and Animal Enviromics*


# Objective

Enviromics is the field of Biometry and Data Analytics dedicated to the study of the [enviromes](https://en.wikipedia.org/wiki/Envirome) ( core of environmental conditions with the successful biological performance of living beings).
Envirotyping (environmental + typing) is the process and pool of techniques to conduct enviromics, more specifically in terms of characterizing environmental conditions and discovering descriptors that help in identifying the non-genetic drivers of phenotypic adaptation in plants, animals, or humans. Here we present the **envirotypeR** as a new R package. It was developed to expand [EnvRtype](https://github.com/allogamous/EnvRtype) as a more powerfull toolkit to facilitate the implementation of open-source enviromics in R.

# Installation 

Current version of this package (0.1.1, Nov 2023)  can be installed directly from this repository
using the `devtools` package:

```
if (!require("devtools")) install.packages("devtools")
devtools::install_github("gcostaneto/envirotypeR",force=TRUE)
```

# Updates

* envirotypeR v0.1.0  includes worldwide 250m elevation data from envirotypeR::SRTM_elevation / data("SRTM_elevation") and to run examples using envirotypeR::get_spatial()
  
* envirotypeR v0.1.0  correction of bugs on envirotypeR::get_climate()
  
* envirotypeR v0.0.9 includes envirotypeR::get_climate() based on `nasapower` to collect climatological data (monthly scales)
  
* envirotypeR v0.0.8 updates envirotypeR::get_spatial() to collect info from `.nc` files
  
* envirotypeR v0.0.7 updates envirotypeR::get_spatial() to collect info from multiple rasters (`rasterStack` format)
 
* envirotypeR v0.0.6 updates envirotypeR::get_spatial() to run using `sf` and `terra` packages
  
* envirotypeR v0.0.5 includes envirotypeR::get_spatial() to collect point-estimates from raster files (digital image files)
  
* envirotypeR v0.0.6 updates envirotypeR::get_soil() to collect soil data
  
* envirotypeR v0.0.5 updates envirotypeR::get_soil() to collect soil data
  
* envirotypeR v0.0.4 includes envirotypeR:get_soil() to collect soil data from [SoilGrids](https://soilgrids.org/) using codes developed by [B.Monier](https://github.com/btmonier) (Buckler Lab Hackathon)
  
* rgdal is retired! We fixed it by Oct 21 2023. More info about rgdal's retirement [here](https://r-spatial.org/r/2022/04/12/evolution.html)

* envirotypeR v0.0.3 updates EnvRtype::get_weather() to include more environmental features (either from NASA POWER and computed variables)

* envirotypeR v0.0.2 updates EnvRtype::get_weather() to incorporate envirotypeR::get_weather() and EnvRtype::processWTH()

* envirotypeR v0.0.1 repo was created using EnvRtype as reference (Sep 2023)


# Coming Soon

* process_cleaning()
* process_index()
* process_stages() 
* process_synthetic() 
* process_colinearity()
* fit_Wmatrix()
* fit_Smatrix() 
* fit_Rmatrix()
* fit_Tmatrix() 
* fit_Mmatrix() 
* fit_Nmatrix() 
* fit_singleERM()
* fit_multipleERM()
* fit_stageERM()
* find_optimalSites()
* build_model() 
* predict_phenotype()
* plot_envirotype()
* envirotyping()
