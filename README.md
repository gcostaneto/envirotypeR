# envirotypeR
*A Framework for Plant and Animal Enviromics*


## Objective

**Enviromics** is the field of biometry and data analytics dedicated to the study of the [enviromes](https://en.wikipedia.org/wiki/Envirome), that is, the core of environmental conditions with the successful biological performance of living beings. **Envirotyping** (environmental + typing) involves a pool of processes and techniques to conduct enviromics, specifically in terms of characterizing environmental conditions and discovering descriptors that help identify the non-genetic drivers of phenotypic adaptation in plants, animals, or humans.Here, we introduce **envirotypeR** as a new toolkit to streamline the implementation of open-source enviromics in R. This R package was developed to enhance [EnvRtype](https://github.com/allogamous/EnvRtype) in order to accomodate many suggestions made by users, but also to include new applications who arose since 2021 (EnvRtype's publication year).


## Installation 

Current version of this package (v0.1.4, Feb 2024)  can be installed directly from this repository
using the `devtools` package:

```
if (!require("devtools")) install.packages("remotes")
remotes::install_github("gcostaneto/envirotypeR",force=TRUE)
```

## Current Resources

Current version of this package (v0.1.5, Jun 2024) has the following modules: 

(1) Generation of a wide number of **environmental features** using remote-data collection based on geographic coordinates and time windows (e.g., planting dates).

* `envirotypeR::process_synthetic()`
* `envirotypeR::get_soil()`
* `envirotypeR::get_spatial()`
* `envirotypeR::get_weather()`
* `envirotypeR::WC_Bioclimate`  # not available for  v0.1.4, Feb 2024
* `envirotypeR::SRTM_elevation` # not available for  v0.1.4, Feb 2024, please use  wc_elev <- terra::rast("https://raw.githubusercontent.com/gcostaneto/envirotypeR/main/inst/extdata/wc2.1_2.5m_elev.tif")
* `envirotypeR::GAEZ_AEZ`       # not available for  v0.1.4, Feb 2024

## History

* #019 **envirotypeR v0.1.5**  we added envirotypeR::process_synthetic() and created documentations for other functions.
  
* #018 **envirotypeR v0.1.4**  we removed the data objects (based on raster files / GeoTiff) for further maintenance).
  
* #018 **envirotypeR v0.1.3**  includes a new data set of 19 features from WorldClim Bioclimate`envirotypeR::WC_Bioclimate`/ data("WC_Bioclimate")

* #017 **envirotypeR v0.1.2**  includes a new data set of 27 features from FAO-GAEZ`envirotypeR::GAEZ_AEZ`/ data("GAEZ_AEZ")
  
* #016 **envirotypeR v0.1.1**  correction of bugs on envirotypeR::get_climate() and includes worldwide 250m elevation data from `envirotypeR::SRTM_elevation`/ data("SRTM_elevation") and to run examples using `envirotypeR::get_spatial()`
  
* #015 **envirotypeR v0.1.0** includes `envirotypeR::get_climate()` based on `nasapower` to collect climatological data (monthly scales)
  
* #014 **envirotypeR v0.0.1** updates `envirotypeR::get_spatial()` to collect info from `.nc` files
  
* #013 **envirotypeR v0.0.9** updates `envirotypeR::get_spatial()` to collect info from multiple rasters (`rasterStack` format)
 
* #012 **envirotypeR v0.0.8** updates `envirotypeR::get_spatial()` to run using `sf` and `terra` packages
  
* #011 **envirotypeR v0.0.7** includes `envirotypeR::get_spatial()` to collect point-estimates from raster files (digital image files)
  
* #010 **envirotypeR v0.0.6** updates `envirotypeR::get_soil()` to collect soil data
  
* #009 **envirotypeR v0.0.5** updates `envirotypeR::get_soil()` to collect soil data
  
* #008 **envirotypeR v0.0.4** includes `envirotypeR::get_soil()` to collect soil data from [SoilGrids](https://soilgrids.org/) using codes developed by [B.Monier](https://github.com/btmonier) (Buckler Lab Hackathon)
  
* rgdal is retired! We fixed it by Oct 21 2023. More info about rgdal's retirement [here](https://r-spatial.org/r/2022/04/12/evolution.html)

* #007 **envirotypeR v0.0.3** updates `EnvRtype::get_weather()` to include more environmental features (either from NASA POWER and computed variables)
  
* #006 **envirotypeR v0.0.2** updates `envirotypeR::get_weather()` to incorporate  `EnvRtype::get_weather()` and `EnvRtype::processWTH()`

* #005 **envirotypeR v0.0.1** repo was created using `EnvRtype` as reference (Sep 2023)

* #004 **EnvRtype** : `EnvRtype::get_weather()`  is updated by [T.Olivoto](https://github.com/TiagoOlivoto)

* #003 **EnvRtype** published at G3 Journal: Feb 2021
  
* #002 **EnvRtype** published at BiorXv : Oct 2020

* #001 **EnvRtype** repo was created (Jan 2020)

## Coming Soon

* Tutorials and Getting Help
* Publication
* CRAN
* process_cleaning()
* process_index()
* process_stages()
* variable_selection()
* fit_frequency()
* fit_Wmatrix()
* fit_Smatrix() 
* fit_Rmatrix()
* fit_Tmatrix() 
* fit_Mmatrix() 
* fit_Nmatrix() 
* plot_envirotype()


## Citation

**Original Pipeline**

Costa-Neto, G., Galli, G., Carvalho, H. F., Crossa, J., and Fritsche-Neto, R. (2021). EnvRtype: a software to interplay enviromics and quantitative genomics in agriculture. **G3 Genes|Genomes|Genetics**. doi:10.1093/g3journal/jkab040.

## Institutions / Contributors

* University of Sao Paulo / ESALQ USP (EnvRtype, from my PhD)
* CIMMYT (EnvRtype, from my PhD)
* Cornell University / Buckler Lab
  
# Usage

Please contact <germano.cneto@gmail.com> for more detail about the usage, suggestions, bugs or help.

<div align='center'>
  
<a href='https://www.free-website-hit-counter.com'><img src='https://www.free-website-hit-counter.com/c.php?d=9&id=159092&s=1' border='0' alt='Free Website Hit Counter'></a><br / ><small><a href='https://www.free-website-hit-counter.com' title="Free Website Hit Counter">Free website hit counter</a></small>

</div>


