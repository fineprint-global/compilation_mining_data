# REPO TITLE 

A short description/abstract, including reference to paper/breif if published. 

## Usage

### Dependencies

#### R
In order to run the script, you need the following software version (e.g, `R` and `GDAL`). The scripts also depends on the following R packages from CRAN, efor example
```{r}
install.packages(c("tidyverse", "sf", "lwgeom", "rmapshaper", "nngeo"))
))
```

The script might also depends on a development version of a package from Github:
```{r}
install.packages("devtools")
devtools::install_github(repo = "gru-wu/gruRmarkdown", ref = "00193d4")
))
```
The argument `ref` can be a reference to a commit or tag from the github repo. 

#### git
You only need git installed if you want to contribute to the repository or clone it without having to download it manually.

#### Data sets 

A description of the input data. What is the format? Is there a link to download the data? Are there restrictions to use the data? 

### Get the scripts 
To get the scripts, you can either

- download the source using the "Clone or download"-button above
- use `git clone https://github.com/fineprint-global/template-data-analysis`

### How to run

A short description of how to run the scripts. Are there several files, is there an order? What is the input and output of each script? 

## Acknowledgement
This project gratefully acknowledges financial support from the ERC as part of the [FINEPRINT](https://www.fineprint.global/) project.
