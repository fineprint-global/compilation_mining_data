# Compilation of data accounts on production of mines

Compilation of a dataset including a range of variables related to the production of individual mines over time.



## General information

The main purposes of these scripts are the retrieval, formatting, harmonization, and integration of various datasets from different sources. In addition various other tasks are performed, like unit conversion, gap filling and estimations, comparison versus other data.
The desired output is a consistent dataset on the production of individual mines and related variables.

Further explanations on the background can be found at http://fineprint.wu.ac.at/10_fp-wp1-mining/



## Technical information

### Structure

#### Scripts

The repository comprises scripts across different folders and sub-folders.

The highest level of differentiation is by type of data source, e.g. company reports ("detailed data") or NSOs ("country-specific").

Within these folders all of the required processing can be accessed from the main script main.R. This main scripts thereby also illustrates the structure of processing. No access of sub-scripts is required.

In each folder the required structure and scripts are different, but mainly follow the logic of data retrieval, formatting, harmonization, integration, and checking.



#### Intermediate outputs and files

Most scripts save and access intermediate files in folder `./03_intermediate/`. These files are all saved as .rds-files.
Therefore other details of data can easily be accessed through these files, e.g. by loading all_tables_converted.rds.



#### Rmd outputs

Several Rmd-files are created for purposes like results from harmonization or an overview of data coverage. These are saved in sub-folders `./04_output/.../`.



### Dependencies

#### R

For all required R packages, at the beginning of the first main script, their installation is checked. If not installed, an installation is started.



#### Data sources

+ Detailed: Company reports
+ Country-specific: National statistical institutions



## Acknowledgement
This project gratefully acknowledges financial support from the ERC as part of the [FINEPRINT](https://www.fineprint.global/) project.


