# Compilation of data accounts on production of mines

Compilation of a dataset including a range of variables related to the production of individual mines over time.



## General information

The main purposes of these scripts are the retrieval, formatting, harmonization, checks, and integration of various datasets from different sources. In addition various other tasks are performed, like unit conversion, gap filling and estimations, additional checks, and comparison versus other data.
The desired output are several consistent datasets on the production of individual mines and related variables. In particular, these comprise a dataset with information sources from company report, data on coal derived from the same, plus the former integrated with information from country-specific and other sources.

Further explanations on the background can be found at http://fineprint.wu.ac.at/10_fp-wp1-mining/



## Technical information

### Structure

#### General

All files are comprised within a shared structure within four upper-level folders: Input / Scripts / Intermediate / Output.  
The main reason is that, despite different processing requirements across the different types of data sources, they share a range of common files (e.g. input files like ID and concordance tables as well as unit conversion factors, or scripts like certain functions).

Within these folders, sub-folders are structured according to the respective types of sources, at the level where necessary and reasonable, for example within "./01_input/01_data/" or within "./02_scripts/".

The two main types of sources where files exist and most data has been processed, are:  
+ detailed_data (company reports)  
+ country-specific (national institutions, e.g. ministries/agencies)  



#### Input files

Input files comprise:  
+ Raw data obtained externally (i.e. without any script in this repository, except for simple database queries)  
+ ID and concordance tables  
+ Log files  
+ Factors (e.g. unit conversion)  
+ Other relevant data and information (e.g. metal prices or national accounts for production)  



#### Scripts

The highest level of differentiation within this sub-folder is by type of data source, e.g. company reports ("detailed data") or NSOs ("country-specific").

Within these folders all of the required processing can be accessed from the main script main.R. This main scripts thereby also illustrates the structure of processing. No access of sub-scripts is required.

In each folder the required structure and scripts are different, but mainly follow the logic of data retrieval, formatting, harmonization, integration, and checking.



#### Intermediate outputs and files

Most scripts save and access intermediate files in folder `./03_intermediate/`. Most of these files are all saved as .rds-files.
Therefore other details of data can easily be accessed through these files, e.g. by loading all_tables_converted.rds.



#### Rmd outputs

Several Rmd-files are created for purposes like results from harmonization or an overview of data coverage. These are saved in sub-folders `./04_output/.../`.

These files are not included in .gitignore, in order to keep older versions for comparability as well as having a general backup of these files in case of major errors during compilation. However, obviously obsolete versions of these files can be deleted manually if necessary.



### Processing

#### detailed_data

Comprises the following:  
+ Pre-check of data for harmonization  
+ Harmonization of variables (including relevant checks)  
+ Conversion (units and production shares)  
+ Intermediate checks (e.g. double counting)  
+ Aggregation of production data (including separation of certain data tables)  
+ Gap filling  
+ Overview of coverage  
+ Compilation of ore-to-metal ratios  
+ Geo-referencing  
+ Compilation of final data output (for this type of source)  


##### Harmonization (detailed_data)

Harmonization is based on so called "source_[]_id" tables, e.g. IDs or names for materials, units and countries used by the primary source. These tables are used to allocate harmonized IDs in each case. As new data is added, which contains IDs and names not yet existent in the respective table, then these have to be added. These new entries are provided in the Rmd-output file or in the so called "no_match" csv-output files.


##### Aggregation

Is conducted in order to allow for gap filling (which would otherwise be not possible due to differences in detail between production data on ores and their contents).




#### country-specific data

Comprises different processing steps for each data source, depending on the respective requirements. These include in most cases:  
+ Data retrieval (either via API access and storage as .rsd-file, or via download of Excel-/csv-files)  
+ Formatting/cleaning (including merging of files)    
+ Harmonization of variables  
+ Conversion (units)
+ Geo-referencing  
+ Aggregation  


##### Data retrieval (country-specific data)

In some cases, data files have to be retrieved via scraping of HTML codes, due to the sheer number of files (e.g. >250 in case of Peru-MINEM). As websites are always subject to potential changes, the HTML specifications used for this purpose can alter over time, making adjustments necessary.


##### Harmonization (country-specific data)

See "Harmonization (detailed_data)" above.





#### Integration

Data from different sources is integrated with each other, giving priority to data from company reports (as it is the one which received the most scrutiny during collection and is openly available). The latter is integrated with data from country-specific sources, being also openly available.  

The integration is based on coordinates, with an additional process involving a range of other variables (e.g. state and mining type). An integration approach making use of mine names (including given other names and sub-site names) was tested, was led to erroneous results.



### Dependencies

#### R

For all required R packages, at the beginning of the main script, their installation is checked. If not installed, an installation is started.



#### Data sources

+ Detailed: Company reports
+ Country-specific: National institutions (ministries/agencies/NSOs)



## Acknowledgement
This project gratefully acknowledges financial support from the ERC as part of the [FINEPRINT](https://www.fineprint.global/) project.


