## Create a single data Rds file for ABCD release 2.0.1.

 More details can be found and downloaded at [NDA ABCD RDS release website] (https://nda.nih.gov/study.html?id=796)

### Step 1: download pre-packaged data (including updated tables from release 2.0.1 and unchanged tables from release 2.0)

### Step 2: Run merge_data to merge instruments
- [Merge using R markdown](notebooks/general/merge_data.Rmd)

### Step 3: Run core_demographics to recode/create few core demographic variables
- [Recode some core demographic variables (using R markdown)](notebooks/derived/core_demographic.Rmd)

### Step 4: Run categorical_extension to recover categorical variables as factor and numerical variables as numeric and redefine "race" variable. 
 - [Recover categorical variables (using R markdown)](notebooks/general/categorical_extension.Rmd)

### Step 5: Run merge.additional.variables to merge additional derived variables
 - [Merge additional variables (using R markdown)](notebooks/general/merge.additional.variables.Rmd)

### Notes: 
- The R codes have to be run in that order; 
- Two csv files, choices_coding_nda2.0.1.csv and NDA_DEAP_names_2.0.1.csv are called by R codes during the process. Memory size >=32GB is recommended.