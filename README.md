## Create a single data Rds file for ABCD release 2.0.1.

 More details can be found and downloaded at [NDA ABCD RDS release website] (https://nda.nih.gov/study.html?id=796)

### Step 1: download pre-packaged data (including updated tables from release 2.0.1 and unchanged tables from release 2.0)

### Step 2: Run merge_data to merge instruments
- [Merge using R markdown](notebooks/general/merge_data.md)

### Step 3: Run core_demographics to recode/create few core demographic variables
- [Recode some core demographic variables (using R markdown)](notebooks/derived/core_demographics.md)

### Step 4: Run categorical_extension to recover categorical variables as factor and numerical variables as numeric and redefine "race" variable. 
 - [Recover categorical variables (using R markdown)](notebooks/general/categorical_extension.md)

### Step 5: Run merge.additional.variables to merge additional derived variables
 - [Merge additional variables (using R markdown)](notebooks/general/merge.additional.variables.md)

### Notes: 
- The R codes have to be run in that order; 
- Two csv files, choices_coding_nda2.0.1.csv and NDA_DEAP_names_2.0.1.csv are called by R codes during the process. Memory size >=32GB is recommended.

The ABCD data collection and scoring instruments are shared on the NDA platform as NDA data dictionaries. As there are substantial differences between the REDCap data collection instruments used by ABCD and the NDA structure for data dictionaries here a short map implemented by the export application https://github.com/ABCD-STUDY/redcap-to-nda, which explain how ABCD REDCap instruments are translated into NDA data sharing instruments during data export:

- Element.Name: NDA official name of the item. Might correspond to ABCD's name for the item but see section about aliases.
- Element.Description: For raw scores the question used during data collection towards the participant or parent. For calculated scores the description of the score.
- Type: The NDA type for the entries (String, Number, Float, etc.). This information is used during data validation if ABCD submits data to NDA.
- valueRange: A list of semicolon separated entries for valid data entries. This might contain NA and keys indicating reasons for missingness (999, 777, etc.).
- notes: This field captures most of the REDCap specific data interpretation information as no NDA specific fields exist that can capture this. There are three fields that are appended here. Any one of these three fields can be empty. The separating characters are: <key/value mapping> | <field notes> / <REDCap branching logic>. The key/value mappings are semicolon separated <key> = <value> entries that explain the numeric keys with human readable values as they appear in for example drop-down entries. Field notes are freeform text entries that are displayed underneath the data entry fields in the ABCD data collection instruments. The branching logic entry lists the conditions (evaluates to TRUE/FALSE) under which this item is displayed to the user based on the values of previously collected information. Such branching logic can explain why fields are empty or NA.
- aliases: Entries in this field are comma-separated element names that all map to the same Element.Name. As projects on NDA re-use fields additional aliases can appear. Branching logic fields might use either the Element.Name (for ABCD items) or one of the alias names (if the Element.Name is from NDA).
- NDA.Instrument: The entry in this field separates items into instruments using the instruments short-name. Such short-names are introduced by NDA based on individual data collection instruments from the study or if original data collection instruments have more than 1,000 items (limitation due to the Oracle database structure of NDA).

