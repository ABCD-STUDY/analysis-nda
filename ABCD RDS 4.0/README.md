# ABCD-RDS-4.0
This repository stores script for merging the NDA ABCD 4.0 released data into an .Rds file
Steps in detail:

1. Start with '01_merge_data_dictionaries.R', change to 4.0 or other versions at line 30.
   Download and save the file: 'abcd_instruments.csv'
   Query the API at line 48, download and save the file: 'ABCD_data_dictionary.csv'
   Generate and save the fie: 'NDA_DEAP_names_4.0.csv'  
2. Start going into the merging the scripts with '02_data_merge_rds_4_0_server.R'
   Some changes and adjustment were made when loading in the files and read into tables,
   to address some specific issues with certain files. For example, move the position of a column,
   update a column name, or skip files, etc.
   Some were also made in the later processing steps.   
3. Non-img files are then used in '03_core_demographics_4_0.R' script.
   One thing different from Rds 3.0 is that the 'anthro_weight_calc' is empty for all of the data. 
   Referencing to the data dictionary, it was then calculated accordingly.  
4. Before going into '05_categorical_extension_4_0.R', use '04_create_choice_coding_nda_4.R' to create 'choices_coding_nda.4.0.csv.'  
5. Use '05_categorical_extension_4_0.R', stop there for image files. Continue to '06_merge_additional_variables.R' for non_img files.  
6. After generating the Rds files, use '07_rm_dup_cols_rds_4.R' to check or clean the duplicated column names, 
   and remove them for each of the Rds file.  
7. Use '08_merge_all_rds.R' to merge the three .Rds file into one.  
8. '09_merge_fitbit_rds.R' is used for merge fitbit data only, which were earlier excluded in merging in '02_data_merge_rds_4_0_server.R'  
