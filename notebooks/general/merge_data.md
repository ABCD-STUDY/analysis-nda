## Merge NDA17 data into a single spreadsheet

Starting from the official download package "ABCD-RELEASE-1" the R-code below will merge the data tables into a single large spreadsheet. Please notice, that this might not be the most efficient way to handle the data. In general we would suggest to use a database layout and packages like dplyr. Nevertheless the code below is provided to illustrate some of the perculiarities of the data.

We will assume that you downloaded the spreadsheets data (3.2GB) and placed them in the directory "data" of the root folder of this project. Specify the path and read in a list of all the text files provided.

```r
script.dir <- "~/src/analysis-nda17/notebooks/general"
setwd(script.dir)
input_list = Sys.glob(paths = c(paste(script.dir,"/../../data/*.txt",sep="")))
```

Remove all files that are not required for this merge. This includes files that are related to the download process as well as files that reference the raw data sharing (Fast-Track).

```r
input_list = input_list[-grep("md5_values.txt",input_list)]
input_list = input_list[-grep("package_info.txt",input_list)]
input_list = input_list[-grep("fmriresults01.txt",input_list)]
```

Read each of the tables into memory. This loop will run for several minutes and requires close to 8GB of main memory. While reading the files the alias_mapping spreadsheet is used to replace Element Names from nda with the corresponding alias names (alias column in NDA data dictionaries). This improves the consistency and readability of the column names.

```r
alia = read.csv('alias_mapping.csv')
tables = list()
for (p in 1:length(input_list)) {
    input = input_list[p]
    print(paste("import: ", input, " [", p, "/",length(input_list), "]", sep=""))

    # read data from tab-separated table as characters
    dt = read.table(file = input, sep = '\t',header = TRUE)

    # replace variable names from nda with their alias names to make them more like ABCD
    instrument = sub('\\.txt$', '', basename(input_list[p]))
    ali = alia[which(alia$instrument == instrument),]
    nn = names(dt)
    for (q in 1:length(nn)) {
        if (nn[q] %in% ali$nda) {
            colnames(dt)[q] <- as.character(ali$abcd[ali$nda == nn[q]])
        }
    }

    tables[[p]] = dt
}
```

The first row in each spreadsheet is the element description. Lets remove those for our data tables. This information is already present in the [ABCD Data Dictionaries](https://ndar.nih.gov/data_dictionary.html?source=ABCD&submission=ALL).
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    dt = dt[-1,]
    dt = droplevels(dt)
    tables[[p]] = dt
}
```

Only keep the last submission's data. This section can be removed as soon as the download ABCD-RELEASE-1 has been corrected.
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    if (length(levels(dt$dataset_id)) > 1) dt = dt[which(dt$dataset_id==max(as.integer(levels(dt$dataset_id)))),]
    tables[[p]] = dt
}
```

Image data could use more than one run, lets focus on the average data and remove run 1 and run 2:
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    if ("lmt_run" %in% names(dt) && "AVERAGE" %in% levels(dt$lmt_run)) dt = dt[dt$lmt_run == "AVERAGE",]
    tables[[p]] = dt
}
```

Only for image data some columns duplicate across the different fMRI tasks. Merging these spreadsheets would create additional ".x" column names. Lets rename these columns to include the instrument name, which would make the names unique again.
```r
varlist = c("scanner_manufacturer_pd", "scanner_type_pd", "deviceserialnumber", "magnetic_field_strength", "procdate", "pipeline_version", "mid_beta_seg_dof", "fmri_beta_gparc_tr", "fmri_beta_gparc_numtrs", "fmri_beta_gparc_mean_motion", "rsfm_tr", "rsfm_nreps", "rsfm_numtrs", 
"rsfm_mean_motion", "rsfm_max_motion", "rsfm_mean_trans", "rsfm_max_trans", "rsfm_mean_rot", "rsfm_max_rot", "respond", "dti_mean_motion",
"dti_mean_trans", "dti_mean_rot")
for (p in 1:length(tables)) {
    dt = tables[[p]]
    for (q in 1:length(varlist)) {
        if (varlist[q] %in% names(dt)) {
            newname = paste(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(input_list[p])), ".", varlist[q], sep="")
            print(paste("change: ", varlist[q], p, " ", newname))
            colnames(dt)[which(names(dt) == varlist[q])] = newname
        }
    }
    tables[[p]] = dt
}
```


To conserve memory lets remove any column that is empty. These columns might have been introduced because other projects use ABCD instruments or, in rare cases because data had to be sanitized before it was exported.
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    dt = dt[!sapply(dt, function(x) all((x=="")|(x=="NA")))]
    tables[[p]] = dt
}
```

Sometimes the "eventname" column shared in many instruments is called "visit". Lets always use "eventname":
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    if ("visit" %in% names(dt)) dt$eventname = dt$visit
    tables[[p]] = dt
}
```

Drop columns introduced by NDA, they are not required in the resulting table.

```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    dt = dt[,!(names(dt) %in% c("collection_id", "dataset_id", "collection_title", "promoted_subjectkey", "site", "week", "subjectkey", "study_cohort_name"))]
    tables[[p]] = dt
}
```

There are some other columns that appear in more than on instrument. The last merge step would introduce duplicate columns if they remain in the data.
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    dt = dt[,!(names(dt) %in% c("visit", "dataset", "beh_nback_all_total", "beh_mid_perform_flag", "beh_mid_nruns", "lmt_run"))]
    tables[[p]] = dt
}
```

Add back the eventname column if it does not exist. This assumes that we are working with the NDA-17 baseline data.
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    if (!("eventname" %in% names(dt))) dt$eventname = "baseline_year_1_arm_1"
    tables[[p]] = dt
}
```

The value in the eventname column is sometimes "screener". Fix these entries by setting the value to "baseline_year_1_arm_1".
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    dt$eventname[which(dt$eventname == "screener")] = "baseline_year_1_arm_1"
    tables[[p]] = dt
}
```

Imaging spreadsheets use a different structure for the eventname column that depends on the site, the participant and the date and time of the scan. In order to align the imaging with the non-imaging data lets set the event name to "baseline_year_1_arm_1".
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    dt$eventname = "baseline_year_1_arm_1"
    tables[[p]] = dt
}
```

As a final step, re-calculate the levels in each table. Information that has been removed in previous steps could have changed the factor information in each table.
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    dt = droplevels(dt)
    tables[[p]] = dt
}
```

Now we can merge the individual tables into a single spreadsheet. The following loop performs repeated merging operations between pairs of  spreadsheets.

```r
t2 = tables
while ( length(t2) > 1 ) {
    print("iteration")
    access= seq(1,length(t2)-1,2)
    for (i in access) {
       bm = dim(t2[[i]])
       # merge by a list of columns that should be present in each instrument, replace the first element with the merge result
       t2[[i]] = merge(t2[[i]], t2[[i+1]], by=c("src_subject_id","eventname","interview_age","interview_date","gender"), all=TRUE)
       # debugging output, 4,524 rows should survive the merge
       print(paste("rows before: ", bm[1], dim(t2[[i+1]])[1], " rows after: ",dim(t2[[i]])[1], "indices: ",i,i+1," columns: ",bm[2],"+",dim(t2[[i+1]])[2], " = ",dim(t2[[i]])[2]))
    }
    # for odd number of instruments add the last spreadsheet back to the list
    if (length(t2) %% 2 != 0) access = append(access,length(t2))
    # reduce the list
    t2 = t2[access]
}
nda17 = t2[[1]]
```
The nda17 data frame should contain 4,524 rows and about 38,000 columns. As a last step we can save the data in R's native file format (780MB).

```r
saveRDS(nda17, "nda17_orig.Rds")
```

In order to read the data back into memory (5.5GB) use:
```r
nda17 = readRDS("nda17_orig.Rds")
```

### Notes

NDA data is shared as numerical columns even if the data represented in the columns is categorical. As an extreme case values '0' and '1' for example might represent in one spreadsheet the categories 'Yes' and 'No', in another spreadsheet they might represent 'No' and 'Yes'. The respective NDA data dictionary will let the user know what coding was used (see Notes column).

See the [categorical_extension script](categorical_extension.md) on how to convert the appropriate numerical columns from NDA into categorical/factor variables.

