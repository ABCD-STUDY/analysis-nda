## Merge NDA18 2.0 data into a single spreadsheet

Starting from the official download package "Study634" the R-code below will merge the data tables into a single large spreadsheet. Please notice, that this might not be the most efficient way to handle the data. In general we would suggest to use a database layout and packages like dplyr. Nevertheless the code below is provided to illustrate some of the perculiarities of the data.

We will assume that you downloaded the spreadsheet data (11GB) and placed them in the directory "data" of the root folder of this project. Specify the path and read in a list of all the text files provided.

```r
script.dir <- "~/src/analysis-nda17/notebooks/general"
setwd(script.dir)
input_list = Sys.glob(paths = c(paste(script.dir,"/../../data/*.txt",sep="")))
```

Remove all files that are not required for this merge. This includes files that are related to the download process as well as files that reference the raw data sharing (Fast-Track).

```r
if (length(grep("package_info.txt",input_list)) > 0) input_list = input_list[-grep("package_info.txt",input_list)]
if (length(grep("fmriresults01.txt",input_list)) > 0) input_list = input_list[-grep("fmriresults01.txt",input_list)]
if (length(grep("genomics_sample03.txt",input_list)) > 0) input_list = input_list[-grep("genomics_sample03.txt",input_list)]
if (length(grep("aurora01.txt",input_list)) > 0) input_list = input_list[-grep("aurora01.txt",input_list)]
```

Read each of the tables into memory. This loop will run for several minutes and requires close to 8GB of main memory. While reading the files the alias_mapping spreadsheet is used to replace Element Names from nda with the corresponding alias names (alias column in NDA data dictionaries). This improves the consistency and readability of the column names.

```r
alia = read.csv('NDA_DEAP_names_2.0.csv')
tables = list()
for (p in 1:length(input_list)) {
    print(p)
    input = input_list[p]
    print(paste("import: ", input, " [", p, "/",length(input_list), "]", sep=""))

    # read data from the tab-separated files as characters, don't use the usual comment character (can be in second row of item description)
    dt = read.table(file = input, sep = '\t',header = TRUE, comment.char = "")

    # replace variable names from nda with their alias names to make them more like ABCD
    instrument = sub('\\.txt$', '', basename(input_list[p]))
    ali  = alia[which(alia$instrument == instrument),]
    nn = names(dt)
    for (q in 1:length(nn)) {
        if (nn[q] %in% ali$nda) {
            colnames(dt)[q] <- as.character(ali$abcd[ali$nda == nn[q]])
        }
    }
    tables[[p]] = dt
}
```

The first row in each spreadsheet is the element description. Lets remove those for our data tables. This information is already present in the [ABCD Data Dictionaries](https://ndar.nih.gov/data_dictionary.html?source=ABCD%2BRelease%2B2.0&submission=ALL).
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    dt = dt[-1,]
    dt = droplevels(dt)
    tables[[p]] = dt
}
```

To conserve memory we could remove columns that are empty. In the initial 1.0 release this happened for a lot of columns. In the current 1.1 release only about 2,000 columns are affected. Most of those are empty because they are secured by branching logic. Lets keep them in the merged dataset.
```r
emptycolumns = list()
for (p in 1:length(tables)) {
    dt = tables[[p]]
    emptycolumns = append(emptycolumns,names(dt)[sapply(dt, function(x) all((x=="")|(x=="NA")))])
    #dt = dt[!sapply(dt, function(x) all((x=="")|(x=="NA")))]
    #tables[[p]] = dt
}
emptycolumn = unlist(emptycolumns)
```

Sometimes the "eventname" column shared in many instruments is called "visit". Lets always use "eventname":
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    if ("visit" %in% names(dt))
       dt$eventname = dt$visit
#      print(paste(p, input_list[p], levels(dt$visit)))
    tables[[p]] = dt
}
```

Drop columns introduced by NDA, they are not required in the resulting table.

```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    dt = dt[,!(names(dt) %in% c("collection_id", "dataset_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name"))]
    tables[[p]] = dt
}
```

There are some other columns that appear in more than on instrument. The last merge step would introduce duplicate columns if they remain in the data.
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    dt = dt[,!(names(dt) %in% c("visit", "lmt_run"))]
    tables[[p]] = dt
}
```

Add back the eventname column if it does not exist. This assumes that we are working with the NDA-18 baseline data. Currently there are two instruments that don't have that column. The Mobile technology instrument used for the Fitbit pilot (baseline only) and the fmriresults instrument used for sharing minimally processed data (baseline only).
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
    if (!("eventname" %in% names(dt))) 
      dt$eventname = "baseline_year_1_arm_1"
#       print(paste(input_list[p], p))
    tables[[p]] = dt
}
```

Imaging spreadsheets use a different structure for the eventname column that depends on the site, the participant and the date and time of the scan. In order to align the imaging with the non-imaging data lets set the event name to "baseline_year_1_arm_1".
```r
for (p in 1:length(tables)) {
    dt = tables[[p]]
#    dt$eventname = "baseline_year_1_arm_1"
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
       #t2[[i]] = merge(t2[[i]], t2[[i+1]], by=c("src_subject_id","eventname","interview_age","interview_date","gender"), all=TRUE)
       # interview_date is not in abcd_ysuip01, don't use for merging
       t2[[i]] = merge(t2[[i]], t2[[i+1]], by=c("src_subject_id","eventname","interview_age","gender"), all=TRUE)
       # debugging output, 4,521 rows should survive the merge
       print(paste("rows before: ", bm[1], dim(t2[[i+1]])[1], " rows after: ",dim(t2[[i]])[1], "indices: ",i,i+1," columns: ",bm[2],"+",dim(t2[[i+1]])[2], " = ",dim(t2[[i]])[2]))
    }
    # for odd number of instruments add the last spreadsheet back to the list
    if (length(t2) %% 2 != 0) access = append(access,length(t2))
    # reduce the list
    t2 = t2[access]
}
nda17 = t2[[1]]
```
The nda17 data frame should contain 4,521 rows and about 30,000 columns. As a last step we can save the data in R's native file format (580MB).

```r
saveRDS(nda17, "nda17_orig.Rds")
```

In order to read the data back into memory (5.5GB) use:
```r
nda17 = readRDS("nda17_orig.Rds")
```

The next step in processing the data is adding the core demographics [core_demographcs](../derived/core_demographics.md).

### Notes

NDA data is shared as numerical columns even if the data represented in the columns is categorical. As an extreme case values '0' and '1' for example might represent in one spreadsheet the categories 'Yes' and 'No', in another spreadsheet they might represent 'No' and 'Yes'. The respective NDA data dictionary will let the user know what coding was used (see Notes column).

See the [categorical_extension script](categorical_extension.md) on how to convert the appropriate numerical columns from NDA into categorical/factor variables.

