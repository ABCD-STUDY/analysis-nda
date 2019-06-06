## Merge NDA18 2.0 data into a single spreadsheet

Starting from the official download package "ABCDstudyNDA" (Study 634) the R-code below will merge the data tables into a single large spreadsheet. Please notice, that this might not be the most efficient way to handle the data. In general we would suggest to use a database layout and packages like dplyr. Nevertheless, the code below is provided to illustrate some of the perculiarities of the data.

There are three sets of instructions that should be run on order. This file (merge_data) the core_demographics, followed by the categorical_extension.

We will assume that you downloaded the spreadsheet data (7.7GB) from the [NIMH data archive](https://data-archive.nimh.nih.gov/abcd/query/abcd-interim-annual-release-2.0.html) and placed them in the directory "data" of the root folder of this project. Specify the path and read in a list of all the text files provided.

```r
rm(list=ls())
script.dir <- "~/src/analysis-nda/notebooks/general"
setwd(script.dir)
input_list = Sys.glob(paths = c(paste(script.dir,"/../../data/*.txt",sep="")))
```

Remove all files that are not required for this merge. This includes files that are related to the download process as well as files that reference the raw data sharing (Fast-Track).

```r
if (length(which(grepl("package_info",input_list))) > 0) input_list = input_list[-which(grepl("package_info",input_list))]
if (length(which(grepl("fmriresults01",input_list))) > 0) input_list = input_list[-which(grepl("fmriresults01",input_list))]
if (length(which(grepl("genomics_sample03",input_list))) > 0) input_list = input_list[-which(grepl("genomics_sample03",input_list))]
if (length(which(grepl("aurora01",input_list))) > 0) input_list = input_list[-which(grepl("aurora01",input_list))]
if (length(which(grepl("omics_experiments",input_list))) > 0) input_list = input_list[-which(grepl("omics_experiments",input_list))]
if (length(which(grepl("errors",input_list))) > 0) input_list = input_list[-which(grepl("errors",input_list))]

instrument.name=NULL
for(p in 1:length(input_list)){
    instrument.name[p] = gsub('*.txt$|.txt', '', basename(input_list[p])) 
}
```

Read each of the tables into memory. This loop will run for several minutes and requires close to 32GB of main memory. While reading the files the alias_mapping spreadsheet is used to replace Element Names from nda with the corresponding alias names (alias column in NDA data dictionaries). This improves the consistency and readability of the column names.

```r
alia = read.csv('NDA_DEAP_names_2.0.csv')
tables = list()
for (p in 1:length(input_list)) {
    print(p)
    input = input_list[p]
    print(paste("import: ", input, " [", p, "/",length(input_list), "]", sep=""))

    # read data from the tab-separated files as characters, don't use the usual comment character (can be in second row of item description)
    dt <- tryCatch({
      a = read.csv(file = input, sep = '\t',header = TRUE, row.names=NULL, comment.char = "", quote="", check.names=FALSE)
      a = as.data.frame(sapply(a, function(x) gsub("\"", "", x)))
      names(a) = as.list(sapply(names(a), function(x) gsub("\"", "", x)))
      a
    }, error = function(e) {
       print(e)
      read.table(file = input, sep = '\t',header = TRUE)
    })

    # replace variable names from nda with their alias names to make them more like ABCD
    instrument=instrument.name[p]
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
len.tables=length(tables)
for (p in 1:len.tables) {
  dt = tables[[p]]
  dt = dt[-1,]
  dt = droplevels(dt)
  tables[[p]] = dt
}
```

Sometimes the "eventname" column shared in many instruments is called "visit". In freesqc01 both columns exist and are different:
```r
for (p in 1:len.tables) {
  dt = tables[[p]]
  if ("visit" %in% names(dt) ){
     print(p)
      print(instrument.name[p])
      dt$eventname = dt$visit
  }
  tables[[p]] = dt
}
```

Drop columns introduced by NDA, they are not required in the resulting table.

```r
for (p in 1:len.tables) {
  dt = tables[[p]]
  dt = dt[,!(names(dt) %in% c("collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name"))]
  tables[[p]] = dt
}
lt01.indx=which(instrument.name=="abcd_lt01"); #longitudinal tracking
```

There are some other columns that appear in more than one instrument. The last merge step would introduce duplicate columns if they remain in the data. Remove interview_age and interview_date from all instrument but keeping lt01 as anchor.

```r
rm.vars=c("visit","interview_age","interview_date","gender")
for (p in 1:len.tables) {
  dt = tables[[p]]
  if (instrument.name[p]=="abcd_midabwdp201"){ #both "abcd_midabwdp201" and "abcd_midabwdp01" have the same variables (same values), delete one;
    dt = dt[,!(names(dt) %in% c("tfmri_mid_all_antic.large.vs.small.reward_beta_cort.destrieux_g.front.inf.orbital.rh",rm.vars))]
   
  } else if (instrument.name[p] == "abcd_dmdtifp201"){ #both abcd_dmdtifp101 and abcd_dmdtifp201 have the same variable, delete one;
    dt = dt[,!(names(dt) %in% c("dmri_dtifull_visitid",rm.vars))]
  } else if (p != lt01.indx){
    dt = dt[,!(names(dt) %in% rm.vars)] 
  }
  
  tables[[p]] = dt
}
```

Starting with release 2.0 the ABCD study releases data for more than the baseline event. Lets get the sample size for each event:
```r
lt.bl=tables[[lt01.indx]][which(tables[[lt01.indx]]$eventname=="baseline_year_1_arm_1"),]
dim(lt.bl)

lt.1yr=tables[[lt01.indx]][which(tables[[lt01.indx]]$eventname=="1_year_follow_up_y_arm_1"),]
dim(lt.1yr)

lt.18m=tables[[lt01.indx]][which(tables[[lt01.indx]]$eventname=="18_month_follow_up_arm_1"),]
dim(lt.18m)

lt.6m=tables[[lt01.indx]][which(tables[[lt01.indx]]$eventname=="6_month_follow_up_arm_1"),]
dim(lt.6m)

# total in 4 events
dim(lt.bl)[1] +  dim(lt.1yr)[1] +  dim(lt.18m)[1] +  dim(lt.6m)[1]

event.tot=c(dim(lt.bl)[1] ,  dim(lt.1yr)[1]  ,  dim(lt.18m)[1] ,  dim(lt.6m)[1])
diff.event=c("baseline_year_1_arm_1","1_year_follow_up_y_arm_1","18_month_follow_up_arm_1","6_month_follow_up_arm_1")
# check: if any table without eventname 
for (p in 1:len.tables) {
  dt = tables[[p]]
  if (!("eventname" %in% names(dt))) {
    print(p)
    print(instrument.name[p])
  }
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
rm(tables)
while ( length(t2) > 1 ) {
    print("iteration")
    access= seq(1,length(t2)-1,2)
    for (i in access) {
       bm = dim(t2[[i]])

       by.vars=c("src_subject_id","eventname")
       t2[[i]] = merge(t2[[i]], t2[[i+1]], by=by.vars, all=TRUE)

       print(paste("rows before: ", bm[1], dim(t2[[i+1]])[1], " rows after: ",dim(t2[[i]])[1], "indices: ",i,i+1," columns: ",bm[2],"+",dim(t2[[i+1]])[2], " = ",dim(t2[[i]])[2]))
    }
    # for odd number of instruments add the last spreadsheet back to the list
    if (length(t2) %% 2 != 0) access = append(access,length(t2))
    # reduce the list
    t2 = t2[access]
}
nda18 = t2[[1]]
nda18=nda18[,-which(grepl("dataset_id",colnames(nda18)))]
```

As a nicety we can sort the levels of eventname by timepoint. 
```
nda18$eventname = factor(nda18$eventname, levels(nda18$eventname)[c(2,4,1,3)])
```

The nda18 data frame should contain 27,368 rows (baseline: 11,875; 6 month: 8,623; 1 year: 4,951; and 18 month: 1,919) and about 65,800 columns. As a last step we can save the data in R's native file format (4.4GB).

```r
saveRDS(nda18, "nda18_orig.Rds")
names.nda18=colnames(nda18)
save(file="names.nda18.RData",names.nda18)
```

In order to read the data back into memory use:
```r
nda18 = readRDS("nda18_orig.Rds")
```

The next step in processing the data is adding the core demographics [core_demographcs](notebooks/derived/core_demographic.md).

### Notes

NDA data is shared as numerical columns even if the data represented in the columns is categorical. As an extreme case values '0' and '1' for example might represent in one spreadsheet the categories 'Yes' and 'No', in another spreadsheet they might represent 'No' and 'Yes'. The respective NDA data dictionary will let the user know what coding was used (see Notes column).

See the [categorical_extension script](categorical_extension.md) on how to convert the appropriate numerical columns from NDA into categorical/factor variables.

