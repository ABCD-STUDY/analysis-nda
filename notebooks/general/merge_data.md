## Merge NDA17 data into a single spreadsheet

The following steps will explain some of the perculiarities of the NDA17 data. Starting from the official download package "ABCD-RELEASE-1" the R-code below will merge the data provided in the different spreadsheets into a single large spreadsheet. Please notice that this might not be the most efficient way to handle this data. In general we would suggest to use a database layout and packages like dplyr to handle the data.

We will assume that you downloaded the spreadsheets data and placed them into a sub-directory "data" of the root folder of this project. Specify the path and read in a list of all the text files provided.

```{r path}
script.dir <- "~/src/analysis-nda17/notebooks"
setwd(script.dir)
input_list = Sys.glob(paths = c(paste(script.dir,"/../data/*.txt",sep="")))
```

Remove all files that are not required for this merge.
```{r remove}
input_list = input_list[-grep("md5_values.txt",input_list)]
input_list = input_list[-grep("package_info.txt",input_list)]
# remove fast-track image03 related data
input_list = input_list[-grep("fmriresults01.txt",input_list)]
```

Lets read in each of the tables and normalize them. This loop will run for a couple of minutes and require close to 8GB of main memory (run on a MacBook Pro). 

```{r tables}
tables = list()
for (p in 1:length(input_list)) {
    input = input_list[p]

    # read data as tab-separated table
    dt = read.table(file = input, sep = '\t',header = TRUE)

    # first data row is description, remove those
    dt = dt[-1,]

    # image data could use more than one run, lets focus on the average and remove run 1 and run 2
    if ("lmt_run" %in% names(dt) && "AVERAGE" %in% levels(dt$lmt_run)) dt = dt[dt$lmt_run == "AVERAGE",]

    # remove any column that is empty
    dt = dt[!sapply(dt, function(x) all(x==""))]

    # sometimes "eventname"" is called "visit", always use "eventname"
    if ("visit" %in% names(dt)) dt$eventname = dt$visit

    # drop columns introduced by NDA, they are not required for the resulting table
    dt = dt[,!(names(dt) %in% c("collection_id", "dataset_id", "collection_title", "promoted_subjectkey", "site", "week", "subjectkey"))]

    # remove further columns that are not required
    dt = dt[,!(names(dt) %in% c("visit", "dataset", "beh_nback_all_total", "beh_mid_perform_flag", "beh_mid_nruns"))]

    # if eventname does not exist, add it back
    if (!("eventname" %in% names(dt))) dt$eventname = "baseline_year_1_arm_1"

    # eventname can be screener, fix and merge with baseline_year_1_arm_1
    dt$eventname[which(dt$eventname == "screener")] = "baseline_year_1_arm_1"

    # only needed because imaging spreadsheets use different eventname
    dt$eventname = "baseline_year_1_arm_1"

    # re-calculate the levels
    dt = droplevels(dt)

    tables[[p]] = dt
}
```

Now merge the tables into a single spreadsheet.

```{r merge}
t2 = tables
while ( length(t2) > 1 ) {
    print("iteration")
    access= seq(1,length(t2)-1,2)
    for (i in access) {
       bm = dim(t2[[i]])
       # merge by a list of columns that should be present in each instrument
       t2[[i]] = merge(t2[[i]], t2[[i+1]], by=c("src_subject_id","eventname","interview_age","interview_date","gender"), all=TRUE)
       # debugging output, 4,524 rows should survive the merge
       print(paste(dim(t2[[i]])[1],bm[1],input_list[i],dim(t2[[i+1]])[1],input_list[i+1],i,i+1))
    }
    t2 = t2[access]
}
```

As a last step we can save the data in R's native format.

```{r export}

```