Merge NDA18 2.0.1 data into a single rds file (need memory size >=32GB)

We will assume that you downloaded the instrument package and placed them in the certain directory. Specify the path and read in a list of all the text files provided.


```r
rm(list=ls())
script.dir <- "~/Desktop/ABCD/analysis-nda18/notebooks/general"
setwd(script.dir)
input_list = Sys.glob(paths = c(paste(script.dir,"/data/ABCDstudyNDA.fix/release.2.0.1.all/*.txt",sep="")))
length(input_list)
```

Remove all files that are not required for this merge. This includes files that are related to the download process as well as files that reference the raw data sharing (Fast-Track).

```r
if (length(which(grepl("package_info",input_list))) > 0) input_list = input_list[-which(grepl("package_info",input_list))]
if (length(which(grepl("fmriresults01",input_list))) > 0) input_list = input_list[-which(grepl("fmriresults01",input_list))]
if (length(which(grepl("genomics_sample03",input_list))) > 0) input_list = input_list[-which(grepl("genomics_sample03",input_list))]
if (length(which(grepl("aurora01",input_list))) > 0) input_list = input_list[-which(grepl("aurora01",input_list))]
if (length(which(grepl("omics_experiments",input_list))) > 0) input_list = input_list[-which(grepl("omics_experiments",input_list))]
if (length(which(grepl("errors",input_list))) > 0) input_list = input_list[-which(grepl("errors",input_list))]
length(input_list)
instrument.name=NULL
for(p in 1:length(input_list)){
    instrument.name[p] = gsub('*.txt$|.txt', '', basename(input_list[p])) 
}
length(instrument.name)
```

Read each of the tables into memory. This loop will run for few hours. While reading the files the alias_mapping spreadsheet is used to replace Element Names from nda with the corresponding alias names (alias column in NDA data dictionaries). This improves the consistency and readability of the column names.

```r
alia = read.csv(paste0(script.dir,'/R.code/fix.release.R.code/NDA_DEAP_names_2.0.1.csv') )
tables = list()
for (p in 1:length(input_list)) {
  print(p)
  input = input_list[p]
  print(paste("import: ", input, " [", p, "/",length(input_list), "]", sep=""))
  
  dt <- tryCatch({
    a = read.csv(file = input, sep = '\t',header = TRUE, row.names=NULL, comment.char = "", quote="", check.names=FALSE)
    a = as.data.frame(sapply(a, function(x) gsub("\"", "", x)))
    names(a) = as.list(sapply(names(a), function(x) gsub("\"", "", x)))
    a
  }, error = function(e) {
    print(e)
    return(read.table(file = input, sep = '\t',header = TRUE))
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

Check if "visit" was used as "eventnames" in tables

```r
for (p in 1:len.tables) {
  dt = tables[[p]]
  if ("visit" %in% names(dt) ){
     print(p)
      print(instrument.name[p]) #nothing print out     
  } 
}
```

Drop columns introduced by NDA, they are not required in the instruments.
But keep dataset_id (QC purpose) and will remove later

```r
for (p in 1:len.tables) {
  dt = tables[[p]]
  dt = dt[,!(names(dt) %in% c(paste0(instrument.name[p],"_id"),"collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name"))]
 
  tables[[p]] = dt
}
```

Check columns that are all empty; rm those empty col;

```r
emptycolumns = list()
for (p in 1:len.tables) {
  dt = tables[[p]]
  emptycolumns = append(emptycolumns,names(dt)[sapply(dt, function(x) all((x=="")|(x=="NA")))])
  dt = dt[!sapply(dt, function(x) all((x=="")|(x=="NA")))]
  tables[[p]] = dt
}
emptycolumn = unlist(emptycolumns)
length(emptycolumn)
#write.csv(file="empty.col.names.csv",emptycolumn,row.names=F)
```

Remove gender/sex,interview_age and interview_date from all instrument but keepig lt01 as anchor;

```r
lt01.indx=which(instrument.name=="abcd_lt01"); #longitudinal tracking
rm.vars=c("visit","interview_age","interview_date","gender","sex") #only need these variables once
for (p in 1:len.tables) {
  dt = tables[[p]]  
  if (p != lt01.indx){
  	dt = dt[,!(names(dt) %in% rm.vars)] 
  	tables[[p]] = dt
  }
}
```

Check: if any table without eventname 

```r
for (p in 1:len.tables) {
  dt = tables[[p]]
  if (!("eventname" %in% names(dt))) {
    print(p)
    print(instrument.name[p])
  }
}
```

Re-calibrate the levels in each table. Information that has been removed in previous steps could have changed the factor information in each table.

```r
for (p in 1:len.tables) {
  dt = tables[[p]]
  dt = droplevels(dt)
  tables[[p]] = dt
}
```


Merge the individual tables into a single spreadsheet. The following loop performs repeated merging operations between pairs of  intruments.

```r

t2 = tables
rm(tables) #save space
cnt=0
while ( length(t2) > 1 ) {
  cnt=cnt+1
  print(paste0("iteration : ",cnt));
  access= seq(1,length(t2)-1,2)
  for (i in access) {
    bm = dim(t2[[i]])
    by.vars=c("src_subject_id","eventname")
    t2[[i]] = merge(t2[[i]], t2[[i+1]], by=by.vars, all=TRUE)
    print(paste("rows before: ", bm[1], dim(t2[[i+1]])[1], " rows after: ",dim(t2[[i]])[1], "indices: ",i,i+1," columns: ",bm[2],"+",dim(t2[[i+1]])[2], "-",length(by.vars)," = ",dim(t2[[i]])[2]))
  }
  # for odd number of instruments add the last spreadsheet back to the list
  if (length(t2) %% 2 != 0) access = append(access,length(t2))
  # reduce the list
  t2 = t2[access]
}
nda18 = t2[[1]]
```

Double check duplicated variables (should only see dataset_id as duplicates)

```r
dup.1=colnames(nda18)[grepl(".x",colnames(nda18),fixed=T)]
dup.2=colnames(nda18)[grepl(".y",colnames(nda18),fixed=T)]
```
rm dataset_id

```r
nda18=nda18[,-which(grepl("dataset_id",colnames(nda18)))]
```
check again

```r
dup.1=colnames(nda18)[grepl(".x",colnames(nda18),fixed=T)]
dup.2=colnames(nda18)[grepl(".y",colnames(nda18),fixed=T)]

saveRDS(nda18, "nda2.0.1_orig.Rds")
```
Next: core_demographics