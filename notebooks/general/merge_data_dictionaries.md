## Combine individual data dictionaries into a single spreadsheet

NDA provides a web-API that can be used to pull information about the data dictionary without the need to login. The following web-calls should therefore work for any user.

We would like to use curl to pull the JSON formatted list of ABCD data dictionaries first. Two libraries have to be installed in R to make the following merge operation work. This install operation needs to be performed only once.
```r
if (!('curl' %in% installed.packages()[,"Package"]))  install.packages('curl')
if (!('jsonlite' %in% installed.packages()[,"Package"]))  install.packages('jsonlite')
```

We start by pulling the list of all ABCD instruments from NDA:
```r
library(jsonlite)

abcd_instruments <- fromJSON("https://ndar.nih.gov/api/datadictionary/v2/datastructure?source=ABCD%20Release%201.1")
# print the list
abcd_instruments$shortName
```

The next step downloads the NDA versions of each ABCD data dictionary and merges them together into a single data frame.
```r
dd = data.frame()
for ( i in 1:length(abcd_instruments$shortName)) {
    inst_name = abcd_instruments$shortName[i]
    inst <- fromJSON(paste("https://ndar.nih.gov/api/datadictionary/v2/datastructure/", inst_name, sep=""))
    inst = inst$dataElements
    # The alias names for each element name are in a list, concatenate that list into a single string
    aliases = lapply(inst$aliases, function(x) { str = ""; if (length(x) > 0) { for( i in 1:length(x)) { str = paste(str, x[[i]], sep=" ") } }; trimws(str);})
    # create a new data frame
    nd = data.frame("Element Name"=inst$name, "Element Description"=inst$description, "type"=inst$type, "valueRange"=inst$valueRange, "notes"=inst$notes, "aliases"=unlist(aliases))
    # and merge
    if (dim(dd)[1] == 0) { dd <- nd } else { dd <- merge(dd, nd,all=TRUE) }
    print(paste("Merged", inst_name, i,"/",length(abcd_instruments$shortName),sep=" "))
}
```
The resulting data dictionary will have close to 43,000 entries. Only about 38,000 are actually provided by ABCD and contain values.

Save the merged data dictionary as a spreadsheet.
```r
fname <- paste(getwd(),"ABCD_data_dictionary.csv", sep="/")
print(paste("write data dictionary to ",fname,sep=""))
write.csv(dd,fname)
```

