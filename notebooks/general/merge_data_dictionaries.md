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

abcd_instruments <- fromJSON("https://ndar.nih.gov/api/datadictionary/v2/datastructure?source=ABCD%20Release%202.0")
# print the list
abcd_instruments$shortName
```

The next step downloads the NDA versions of each ABCD data dictionary and merges them together into a single data frame.
```r
dd = data.frame()
for ( i in 1:length(abcd_instruments$shortName)) {
    inst_name = abcd_instruments$shortName[i]
    print(paste("Try to get: ", inst_name))
    inst <- fromJSON(paste("https://ndar.nih.gov/api/datadictionary/v2/datastructure/", inst_name, sep=""))
    inst = inst$dataElements
    # The alias names for each element name are in a list, concatenate that list into a single string
    aliases = lapply(inst$aliases, function(x) { str = ""; if (length(x) > 0) { for( i in 1:length(x)) { str = paste(str, x[[i]], sep=" ") } }; trimws(str);})
    # create a new data frame
    nd = data.frame("Element Name"=inst$name, "Element Description"=inst$description, "type"=inst$type, "valueRange"=inst$valueRange, "notes"=inst$notes, "aliases"=unlist(aliases), "NDA Instrument"=inst_name)
    # and merge
    if (dim(dd)[1] == 0) { dd <- nd } else { dd <- merge(dd, nd,all=TRUE) }
    print(paste("Merged", inst_name, i,"/",length(abcd_instruments$shortName),sep=" "))
}
```
The resulting data dictionary will have close to 66,000 entries. Only about 38,000 are actually provided by ABCD and contain values.

Save the merged data dictionary as a spreadsheet.
```r
fname <- paste(getwd(),"ABCD_data_dictionary.csv", sep="/")
print(paste("write data dictionary to ",fname,sep=""))
write.csv(dd,fname)
```

Since release 2.0 the NDA dictionary contains all the alias names used on the DEAP system. In order to generate the NDA_DEAP_names_2.0.csv we can use the following code:
```r
numAliases = length(which(!is.na(dd$aliases)))
aliasNames = data.frame("nda"=character(numAliases), "abcd"=character(numAliases), "instrument"=character(numAliases))
for (i in 1:dim(dd)[1]) {
  if (dd[i,]$aliases != "") {
    # There can be a list of alias names. We want to use only a single alias and we want to prefer to use the alias that is longest.
    alias = strsplit(as.character(dd[i,]$aliases), " ")[[1]][1]
    aliasNames[i,] = data.frame("nda"=dd[i,]$Element.Name,"abcd"=alias,"instrument"=as.character(dd[i,]$NDA.Instrument))
  }
}
write.csv(aliasNames, "NDA_DEAP_names_2.0.csv")
```
