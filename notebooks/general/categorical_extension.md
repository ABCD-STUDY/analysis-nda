## Convert some numerical columns to categorical variables

Categorical variables are coded on NDA as numbers even if they have been collected as categorical variables in ABCD's database. The following steps will convert the numerical columns back into factor variables. This will matter in statistical models as factors have to be treated separately from continuous variables.

Read in the merged data (see [merge_data](notebooks/general/merge_data.md)) and a table that contains the factors for each categorical variable. The second table is stored in a csv file that is part of this repository and has been generated from the original REDCap data dictionaries.

```r
nda17 = readRDS("nda17.Rds")
categories = read.csv('choices_coding.csv')
```

This loop will go throught the column names stored in the categories table and convert those columns in the nda17 data frame to factor variables.

```r
for (kitty in categories$name) {
    if (!(kitty %in% names(nda17))) next
    choices = strsplit(as.character(categories[categories$name == kitty,]$choices), "|",fixed=TRUE)
    lev = levels(nda17[[kitty]])
    orig_levels = lev
    for (c in 1:length(choices[[1]])) {
        choice = choices[[1]][c]
        number = trimws(strsplit(choice, ",")[[1]][1])
        labels = strsplit(choice, ",")[[1]][-1]
        # I am not able to simply paste the result from strsplit, use a loop instead
        label = labels[[1]]
        if (length(labels)>1) for (i in 2:length(labels)) label = paste(label, labels[[i]], sep=",")
        label = trimws(label)
        lev[which(lev == number)] = label
    }
    nda17[[kitty]] = factor(nda17[[kitty]],levels=orig_levels, labels=lev)
}
```

Continuous variables are imported as characters from the NDA tables due to enclosing double-quotes. We can convert them back into numerical values based on the type contained in the NDA data dictionary (and the known list of categorical variables):
```r
# pull and merge the data dictionaries for ABCD from NDA
if (!('curl' %in% installed.packages()[,"Package"]))  install.packages('curl')
if (!('jsonlite' %in% installed.packages()[,"Package"]))  install.packages('jsonlite')
library(jsonlite)
abcd_instruments <- fromJSON("https://ndar.nih.gov/api/datadictionary/v2/datastructure?source=ABCD")
numbers = list()
for ( i in 1:length(abcd_instruments$shortName)) {
    inst_name = abcd_instruments$shortName[i]
    inst <- fromJSON(paste("https://ndar.nih.gov/api/datadictionary/v2/datastructure/", inst_name, sep=""))
    numbers = append(numbers, inst$dataElements[inst$dataElements$type %in% c("Integer","Float"),]$name)
}
# remove known categorical variables
numbers = numbers[!(numbers %in% categories$name)]
for (i in 1:length(numbers)) {
    nda17[unlist(numbers[i])] = as.numeric(as.character(nda17[[unlist(numbers[i])]]))
}
```

We can save this new version of the ABCD data combined spreadsheet now:
```r
saveRDS(nda17, "nda17.Rds")
```

Looking at the factor levels you will find that some of them are more difficult to read than others. Some contain HTML instructions for two language versions (English and Spanish) for each level. For dropdown entries the language encoding cannot use HTML, instead the following pattern is used: '##en##English##/en## ##es##Spanish##/es##'.
