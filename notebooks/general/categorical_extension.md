## Convert numerical columns to categorical variables

Categorical variables are coded on NDA as numbers even if they have been collected as categorical variables in ABCD's database. The following steps will convert the numerical columns back into factor variables. This will matter in statistical models as factors have to be treated separately from continuous variables.

Read in the merged data (see [merge_data](notebooks/general/merge_data.md)) and a table that contains the type assignments for each factor. The second table is stored in a file that is part of this repository and has been generated from the original REDCap data dictionaries.

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
    for (c in 1:length(choices[[1]])) {
        choice = choices[[1]][c]
        number = trimws(strsplit(choice, ",")[[1]][1])
        labels = strsplit(choice, ",")[[1]][-1]
        # I am not able to simply paste the result from strsplit, use a loop instead
        label = ""
        for (i in 1:length(labels)) label = paste(label, labels[[i]], sep=", ")
        label = trimws(label)
        lev[which(lev == number)] = label
    }
    nda17[[kitty]] = factor(nda17[[kitty]],labels=lev)
}
```

We can save this new version of the ABCD data combined spreadsheet now:
```r
saveRDS(nda17, "nda17.Rds")
```
