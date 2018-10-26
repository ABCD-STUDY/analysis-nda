## Impute missing demographic entries

For some ABCD participants there might be missing values in some core demographic variables that are used in many anlaysis. It is benefitial to impute these missing values to be able to use the available data points for these participants.

We will assume that you have a single NDA-17 data frame (see [recode of core demographic variables](https://github.com/ABCD-STUDY/analysis-nda17/blob/master/notebooks/derived/core_demographic.md)) and you that you have followed the steps in a) [merge individual spreadsheets](https://github.com/ABCD-STUDY/analysis-nda17/blob/master/notebooks/general/merge_data.md), b) [recover categorical variables](https://github.com/ABCD-STUDY/analysis-nda17/blob/master/notebooks/general/categorical_extension.md), and c) [recode some core demographic variables](https://github.com/ABCD-STUDY/analysis-nda17/blob/master/notebooks/derived/core_demographic.md).

```r
if (!('data.table' %in% installed.packages()[,"Package"]))  install.packages('data.table')
if (!('mice' %in% installed.packages()[,"Package"]))  install.packages('mice')
library(data.table)
library(mice)

nda17$household.income[ nda17$household.income == ""] = NA
nda17$highest.education[ nda17$highest.education == ""] = NA

dat_nms = c("src_subject_id", "abcd_site", "age", "female", "race.ethnicity", "highest.education", "married", "household.income")
if (sum(as.numeric(dat_nms %in% names(nda17))) != length(dat_nms)) print("Error: missing core demographics. Add those first")
dat = data.table(nda17[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(nda17[[m]]) | (nda17[m] == "")),sep=""))
```

The above should show that household income has the most missing values (357).

```r
# Number of multiple imputed datasets & maximum number of iterations 
n.imp = 5
n.iter = 5

var.ls <- c("src_subject_id", "age", "female", "race.ethnicity", "household.income", "highest.education","married")
dat0 <- dat[, var.ls, with = FALSE ]
dat0[, table(age, useNA = "if") ]
dat0[, table(female, useNA = "if") ]
dat0[, table(race.ethnicity, useNA = "if") ]
dat0[, table(household.income, useNA = "if") ]
dat0[, table(highest.education, useNA = "if") ]
dat0[, table(married, useNA = "if") ]

ini <- mice( dat0, m = 1, maxit = 0 )
meth = ini$meth

meth["female"]     <- "logreg"
meth["married"] <- "logreg"
meth["race.ethnicity"]   <- "polyreg"
meth["household.income"]      <- "polyreg"
meth["highest.education"]  <- "polyreg"

pred = ini$pred

# Excluding variables from the imputation models
pred[, c("src_subject_id") ] <- 0
pred

# Specifying parameters for the imputation
post <- mice( dat0, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post

dat.imp <- mice( dat0, meth = meth, pred = pred, post = post,
                 seed = 1111,
                 m = n.imp, maxit = n.iter)
rm(dat0)

# get one imputed dataset out
completedData <- complete(dat.imp,1)
```

