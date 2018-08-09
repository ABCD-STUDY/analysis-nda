## Definition of convenience variables

The following sections extend the nda17 data frame (see [creating a single data spreadsheet](https://github.com/ABCD-STUDY/analysis-nda17#create-a-single-data-spreadsheet)) by some core demographic columns.
 - subjectid
 - age in years
 - female
 - race.ethnicity
 - household.income
 - high.educ
 - married

Most of these are simple re-definitions of existing columns with simplier names, other columns are re-scored versions of nda17 columns.

Start by reading in the merged data from disk.
```r
nda17 = readRDS("nda17.Rds")
```

Now extend nda17 by the new columns.

### Subjectid

```r
nda17$subjectid = nda17$src_subject_id
```

### Age (in month)

```r
nda17$age = nda17$interview_age
```

### Female

```r
nda17$female = factor(as.numeric(nda17$gender == "F"), levels = 0:1, labels = c("no", "yes") ) 
```

On NDA the "gender" variable is listed as containing "Sex at birth". Lets create a copy called "sex".
```
nda17$sex = nda17$gender
```


### Household income

```r
household.income = as.character(nda17$demo_comb_income_v2)
household.income[nda17$demo_comb_income_v2 == "1"] = "[<50K]"
household.income[nda17$demo_comb_income_v2 == "2"] = "[<50K]"
household.income[nda17$demo_comb_income_v2 == "3"] = "[<50K]"
household.income[nda17$demo_comb_income_v2 == "4"] = "[<50K]"
household.income[nda17$demo_comb_income_v2 == "5"] = "[<50K]"
household.income[nda17$demo_comb_income_v2 == "6"] = "[<50K]"
household.income[nda17$demo_comb_income_v2 == "7"] = "[>=50K & <100K]"
household.income[nda17$demo_comb_income_v2 == "8"] = "[>=50K & <100K]"
household.income[nda17$demo_comb_income_v2 == "9"] = "[>=100K]"
household.income[nda17$demo_comb_income_v2 == "10"] = "[>=100K]"
household.income[nda17$demo_comb_income_v2 == "777"] = NA
household.income[nda17$demo_comb_income_v2 == "999"] = NA
household.income[household.income %in% c(NA, "999", "777")] = NA
nda17$household.income = factor(household.income)
```

### Highest level of parental education
This can be either the first (demo_prnt_ed_v2) or the second (demo_prtnr_ed_v2) parent.
```r
highest.education = rep("999", length(nda17$demo_prnt_ed_v2))
highest.education[nda17$demo_prnt_ed_v2 == "0"] = 1
highest.education[nda17$demo_prnt_ed_v2 == "1"] = 4
highest.education[nda17$demo_prnt_ed_v2 == "2"] = 5
highest.education[nda17$demo_prnt_ed_v2 == "3"] = 6
highest.education[nda17$demo_prnt_ed_v2 == "4"] = 7
highest.education[nda17$demo_prnt_ed_v2 == "5"] = 8
highest.education[nda17$demo_prnt_ed_v2 == "6"] = 9
highest.education[nda17$demo_prnt_ed_v2 == "7"] = 10
highest.education[nda17$demo_prnt_ed_v2 == "8"] = 11
highest.education[nda17$demo_prnt_ed_v2 == "9"] = 12
highest.education[nda17$demo_prnt_ed_v2 == "10"] = 13
highest.education[nda17$demo_prnt_ed_v2 == "11"] = 14
highest.education[(nda17$demo_prnt_ed_v2 == "12") | (nda17$demo_prnt_ed_v2 == "13")] = 16
highest.education[nda17$demo_prnt_ed_v2 == "14"] = 17
highest.education[nda17$demo_prnt_ed_v2 == "15"] = 18
highest.education[(nda17$demo_prnt_ed_v2 == "16") | (nda17$demo_prnt_ed_v2 == "17")] = 20
highest.education[nda17$demo_prnt_ed_v2 == "18"] = 21
highest.education[nda17$demo_prnt_ed_v2 == "19"] = 22
highest.education[nda17$demo_prnt_ed_v2 == "20"] = 23
highest.education[nda17$demo_prnt_ed_v2 == "21"] = 24
highest.education[nda17$demo_prnt_ed_v2 == "777"] = 999
highest.education[highest.education == 999] = NA

highest.education2 = rep("999", length(nda17$demo_prtnr_ed_v2))
highest.education2[nda17$demo_prtnr_ed_v2 == "0"] = 1
highest.education2[nda17$demo_prtnr_ed_v2 == "1"] = 4
highest.education2[nda17$demo_prtnr_ed_v2 == "2"] = 5
highest.education2[nda17$demo_prtnr_ed_v2 == "3"] = 6
highest.education2[nda17$demo_prtnr_ed_v2 == "4"] = 7
highest.education2[nda17$demo_prtnr_ed_v2 == "5"] = 8
highest.education2[nda17$demo_prtnr_ed_v2 == "6"] = 9
highest.education2[nda17$demo_prtnr_ed_v2 == "7"] = 10
highest.education2[nda17$demo_prtnr_ed_v2 == "8"] = 11
highest.education2[nda17$demo_prtnr_ed_v2 == "9"] = 12
highest.education2[nda17$demo_prtnr_ed_v2 == "10"] = 13
highest.education2[nda17$demo_prtnr_ed_v2 == "11"] = 14
highest.education2[(nda17$demo_prtnr_ed_v2 == "12") | (nda17$demo_prtnr_ed_v2 == "13")] = 16
highest.education2[nda17$demo_prtnr_ed_v2 == "14"] = 17
highest.education2[nda17$demo_prtnr_ed_v2 == "15"] = 18
highest.education2[(nda17$demo_prtnr_ed_v2 == "16") | (nda17$demo_prtnr_ed_v2 == "17")] = 20
highest.education2[nda17$demo_prtnr_ed_v2 == "18"] = 21
highest.education2[nda17$demo_prtnr_ed_v2 == "19"] = 22
highest.education2[nda17$demo_prtnr_ed_v2 == "20"] = 23
highest.education2[nda17$demo_prtnr_ed_v2 == "21"] = 24
highest.education2[nda17$demo_prtnr_ed_v2 == "777"] = 999
highest.education2[highest.education2 == 999] = NA
nda17$highest.education = factor( as.character(pmax(as.numeric(highest.education), as.numeric(highest.education2),na.rm=T)) )
```

Here a simplified version of the highest education that results in only 5 different levels. These levels correspond to the numbers published by the American Community Survey (ACS). 
```r
high.educ1 = nda17$demo_prnt_ed_v2
high.educ2 = nda17$demo_prtnr_ed_v2
high.educ1[which(high.educ1 == "999")] = NA
high.educ2[which(high.educ2 == "999")] = NA
high.educ1[which(high.educ1 == "777")] = NA
high.educ2[which(high.educ2 == "777")] = NA
high.educ = pmax(as.numeric(as.character(high.educ1)), as.numeric(as.character(high.educ2)), na.rm=T)
idx <- which(high.educ %in% 0:12, arr.ind = TRUE)
high.educ[idx] = "< HS Diploma"
idx <- which(high.educ %in% 13:14, arr.ind = TRUE)
high.educ[idx] = "HS Diploma/GED"
idx <- which(high.educ %in% 15:17, arr.ind = TRUE)
high.educ[idx] = "Some College"
idx <- which(high.educ == 18, arr.ind = TRUE)
high.educ[idx] = "Bachelor"
idx <- which(high.educ %in% 19:21, arr.ind = TRUE)
high.educ[idx] = "Post Graduate Degree"
high.educ[which(high.educ == "999")]=NA
high.educ[which(high.educ == "777")]=NA
nda17$high.educ <- factor(high.educ)
```

### Marrital status

```r
married = rep("0", length(nda17$demo_prnt_marital_v2))
married[as.numeric(nda17$demo_prnt_marital_v2) == 1] = 1
married[as.numeric(nda17$demo_prnt_marital_v2) == 7] = NA
nda17$married = factor( married, levels= 0:1, labels = c("no", "yes") )
```
Add another variable that also includes couples that just live together. 
```r
married.livingtogether = rep("0", length(nda17$demo_prnt_marital_v2))
married.livingtogether[as.numeric(nda17$demo_prnt_marital_v2) == 1 | as.numeric(nda17$demo_prnt_marital_v2) == 6] = 1
married.livingtogether[as.numeric(nda17$demo_prnt_marital_v2) == 7] = NA
nda17$married.livingtogether = factor( married.livingtogether, levels= 0:1, labels = c("no", "yes") )
```


### Body-Mass index

```r
nda17$anthro_bmi_calc = as.numeric(as.character(nda17$anthro_weight_calc)) / as.numeric(as.character(nda17$anthro_height_calc))^2 * 703
```

### A simplified race.ethnicity value

ABCD is using a simplified 5 category race/ethnicity scale for reporting purposes and for comparison of the ABCD cohort to data from the American Community Census. The following code will add a new 'race_ethnicity' column to the NDA-17 data frame that implement this scale.

```r
nda17$demo_race_white= (nda17$demo_race_a_p___10 == 1)*1
nda17$demo_race_black= (nda17$demo_race_a_p___11 == 1)*1
nda17$demo_race_asian = 0
nda17$demo_race_asian[nda17$demo_race_a_p___18 == 1 | nda17$demo_race_a_p___19 == 1 | 
			nda17$demo_race_a_p___20 == 1 | nda17$demo_race_a_p___21 == 1 | 
			nda17$demo_race_a_p___22 == 1 | nda17$demo_race_a_p___23 == 1 |
		    nda17$demo_race_a_p___24==1] = 1
nda17$demo_race_aian = 0
nda17$demo_race_aian[nda17$demo_race_a_p___12 == 1 | nda17$demo_race_a_p___13 == 1] = 1
nda17$demo_race_nhpi = 0
nda17$demo_race_nhpi[nda17$demo_race_a_p___14 == 1 | nda17$demo_race_a_p___15 == 1 | 
				nda17$demo_race_a_p___16 == 1 | nda17$demo_race_a_p___17 == 1] = 1
nda17$demo_race_other = 0
nda17$demo_race_other[nda17$demo_race_a_p___25 == 1] = 1
nda17$demo_race_mixed = nda17$demo_race_white + nda17$demo_race_black + nda17$demo_race_asian + 
					nda17$demo_race_aian + nda17$demo_race_nhpi + nda17$demo_race_other

nda17$demo_race_mixed[ nda17$demo_race_mixed <= 1] =  0
nda17$demo_race_mixed[ nda17$demo_race_mixed > 1] =  1

nda17$race.eth = NA
nda17$race.eth[ nda17$demo_race_white == 1] = 2
nda17$race.eth[ nda17$demo_race_black == 1] = 3
nda17$race.eth[ nda17$demo_race_asian == 1] = 4
nda17$race.eth[ nda17$demo_race_aian == 1]  = 5
nda17$race.eth[ nda17$demo_race_nhpi == 1]  = 6
nda17$race.eth[ nda17$demo_race_other == 1] = 7
nda17$race.eth[ nda17$demo_race_mixed == 1] = 8

nda17$race.eth[nda17$demo_ethn_v2 == "Yes"] = 1   

nda17$race.eth <- factor(nda17$race.eth,
                       levels = 1:8,
                       labels = c("Hispanic", "White", "Black", "Asian", "AIAN", "NHPI", "Other", "Mixed") ) 
```
The above race.eth value has more categories compared to what has been used recently in ABCD. Here is the reduced definition of race/ethnicity used most frequently
```r
nda17$race.ethnicity = nda17$race.eth
nda17$race.ethnicity[!(nda17$race.eth=="White" | nda17$race.eth=="Black" |
					nda17$race.eth=="Asian" | nda17$race.eth=="Hispanic")] = "Other"
nda17$race.ethnicity = droplevels(nda17$race.ethnicity)
```

It is worthwhile to point out here that the above category for hispanic is calculated in ABCD differently from the other race categories. In particular any ethnicity selection of hispanic will map the participant into the hispanic category regardless of the selection of one or more race categories.

Save the new data frame again.
```r
saveRDS(nda17, "nda17.Rds")
```
