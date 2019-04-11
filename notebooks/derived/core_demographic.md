## Definition of convenience variables

The following sections extend the nda18 data frame (see [creating a single data spreadsheet](https://github.com/ABCD-STUDY/analysis-nda17#create-a-single-data-spreadsheet) and [create categorical variables](https://github.com/ABCD-STUDY/analysis-nda/blob/master/notebooks/general/categorical_extension.md)) by some core demographic columns.
 - subjectid
 - age in years
 - female
 - race.ethnicity
 - household.income
 - high.educ
 - married
 - abcd_site

Most of these are simple re-definitions of existing columns with simplier names, other columns are re-scored versions of nda17 columns.

Start by reading in the merged data from disk.
```r
nda18 = readRDS("nda18_orig.Rds")
```

Now extend nda18 by the new columns.

### Site name
The site name is anonymized and stored per event in case participants move from one site to another during the study.

```r
nda18$abcd_site = nda18$site_id_l
```

### Subjectid

```r
nda18$subjectid = nda18$src_subject_id
```

### Age (in month)
Get a better name for interview_age.
```r
nda18$age = nda18$interview_age
```

### Female

```r
nda18$female = factor(as.numeric(nda18$gender == "F"), levels = 0:1, labels = c("no", "yes") ) 
```

On NDA the "gender" variable is listed as containing "Sex at birth". Lets create a copy called "sex".
```
nda18$sex = nda18$gender
nda18$sex[which(nda18$sex=="")]=NA
nda18$sex=factor( nda18$sex, levels= c("F","M"))
```


### Household income

The demo_comb_income_v2 variable has been renamed in the merge script to demo_comb_income_v2b (the ABCD name listed in NDA_DEAP_names_1.1.csv).

```r
household.income = nda18$demo_comb_income_v2b
household.income[nda18$demo_comb_income_v2b == "1"] = 1 # "[<50K]"
household.income[nda18$demo_comb_income_v2b == "2"] = 1 # "[<50K]"
household.income[nda18$demo_comb_income_v2b == "3"] = 1 # "[<50K]"
household.income[nda18$demo_comb_income_v2b == "4"] = 1 # "[<50K]"
household.income[nda18$demo_comb_income_v2b == "5"] = 1 # "[<50K]"
household.income[nda18$demo_comb_income_v2b == "6"] = 1 # "[<50K]"
household.income[nda18$demo_comb_income_v2b == "7"] = 2 # "[>=50K & <100K]"
household.income[nda18$demo_comb_income_v2b == "8"] = 2 # "[>=50K & <100K]"
household.income[nda18$demo_comb_income_v2b == "9"] = 3 # "[>=100K]"
household.income[nda18$demo_comb_income_v2b == "10"] = 3 # "[>=100K]"
household.income[nda18$demo_comb_income_v2b == "777"] = NA
household.income[nda18$demo_comb_income_v2b == "999"] = NA
household.income[household.income %in% c(NA, "999", "777")] = NA
nda18$household.income = factor( household.income, levels= 1:3, labels = c("[<50K]", "[>=50K & <100K]", "[>=100K]") )
```

### Highest level of parental education
This can be either the first (demo_prnt_ed_v2b) or the second (demo_prtnr_ed_v2b) parent.
```r
highest.education = rep("999", length(nda18$demo_prnt_ed_v2b))
highest.education[nda18$demo_prnt_ed_v2b == "0"] = 1
highest.education[nda18$demo_prnt_ed_v2b == "1"] = 4
highest.education[nda18$demo_prnt_ed_v2b == "2"] = 5
highest.education[nda18$demo_prnt_ed_v2b == "3"] = 6
highest.education[nda18$demo_prnt_ed_v2b == "4"] = 7
highest.education[nda18$demo_prnt_ed_v2b == "5"] = 8
highest.education[nda18$demo_prnt_ed_v2b == "6"] = 9
highest.education[nda18$demo_prnt_ed_v2b == "7"] = 10
highest.education[nda18$demo_prnt_ed_v2b == "8"] = 11
highest.education[nda18$demo_prnt_ed_v2b == "9"] = 12
highest.education[nda18$demo_prnt_ed_v2b == "10"] = 13
highest.education[nda18$demo_prnt_ed_v2b == "11"] = 14
highest.education[(nda18$demo_prnt_ed_v2b == "12") | (nda18$demo_prnt_ed_v2b == "13")] = 16
highest.education[nda18$demo_prnt_ed_v2b == "14"] = 17
highest.education[nda18$demo_prnt_ed_v2b == "15"] = 18
highest.education[(nda18$demo_prnt_ed_v2b == "16") | (nda18$demo_prnt_ed_v2b == "17")] = 20
highest.education[nda18$demo_prnt_ed_v2b == "18"] = 21
highest.education[nda18$demo_prnt_ed_v2b == "19"] = 22
highest.education[nda18$demo_prnt_ed_v2b == "20"] = 23
highest.education[nda18$demo_prnt_ed_v2b == "21"] = 24
highest.education[nda18$demo_prnt_ed_v2b == "777"] = 999
highest.education[highest.education == 999] = NA

highest.education2 = rep("999", length(nda18$demo_prtnr_ed_v2b))
highest.education2[nda18$demo_prtnr_ed_v2b == "0"] = 1
highest.education2[nda18$demo_prtnr_ed_v2b == "1"] = 4
highest.education2[nda18$demo_prtnr_ed_v2b == "2"] = 5
highest.education2[nda18$demo_prtnr_ed_v2b == "3"] = 6
highest.education2[nda18$demo_prtnr_ed_v2b == "4"] = 7
highest.education2[nda18$demo_prtnr_ed_v2b == "5"] = 8
highest.education2[nda18$demo_prtnr_ed_v2b == "6"] = 9
highest.education2[nda18$demo_prtnr_ed_v2b == "7"] = 10
highest.education2[nda18$demo_prtnr_ed_v2b == "8"] = 11
highest.education2[nda18$demo_prtnr_ed_v2b == "9"] = 12
highest.education2[nda18$demo_prtnr_ed_v2b == "10"] = 13
highest.education2[nda18$demo_prtnr_ed_v2b == "11"] = 14
highest.education2[(nda18$demo_prtnr_ed_v2b == "12") | (nda18$demo_prtnr_ed_v2b == "13")] = 16
highest.education2[nda18$demo_prtnr_ed_v2b == "14"] = 17
highest.education2[nda18$demo_prtnr_ed_v2b == "15"] = 18
highest.education2[(nda18$demo_prtnr_ed_v2b == "16") | (nda18$demo_prtnr_ed_v2b == "17")] = 20
highest.education2[nda18$demo_prtnr_ed_v2b == "18"] = 21
highest.education2[nda18$demo_prtnr_ed_v2b == "19"] = 22
highest.education2[nda18$demo_prtnr_ed_v2b == "20"] = 23
highest.education2[nda18$demo_prtnr_ed_v2b == "21"] = 24
highest.education2[nda18$demo_prtnr_ed_v2b == "777"] = 999
highest.education2[highest.education2 == 999] = NA
nda18$highest.education = factor( as.character(pmax(as.numeric(highest.education), as.numeric(highest.education2),na.rm=T)), levels=c(9,10,11,12,13,14,15,16,17,18,20,21,22,23,24), labels=c("9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "20", "21", "22", "23", "24") )
```

Here a simplified version of the highest education that results in only 5 different levels. These levels correspond to the numbers published by the American Community Survey (ACS). 
```r
high.educ1 = nda18$demo_prnt_ed_v2b
high.educ2 = nda18$demo_prtnr_ed_v2b
high.educ1[which(high.educ1 == "999")] = NA
high.educ2[which(high.educ2 == "999")] = NA
high.educ1[which(high.educ1 == "777")] = NA
high.educ2[which(high.educ2 == "777")] = NA
high.educ = pmax(as.numeric(as.character(high.educ1)), as.numeric(as.character(high.educ2)), na.rm=T)
idx <- which(high.educ %in% 0:12, arr.ind = TRUE)
high.educ[idx] = 1 # "< HS Diploma"
idx <- which(high.educ %in% 13:14, arr.ind = TRUE)
high.educ[idx] = 2 # "HS Diploma/GED"
idx <- which(high.educ %in% 15:17, arr.ind = TRUE)
high.educ[idx] = 3 # "Some College"
idx <- which(high.educ == 18, arr.ind = TRUE)
high.educ[idx] = 4 # "Bachelor"
idx <- which(high.educ %in% 19:21, arr.ind = TRUE)
high.educ[idx] = 5 # "Post Graduate Degree"
high.educ[which(high.educ == "999")]=NA
high.educ[which(high.educ == "777")]=NA
nda18$high.educ = factor( high.educ, levels= 1:5, labels = c("< HS Diploma","HS Diploma/GED","Some College","Bachelor","Post Graduate Degree") )
```

### Marrital status

The demo_prnt_marital_v2 variable has also been renamed in NDA_DEAP_names_1.1.csv to demo_prnt_marital_v2b.

```r
married = rep(NA, length(nda18$demo_prnt_marital_v2b))
married[nda18$demo_prnt_marital_v2b == 1] = 1
married[nda18$demo_prnt_marital_v2b %in% 2:6] = 0
nda18$married = factor( married, levels= 0:1, labels = c("no", "yes") )
```
Add another variable that also includes couples that just live together. 
```r
married.livingtogether = rep(NA, length(nda18$demo_prnt_marital_v2b))
married.livingtogether[nda18$demo_prnt_marital_v2b %in% c(1,6)] = 1
married.livingtogether[nda18$demo_prnt_marital_v2b %in% 2:5] = 0
nda18$married.or.livingtogether = factor( married.livingtogether, levels= 0:1, labels = c("no", "yes") )
```


### Body-Mass index
There could be a problem here with NDA_DEAP_names_1.1.csv because anthroweightcalc has not been renamed to anthro_weight_calc - probably because the name of the instrument "abcd_ant01" is not the correct name for this item (needs to be checked).

```r
nda18$anthro_bmi_calc = as.numeric(as.character(nda18$anthro_weight_calc)) / as.numeric(as.character(nda18$anthro_height_calc))^2 * 703
nda18$anthro_bmi_calc[which(nda18$anthro_bmi_calc>100)]=NA
```

### A simplified race.ethnicity value

ABCD is using a simplified 5 category race/ethnicity scale for reporting purposes and for comparison of the ABCD cohort to data from the American Community Census. The following code will add a new 'race_ethnicity' column to the NDA-17 data frame that implement this scale.

```r
nda18$demo_race_white= (nda18$demo_race_a_p___10 == 1)*1
nda18$demo_race_black= (nda18$demo_race_a_p___11 == 1)*1
nda18$demo_race_asian = 0
nda18$demo_race_asian[nda18$demo_race_a_p___18 == 1 | nda18$demo_race_a_p___19 == 1 | 
			nda18$demo_race_a_p___20 == 1 | nda18$demo_race_a_p___21 == 1 | 
			nda18$demo_race_a_p___22 == 1 | nda18$demo_race_a_p___23 == 1 |
		    nda18$demo_race_a_p___24==1] = 1
nda18$demo_race_aian = 0
nda18$demo_race_aian[nda18$demo_race_a_p___12 == 1 | nda18$demo_race_a_p___13 == 1] = 1
nda18$demo_race_nhpi = 0
nda18$demo_race_nhpi[nda18$demo_race_a_p___14 == 1 | nda18$demo_race_a_p___15 == 1 | 
				nda18$demo_race_a_p___16 == 1 | nda18$demo_race_a_p___17 == 1] = 1
nda18$demo_race_other = 0
nda18$demo_race_other[nda18$demo_race_a_p___25 == 1] = 1
nda18$demo_race_mixed = nda18$demo_race_white + nda18$demo_race_black + nda18$demo_race_asian + 
					nda18$demo_race_aian + nda18$demo_race_nhpi + nda18$demo_race_other

nda18$demo_race_mixed[ nda18$demo_race_mixed <= 1] =  0
nda18$demo_race_mixed[ nda18$demo_race_mixed > 1] =  1

nda18$race.eth.8level = NA
nda18$race.eth.8level[ nda18$demo_race_white == 1] = 2
nda18$race.eth.8level[ nda18$demo_race_black == 1] = 3
nda18$race.eth.8level[ nda18$demo_race_asian == 1] = 4
nda18$race.eth.8level[ nda18$demo_race_aian == 1]  = 5
nda18$race.eth.8level[ nda18$demo_race_nhpi == 1]  = 6
nda18$race.eth.8level[ nda18$demo_race_other == 1] = 7
nda18$race.eth.8level[ nda18$demo_race_mixed == 1] = 8

nda18$race.eth.8level[nda18$demo_ethn_p == 1] = 1


nda18$demo_race_hispanic=NA;
nda18$demo_race_hispanic[nda18$demo_ethn_p == 1] =1
nda18$demo_race_hispanic[nda18$demo_ethn_p == 2] =0

nda18$race.eth.8level <- factor(nda18$race.eth.8level,
                       levels = c(2,1,3,4,5,6,7,8),
                       labels = c("White", "Hispanic", "Black", "Asian", "AIAN", "NHPI", "Other", "Mixed") ) 
```
The above race.eth value has more categories compared to what has been used recently in ABCD. Here is the reduced definition of race/ethnicity used most frequently
```r
nda18$race.ethnicity.5level = nda18$race.eth.8level
nda18$race.ethnicity.5level[!(nda18$race.eth.8level=="White" | nda18$race.eth.8level=="Black" |
					nda18$race.eth.8level=="Asian" | nda18$race.eth.8level=="Hispanic")] = "Other"
nda18$race.ethnicity.5level = droplevels(nda18$race.ethnicity.5level)
```

It is worthwhile to point out here that the above category for hispanic is calculated in ABCD differently from the other race categories. In particular any ethnicity selection of hispanic will map the participant into the hispanic category regardless of the selection of one or more race categories.


### Baseline observation carry forward

For statistical modeling purpose, some variables collected at baseline, their values need to be carried forward to the follow-up visits.

These variables are assumed to be invariant:
```r
constant.vars=c("race_ethnicity","race.ethnicity.5level","race.eth.8level","rel_relationship", "rel_family_id","rel_same_sex") 
```
```r
table(nda18$sex,by=nda18$eventname)
for(i in 1:length(constant.vars)){
  print(table(nda18[,constant.vars[i]],by=nda18$eventname,dnn=constant.vars[i]))
}

bl.demo=nda18[which(nda18$eventname=="baseline_year_1_arm_1"),c("subjectid",constant.vars)]

nda18=nda18[,-which(colnames(nda18)%in%constant.vars)]
nda18=merge(nda18,bl.demo,by=c("subjectid"))
```

These can be changed overtime, but need baseline values filled in at follow up visits

```r
bl.vars=c("married.or.livingtogether","married","high.educ","household.income") 
bl.demo=nda18[which(nda18$eventname=="baseline_year_1_arm_1"),c("subjectid",bl.vars)]
colnames(bl.demo)[-1]=paste0(bl.vars,".bl") #rename these variables to baseline variables

nda18=merge(nda18,bl.demo,by=c("subjectid"))
```

Save the new data frame again.
```r
saveRDS(nda18, "nda18.Rds")
```
