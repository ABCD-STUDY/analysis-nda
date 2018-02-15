## Definition of convenience variables

The following sections extend the nda17 data frame by some core demographic columns.
 - subjectid
 - age in years
 - female
 - race.ethnicity
 - highest.household.income
 - highest.education
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

### Highest household income

```r
highest.household.income = as.character(nda17$demo_comb_income_v2)
highest.household.income[nda17$demo_comb_income_v2 %in% c(1:6)] = "[<50K]"
highest.household.income[nda17$demo_comb_income_v2 %in% c(7,8)] = "[>=50K & <100K]"
highest.household.income[nda17$demo_comb_income_v2 %in% c(9:10)] = "[>=100K]"
highest.household.income[nda17$demo_comb_income_v2 %in% c(11:12)] = NA
highest.household.income[nda17$demo_comb_income_v2 %in% c(NA, "999", "777")] = ""
nda17$highest.household.income = factor(highest.household.income)
```

### Highest level of parent education

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
highest.education[nda17$demo_prnt_ed_v2 == "77"] = 999

highest.education = as.character(highest.education)
highest.education[highest.education == 999] = ""
nda17$highest.education = factor(highest.education)
```

### Marrital status

```r
married = rep("0", length(nda17$demo_prnt_marital_v2))
married[as.numeric(nda17$demo_prnt_marital_v2) == 1] = 1
nda17$married = factor( married, levels= 0:1, labels = c("no", "yes") )
```

### A simplified race.ethnicity value

ABCD is using a simplified 5 category race/ethnicity scale for reporting purposes and for comparison of the ABCD cohort to data from the American Community Census. The following code will add a new 'race_ethnicity' column to the NDA-17 data frame that implement this scale.

```r
isWhite = rep("999", length(nda17$demo_ethn_v2))
col = nda17$demo_ethn_v2
idx = which( (nda17$demo_ethn_v2 == "<span lang=\"en\">Yes</span><span lang=\"es\" style= \"color:maroon\">Sí</span>") | (nda17$demo_ethn_v2 == "<span lang=\"en\">No</span><span lang=\"es\" style= \"color:maroon\">No</span>"))
isWhite[idx] = col[idx]
isWhite[which(col != "<span lang=\"en\">Yes</span><span lang=\"es\" style= \"color:maroon\">Sí</span>" & nda17$demo_race_a_p___10 == "1")] = TRUE
isWhite[-which(col != "<span lang=\"en\">Yes</span><span lang=\"es\" style= \"color:maroon\">Sí</span>" & nda17$demo_race_a_p___10 == "1")] = FALSE

isBlack = rep("999", length(nda17$demo_ethn_v2))
col = nda17$demo_ethn_v2
idx = which( (nda17$demo_ethn_v2 == "<span lang=\"en\">Yes</span><span lang=\"es\" style= \"color:maroon\">Sí</span>") | (nda17$demo_ethn_v2 == "<span lang=\"en\">No</span><span lang=\"es\" style= \"color:maroon\">No</span>"))
isBlack[idx] = col[idx]
isBlack[which(isBlack != "<span lang=\"en\">Yes</span><span lang=\"es\" style= \"color:maroon\">Sí</span>" & nda17$demo_race_a_p___11 == "1")] = TRUE
isBlack[-which(isBlack != "<span lang=\"en\">Yes</span><span lang=\"es\" style= \"color:maroon\">Sí</span>" & nda17$demo_race_a_p___11 == "1")] = FALSE

isHispa = rep("999", length(nda17$demo_ethn_v2))
col = nda17$demo_ethn_v2
idx = which((col == "<span lang=\"en\">Yes</span><span lang=\"es\" style= \"color:maroon\">Sí</span>") | (col == "<span lang=\"en\">No</span><span lang=\"es\" style= \"color:maroon\">No</span>"))
isHispa[idx] = col[idx]
isHispa[which(col == "<span lang=\"en\">Yes</span><span lang=\"es\" style= \"color:maroon\">Sí</span>")] = TRUE
isHispa[-which(col == "<span lang=\"en\">Yes</span><span lang=\"es\" style= \"color:maroon\">Sí</span>")] = FALSE

isAsian = rep("999", length(nda17$demo_ethn_v2))
col = nda17$demo_ethn_v2
idx = which((col == "<span lang=\"en\">Yes</span><span lang=\"es\" style= \"color:maroon\">Sí</span>") | (col == "<span lang=\"en\">No</span><span lang=\"es\" style= \"color:maroon\">No</span>"))
isAsian[idx] = col[idx]
idx = which((nda17$demo_race_a_p___18 == "1") | (nda17$demo_race_a_p___19 == "1") | (nda17$demo_race_a_p___20 == "1") | (nda17$demo_race_a_p___21 == "1") | (nda17$demo_race_a_p___22 == "1") | (nda17$demo_race_a_p___23 == "1") | (nda17$demo_race_a_p___24 == "1"))
isAsian[idx] = TRUE
isAsian[-idx] = FALSE

isOther = rep("999", length(nda17$demo_ethn_v2))
col = nda17$demo_ethn_v2
idx = which((col == "<span lang=\"en\">Yes</span><span lang=\"es\" style= \"color:maroon\">Sí</span>") | (col == "<span lang=\"en\">No</span><span lang=\"es\" style= \"color:maroon\">No</span>"))
isOther[idx] = col[idx]
idx = which((nda17$demo_race_a_p___12 == "1") | (nda17$demo_race_a_p___13 == "1") | (nda17$demo_race_a_p___14 == "1") | (nda17$demo_race_a_p___15 == "1") | (nda17$demo_race_a_p___16 == "1") | (nda17$demo_race_a_p___17 == "1") | (nda17$demo_race_a_p___25 == "1") | (nda17$demo_race_a_p___77 == "1") | (nda17$demo_race_a_p___99 == "1"))
isOther[idx] = TRUE
isOther[-idx] = FALSE

race.ethnicity = rep("", length(nda17$demo_ethn_v2))
for( i in 1:length(race.ethnicity)) {
   if ( as.numeric(isWhite[i]==TRUE) + 
        as.numeric(isBlack[i]==TRUE) + 
        as.numeric(isHispa[i]==TRUE) + 
        as.numeric(isAsian[i]==TRUE) + 
        as.numeric(isOther[i]==TRUE) >= 2) {

      ethnicity_recode = "999"
      if ( (as.numeric(nda17$demo_ethn_v2[i]) == "1") || (as.numeric(nda17$demo_ethn_v2[i]) == "2") ) {
         ethnicity_recode = as.numeric(nda17$demo_ethn_v2[i])
      }
      if (ethnicity_recode == "1") {
         race.ethnicity[i] = 3
      } else {
         race.ethnicity[i] = 5
      }
      next
   }
   if (isWhite[i]) {
      race.ethnicity[i] = 1
   }
   if (isBlack[i]) {
      race.ethnicity[i] = 2
   }
   if (isHispa[i]) {
      race.ethnicity[i] = 3
   }
   if (isAsian[i]) {
      race.ethnicity[i] = 4
   }
   if (isOther[i]) {
      race.ethnicity[i] = 5
   }
   # twin sites report asian as other
   twinRecruitmentSites = c("site14", "site02", "site20", "site19")
   if ( (nda17$abcd_site[i] %in% twinRecruitmentSites) && (nda17$rel_relationship %in% c("twin", "sibling")) ) {
      # report other instead
      if (race.ethnicity[i] == 4) race.ethnicity = 5
   }
}
nda17$race.ethnicity = factor(race.ethnicity, levels = 1:5, labels=c("White","Black","Hispanic","Asian","Other"))
```

Save the new data frame again.
```r
saveRDS(nda17, "nda17.Rds")
```
