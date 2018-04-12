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

### Household income

```r
household.income = as.character(nda17$demo_comb_income_v2)
household.income[nda17$demo_comb_income_v2 == "##en##Less than $5,000##/en####es##Menos de $5,000##/es##"] = "[<50K]"
household.income[nda17$demo_comb_income_v2 == "##en##$5,000 through $11,999##/en####es##De $5,000 a $11,999##/es##"] = "[<50K]"
household.income[nda17$demo_comb_income_v2 == "##en##$12,000 through $15,999##/en####es##De $12,000 a $15,999##/es##"] = "[<50K]"
household.income[nda17$demo_comb_income_v2 == "##en##$16,000 through $24,999##/en####es##De $16,000 a $24,999##/es##"] = "[<50K]"
household.income[nda17$demo_comb_income_v2 == "##en##$25,000 through $34,999##/en####es##De $25,000 a $34,999##/es##"] = "[<50K]"
household.income[nda17$demo_comb_income_v2 == "##en##$35,000 through $49,999##/en####es##De $35,000 a $49,999##/es##"] = "[<50K]"
household.income[nda17$demo_comb_income_v2 == "##en##$50,000 through $74,999##/en####es##De $50,000 a $74,999##/es##"] = "[>=50K & <100K]"
household.income[nda17$demo_comb_income_v2 == "##en##$75,000 through $99,999##/en####es##De $75,000 a $99,999##/es##"] = "[>=50K & <100K]"
household.income[nda17$demo_comb_income_v2 == "##en##$100,000 through $199,999##/en####es##De $100,000 a $199,999##/es##"] = "[>=100K]"
household.income[nda17$demo_comb_income_v2 == "##en##$200,000 and greater##/en####es##$200,000 o más##/es##"] = "[>=100K]"
household.income[nda17$demo_comb_income_v2 == "##en##Refuse to answer##/en####es##No deseo responder##/es##"] = NA
household.income[nda17$demo_comb_income_v2 == "##en##Don't know##/en####es##No lo sé##/es##"] = NA
household.income[household.income %in% c(NA, "999", "777")] = NA
nda17$household.income = factor(household.income)
```

### Highest level of parental education

```r
highest.education = rep("999", length(nda17$demo_prnt_ed_v2))
m = list()
m[1] = "##en##Never attended/Kindergarten only##/en####es##Nunca asistí/Kinder solamente##/es##"
m[2] = "##en##1st grade##/en####es##1.er grado##/es##"
m[3] = "##en##2nd grade##/en####es##2.º grado##/es##"
m[4] = "##en##3rd grade##/en####es##3.er grado##/es##"
m[5] = "##en##4th grade##/en####es##4.º grado##/es##"
m[6] = "##en##5th grade##/en####es##5.º grado##/es##"
m[7] = "##en##6th grade##/en####es##6.º grado##/es##"
m[8] = "##en##7th grade##/en####es##7.º grado##/es##"
m[9] = "##en##8th grade##/en####es##8.º grado##/es##"
m[10] = "##en##9th grade##/en####es##9.º grado##/es##"
m[11] = "##en##10th grade##/en####es##10.º grado##/es##"
m[12] = "##en##11th grade##/en####es##11.º grado##/es##"
m[13] = "##en##12th grade, no diploma##/en####es## 12.º grado, sin certificado/diploma##/es##"
m[14] = "##en##High school graduate##/en####es##Preparatoria terminada##/es##"
m[15] = "##en##GED or equivalent##/en####es##Diploma General de Equivalencia (GED) o equivalente##/es##"
m[16] = "##en##Some college, no degree##/en####es##Estudios universitarios parciales; sin título##/es##"
m[17] = "##en##Associate degree: Occupational, Technical, or Vocational##/en####es##Título de asociado: programa ocupacional, técnico o vocacional##/es##"
m[18] = "##en##Associate degree: Academic Program##/en####es##Título de asociado: programa académico##/es##"
m[19] = "##en##Bachelor's degree (ex. BA, AB, BS, BBS)##/en####es##Licenciatura (p. ej., BA, AB, BS, BBA)##/es##"
m[20] = "##en##Master's degree (ex. MA, MS, MEng, MEd, MBA)##/en####es##Maestría (p. ej., MA, MS, MEng, MEd, MBA)##/es##"
m[21] = "##en##Professional School degree (ex. MD, DDS, DVN, JD)##/en####es##Título de escuela profesional (p. ej., MD, DDS, DVM, JD)##/es##"
m[22] = "##en##Doctoral degree (ex. PhD, EdD)##/en####es##Doctorado (p. ej., PhD, EdD)##/es##"
m[77] = "##en##Refused to answer##/en####es##Prefiero no responder##/es##"
highest.education[nda17$demo_prnt_ed_v2 == m[1]] = 1
highest.education[nda17$demo_prnt_ed_v2 == m[2]] = 4
highest.education[nda17$demo_prnt_ed_v2 == m[3]] = 5
highest.education[nda17$demo_prnt_ed_v2 == m[4]] = 6
highest.education[nda17$demo_prnt_ed_v2 == m[5]] = 7
highest.education[nda17$demo_prnt_ed_v2 == m[6]] = 8
highest.education[nda17$demo_prnt_ed_v2 == m[7]] = 9
highest.education[nda17$demo_prnt_ed_v2 == m[8]] = 10
highest.education[nda17$demo_prnt_ed_v2 == m[9]] = 11
highest.education[nda17$demo_prnt_ed_v2 == m[10]] = 12
highest.education[nda17$demo_prnt_ed_v2 == m[11]] = 13
highest.education[nda17$demo_prnt_ed_v2 == m[12]] = 14
highest.education[(nda17$demo_prnt_ed_v2 == m[13]) | (nda17$demo_prnt_ed_v2 == m[14])] = 16
highest.education[nda17$demo_prnt_ed_v2 == m[15]] = 17
highest.education[nda17$demo_prnt_ed_v2 == m[16]] = 18
highest.education[(nda17$demo_prnt_ed_v2 == m[17]) | (nda17$demo_prnt_ed_v2 == m[18])] = 20
highest.education[nda17$demo_prnt_ed_v2 == m[19]] = 21
highest.education[nda17$demo_prnt_ed_v2 == m[20]] = 22
highest.education[nda17$demo_prnt_ed_v2 == m[21]] = 23
highest.education[nda17$demo_prnt_ed_v2 == m[22]] = 24
highest.education[nda17$demo_prnt_ed_v2 == m[77]] = 999
highest.education[highest.education == 999] = NA
nda17$highest.education = factor(highest.education)
```

Here a simplified version of the highest education that results in only 5 different levels. These levels correspond to the numbers published by the American Community Census (ACS). 
```r
high.educ = rep("999", length(nda17$demo_prnt_ed_v2))
idx <- which(nda17$demo_prnt_ed_v2 %in% m[1:13],arr.ind = TRUE)
high.educ[idx] = "< HS Diploma"
idx <- which(nda17$demo_prnt_ed_v2 %in% m[14:15], arr.ind = TRUE)
high.educ[idx] = "HS Diploma/GED"
idx <- which(nda17$demo_prnt_ed_v2 %in% m[16:18], arr.ind = TRUE)
high.educ[idx] = "Some College"
idx <- which(nda17$demo_prnt_ed_v2 == m[19], arr.ind = TRUE)
high.educ[idx] = "Bachelor"
idx <- which(nda17$demo_prnt_ed_v2 %in% m[20:22], arr.ind = TRUE)
high.educ[idx] = "Post Graduate Degree"
high.educ[which(high.educ == "999")]=NA
nda17$high.educ <- factor(high.educ)
```

### Marrital status

```r
married = rep("0", length(nda17$demo_prnt_marital_v2))
married[as.numeric(nda17$demo_prnt_marital_v2) == 1] = 1
nda17$married = factor( married, levels= 0:1, labels = c("no", "yes") )
```

### Body-Mass index

```r
nda17$anthro_bmi_calc = nda17$anthro_weight_calc / nda17$anthro_height_calc^2 * 703
```

### A simplified race.ethnicity value

ABCD is using a simplified 5 category race/ethnicity scale for reporting purposes and for comparison of the ABCD cohort to data from the American Community Census. The following code will add a new 'race_ethnicity' column to the NDA-17 data frame that implement this scale.

```r
isWhite = rep("999", length(nda17$demo_ethn_v2))
col = nda17$demo_ethn_v2
idx = which( (nda17$demo_ethn_v2 == "Yes") | (nda17$demo_ethn_v2 == "No"))
isWhite[idx] = col[idx]
isWhite[which(col != "Yes" & nda17$demo_race_a_p___10 == "1")] = TRUE
isWhite[-which(col != "Yes" & nda17$demo_race_a_p___10 == "1")] = FALSE

isBlack = rep("999", length(nda17$demo_ethn_v2))
col = nda17$demo_ethn_v2
idx = which( (nda17$demo_ethn_v2 == "Yes") | (nda17$demo_ethn_v2 == "No"))
isBlack[idx] = col[idx]
isBlack[which(isBlack != "Yes" & nda17$demo_race_a_p___11 == "1")] = TRUE
isBlack[-which(isBlack != "Yes" & nda17$demo_race_a_p___11 == "1")] = FALSE

isHispa = rep("999", length(nda17$demo_ethn_v2))
col = nda17$demo_ethn_v2
idx = which((col == "Yes") | (col == "No"))
isHispa[idx] = col[idx]
isHispa[which(col == "Yes")] = TRUE
isHispa[-which(col == "Yes")] = FALSE
```

It is worthwhile to point out here that the above category for hispanic is calculated in ABCD differently from the other race categories. In particular any ethnicity selection of hispanic will map the participant into the hispanic category regardless of the selection of one or more race categories.

```r
isAsian = rep("999", length(nda17$demo_ethn_v2))
col = nda17$demo_ethn_v2
idx = which((col == "Yes") | (col == "No"))
isAsian[idx] = col[idx]
idx = which((nda17$demo_race_a_p___18 == "1") | (nda17$demo_race_a_p___19 == "1") | (nda17$demo_race_a_p___20 == "1") | (nda17$demo_race_a_p___21 == "1") | (nda17$demo_race_a_p___22 == "1") | (nda17$demo_race_a_p___23 == "1") | (nda17$demo_race_a_p___24 == "1"))
isAsian[idx] = TRUE
isAsian[-idx] = FALSE

isOther = rep("999", length(nda17$demo_ethn_v2))
col = nda17$demo_ethn_v2
idx = which((col == "Yes") | (col == "No"))
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
      if ( (nda17$demo_ethn_v2[i] == "Yes") || (nda17$demo_ethn_v2[i] == "No") ) {
         ethnicity_recode = nda17$demo_ethn_v2[i]
      }
      # even if you are mixed race or other, selecting hispanic ethnicity assigns the participant to the hispanic category
      if (ethnicity_recode == "Yes") {
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
