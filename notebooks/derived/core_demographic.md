## Definition of convenience variables

The following sections extend the nda17 data frame (see [creating a single data spreadsheet](https://github.com/ABCD-STUDY/analysis-nda17#create-a-single-data-spreadsheet)) by some core demographic columns.
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
highest.household.income[nda17$demo_comb_income_v2 == "##en##Less than $5,000##/en####es##Menos de $5,000##/es##"] = "[<50K]"
highest.household.income[nda17$demo_comb_income_v2 == "##en##$5,000 through $11,999##/en####es##De $5,000 a $11,999##/es##"] = "[<50K]"
highest.household.income[nda17$demo_comb_income_v2 == "##en##$12,000 through $15,999##/en####es##De $12,000 a $15,999##/es##"] = "[<50K]"
highest.household.income[nda17$demo_comb_income_v2 == "##en##$16,000 through $24,999##/en####es##De $16,000 a $24,999##/es##"] = "[<50K]"
highest.household.income[nda17$demo_comb_income_v2 == "##en##$25,000 through $34,999##/en####es##De $25,000 a $34,999##/es##"] = "[<50K]"
highest.household.income[nda17$demo_comb_income_v2 == "##en##$35,000 through $49,999##/en####es##De $35,000 a $49,999##/es##"] = "[<50K]"
highest.household.income[nda17$demo_comb_income_v2 == "##en##$50,000 through $74,999##/en####es##De $50,000 a $74,999##/es##"] = "[>=50K & <100K]"
highest.household.income[nda17$demo_comb_income_v2 == "##en##$75,000 through $99,999##/en####es##De $75,000 a $99,999##/es##"] = "[>=50K & <100K]"
highest.household.income[nda17$demo_comb_income_v2 == "##en##$100,000 through $199,999##/en####es##De $100,000 a $199,999##/es##"] = "[>=100K]"
highest.household.income[nda17$demo_comb_income_v2 == "##en##$200,000 and greater##/en####es##$200,000 o más##/es##"] = "[>=100K]"
highest.household.income[nda17$demo_comb_income_v2 == "##en##Refuse to answer##/en####es##No deseo responder##/es##"] = NA
highest.household.income[nda17$demo_comb_income_v2 == "##en##Don't know##/en####es##No lo sé##/es##"] = NA
highest.household.income[highest.household.income %in% c(NA, "999", "777")] = NA
nda17$highest.household.income = factor(highest.household.income)
```

### Highest level of parent education

```r
highest.education = rep("999", length(nda17$demo_prnt_ed_v2))
m[0] = "##en##Never attended/Kindergarten only##/en####es##Nunca asistí/Kinder solamente##/es##"
m[1] = "##en##1st grade##/en####es##1.er grado##/es##"
m[2] = "##en##2nd grade##/en####es##2.º grado##/es##"
m[3] = "##en##3rd grade##/en####es##3.er grado##/es##"
m[4] = "##en##4th grade##/en####es##4.º grado##/es##"
m[5] = "##en##5th grade##/en####es##5.º grado##/es##"
m[6] = "##en##6th grade##/en####es##6.º grado##/es##"
m[7] = "##en##7th grade##/en####es##7.º grado##/es##"
m[8] = "##en##8th grade##/en####es##8.º grado##/es##"
m[9] = "##en##9th grade##/en####es##9.º grado##/es##"
m[10] = "##en##10th grade##/en####es##10.º grado##/es##"
m[11] = "##en##11th grade##/en####es##11.º grado##/es##"
m[12] = "##en##12th grade, no diploma##/en####es## 12.º grado, sin certificado/diploma##/es##"
m[13] = "##en##High school graduate##/en####es##Preparatoria terminada##/es##"
m[14] = "##en##GED or equivalent##/en####es##Diploma General de Equivalencia (GED) o equivalente##/es##"
m[15] = "##en##Some college, no degree##/en####es##Estudios universitarios parciales; sin título##/es##"
m[16] = "##en##Associate degree: Occupational, Technical, or Vocational##/en####es##Título de asociado: programa ocupacional, técnico o vocacional##/es##"
m[17] = "##en##Associate degree: Academic Program##/en####es##Título de asociado: programa académico##/es##"
m[18] = "##en##Bachelor's degree (ex. BA, AB, BS, BBS)##/en####es##Licenciatura (p. ej., BA, AB, BS, BBA)##/es##"
m[19] = "##en##Master's degree (ex. MA, MS, MEng, MEd, MBA)##/en####es##Maestría (p. ej., MA, MS, MEng, MEd, MBA)##/es##"
m[20] = "##en##Professional School degree (ex. MD, DDS, DVN, JD)##/en####es##Título de escuela profesional (p. ej., MD, DDS, DVM, JD)##/es##"
m[21] = "##en##Doctoral degree (ex. PhD, EdD)##/en####es##Doctorado (p. ej., PhD, EdD)##/es##"
m[77] = "##en##Refused to answer##/en####es##Prefiero no responder##/es##"
highest.education[nda17$demo_prnt_ed_v2 == m[0]] = 1
highest.education[nda17$demo_prnt_ed_v2 == m[1]] = 4
highest.education[nda17$demo_prnt_ed_v2 == m[2]] = 5
highest.education[nda17$demo_prnt_ed_v2 == m[3]] = 6
highest.education[nda17$demo_prnt_ed_v2 == m[4]] = 7
highest.education[nda17$demo_prnt_ed_v2 == m[5]] = 8
highest.education[nda17$demo_prnt_ed_v2 == m[6]] = 9
highest.education[nda17$demo_prnt_ed_v2 == m[7]] = 10
highest.education[nda17$demo_prnt_ed_v2 == m[8]] = 11
highest.education[nda17$demo_prnt_ed_v2 == m[9]] = 12
highest.education[nda17$demo_prnt_ed_v2 == m[10]] = 13
highest.education[nda17$demo_prnt_ed_v2 == m[11]] = 14
highest.education[(nda17$demo_prnt_ed_v2 == m[12]) | (nda17$demo_prnt_ed_v2 == m[13])] = 16
highest.education[nda17$demo_prnt_ed_v2 == m[14]] = 17
highest.education[nda17$demo_prnt_ed_v2 == m[15]] = 18
highest.education[(nda17$demo_prnt_ed_v2 == m[16]) | (nda17$demo_prnt_ed_v2 == m[17])] = 20
highest.education[nda17$demo_prnt_ed_v2 == m[18]] = 21
highest.education[nda17$demo_prnt_ed_v2 == m[19]] = 22
highest.education[nda17$demo_prnt_ed_v2 == m[20]] = 23
highest.education[nda17$demo_prnt_ed_v2 == m[21]] = 24
highest.education[nda17$demo_prnt_ed_v2 == m[77]] = 999
highest.education[highest.education == 999] = NA
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
```

It is worthwhile to point out here that the above category for hispanic is calculated in ABCD differently from the other race categories. In particular any ethnicity selection of hispanic will map the participant into the hispanic category regardless of the selection of one or more race categories.

```r
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
      if ( (nda17$demo_ethn_v2[i] == "<span lang=\"en\">Yes</span><span lang=\"es\" style= \"color:maroon\">Sí</span>") || (nda17$demo_ethn_v2[i] == "<span lang=\"en\">No</span><span lang=\"es\" style= \"color:maroon\">No</span>") ) {
         ethnicity_recode = nda17$demo_ethn_v2[i]
      }
      # even if you are mixed race or other, selecting hispanic ethnicity assigns the participant to the hispanic category
      if (ethnicity_recode == "<span lang=\"en\">Yes</span><span lang=\"es\" style= \"color:maroon\">Sí</span>") {
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
