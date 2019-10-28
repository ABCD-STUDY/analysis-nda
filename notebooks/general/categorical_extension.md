All variables coming from NDA tables are "factor" (without category labels); we need to put them back to the original data type, numeric or categorical (with category labels)

Read in data from output of core_demographics

```r
script.dir <- "~/Desktop/ABCD/analysis-nda18/notebooks/general/"
setwd(script.dir)

nda18 = readRDS("nda2.0.1_demo.Rds")

categories = read.csv('choices_coding_nda2.0.1.csv') 
```

This loop will go throught the column names stored in the categories table and convert those columns in the data frame to factor variables.

```r
for (kitty in categories$name) {
	#print(kitty)
    if (!(kitty %in% names(nda18))) next
    choices = strsplit(as.character(categories[categories$name == kitty,]$choices), "|",fixed=TRUE)
    lev = levels(nda18[[kitty]])
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
    nda18[[kitty]] = factor(nda18[[kitty]],levels=orig_levels, labels=lev)
    nda18[[kitty]][nda18[[kitty]] == ""] = NA
   
}

nda18 = droplevels(nda18)
```


If we ignore knows categorical variables we can try to convert all other variables to numeric variables if their levels are numerical:

```r
ncols = ncol(nda18)
colnames = names(nda18)
data_clean = nda18
typevec = NA
nlevvec = rep(NA,length(typevec))
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol | is.na(x)
for (coli in 3:ncols) {
  levvec = levels(as.factor(as.character(nda18[,coli])))
  nlev = length(levvec) 
  levvec.numeric = suppressWarnings(as.numeric(levvec))
  nnum = sum((!is.na(levvec.numeric))|(levvec=="")|(levvec=="NA")) 
  nempty = sum(levvec==""|(levvec=="NA"))
  nlevvec[coli] = nlev
  if (names(nda18)[coli] %in% categories$name) {
      typevec[coli] = 'Categorical'
  } else if (nnum==nlev) { # All numeric
    data_clean[,coli] = as.numeric(as.character(nda18[,coli]))
    nint = sum(is.wholenumber(levvec.numeric))
    if (nint==nlev) {
      typevec[coli] = 'Integer'
    } else {
      typevec[coli] = 'Real'
    }
  } else if ((nnum-nempty)==0) { # No numeric, other than empty string
    if (nlev==2) {
      typevec[coli] = 'Binary'
    } else {
      typevec[coli] = 'Categorical'
    }
  } else {
    typevec[coli] = 'Ambiguous' # Inspect more closely
  }
  cat(sprintf('%5d: type=%s nlev=%d (%s)\n',coli,typevec[coli],nlevvec[coli],colnames[coli]))
}
nda18 = data_clean
rm(data_clean)
```

Redefine race 

```r

library(data.table)
dat_nms = c("subjectid","demo_ethn_p", "demo_race_a_p___10", "demo_race_a_p___11","demo_race_a_p___12", "demo_race_a_p___13",
"demo_race_a_p___14", "demo_race_a_p___15", "demo_race_a_p___16", "demo_race_a_p___17", 
"demo_race_a_p___18", "demo_race_a_p___19", "demo_race_a_p___20", "demo_race_a_p___21", "demo_race_a_p___22","demo_race_a_p___23",
"demo_race_a_p___24", "demo_race_a_p___25",
"demo_race_a_p___77", "demo_race_a_p___99")
ind_dat = which(names(nda18)==dat_nms[1])
for(j in 2:length(dat_nms)){
ind_dat = c(ind_dat,which(names(nda18)==dat_nms[j]))
}
names(nda18)[ind_dat]
dat = data.table(nda18[nda18$eventname=="baseline_year_1_arm_1",ind_dat])

# White
dat[, white:= (demo_race_a_p___10 == "White")*1 ]

# Black
dat[, black:= (demo_race_a_p___11 == "Black/African American")*1 ]

# Asian
dat[, asian:= 0]
dat[ (demo_race_a_p___18 == "Asian Indian" | demo_race_a_p___19 == "Chinese" | demo_race_a_p___20 == "Filipino" |
demo_race_a_p___21 == "Japanese" | demo_race_a_p___22 == "Korean" | demo_race_a_p___23 == "Vietnamese" |
demo_race_a_p___24=="Other Asian"), asian:= 1 ]

# AIAN: American Indian and Alaska Native
dat[, aian:= 0]
dat[ (demo_race_a_p___12 == "American Indian, Native American" | demo_race_a_p___13 == "Alaska Native"), aian:=1 ]

#NHPI: Native Hawaiian and Other Pacific
dat[, nhpi:= 0]
dat[ demo_race_a_p___14 == "Native Hawaiian" | demo_race_a_p___15 == "Guamanian" | demo_race_a_p___16 == "Samoan" |
demo_race_a_p___17 == "Other Pacific Islander", nhpi:= 1 ]

# Other
dat[, other:= 0 ]
dat[ demo_race_a_p___25 == "Other Race", other:= 1 ]

# Mixed
dat[, mixed:= (white + black + asian + aian + nhpi + other)]
dat[, table(mixed, useNA = "if")]
dat[ mixed <= 1, mixed:= 0]
dat[ mixed > 1, mixed:= 1]
dat[, table(mixed, useNA = "if")]

# Race 4 level
dat[ white == 1, race.4level:= 1]
dat[ black == 1,race.4level:= 2]
dat[ asian == 1,race.4level:= 3]
dat[ aian == 1,race.4level:= 4]
dat[ nhpi == 1,race.4level:= 4]
dat[ other == 1,race.4level:= 4]
dat[ mixed == 1,race.4level:= 4]
dat[, table(race.4level, useNA = "if") ]

dat$race.4level<- factor(dat$race.4level,
levels = 1:4,
labels = c("White","Black","Asian","Other/Mixed"))
dat$race.eth[dat$race.eth==1] = "White"
dat$race.eth[dat$race.eth==2] = "Black"
dat$race.eth[dat$race.eth==3] ="Asian"
dat$race.eth[dat$race.eth==4] = "Other/Mixed"
dat[, table(race.4level, useNA = "if") ]

# Race 6 level
dat[white==1,race.6level:=1]
dat[black==1,race.6level:=2]
dat[asian==1,race.6level:=3]
dat[aian==1,race.6level:=4]
dat[nhpi==1,race.6level:=4]
dat[other==1,race.6level:=5]
dat[mixed==1,race.6level:=6]
dat[, table(race.6level,useNA="if") ]

dat$race.6level<- factor(dat$race.6level,
levels=1:6,
labels= c("White","Black","Asian","AIAN/NHPI","Other","Mixed"))
dat$race.eth[dat$race.6level==1] ="White"
dat$race.eth[dat$race.6level==2] ="Black"
dat$race.eth[dat$race.6level==3] ="Asian"
dat$race.eth[dat$race.6level==4] ="AIAN/NHPI"
dat$race.eth[dat$race.6level==5] ="Other"
dat$race.eth[dat$race.6level==6] ="Mixed"
dat[, table(race.6level,useNA="if") ]

# Hispanic
dat$hisp=NA;
indx.1=which(dat$demo_ethn_p=="Yes")
indx.0=which(dat$demo_ethn_p=="No")
dat$hisp[indx.1]=1;
dat$hisp[indx.0]=0;


dat$hisp<- factor(dat$hisp,
levels=0:1,
labels= c("No","Yes"))

```

Invariant variable; Baseline value carry Forward (BOCF) directly

```r
bl.race=dat[,c("subjectid","race.4level","race.6level","hisp")] #BOCF at the end;


#variables from ACS instrument (not dervied in this rds file) are also considered as invariant
constant.vars=c("race_ethnicity","rel_relationship", "rel_family_id","rel_same_sex","rel_group_id") 


bl.demo=nda18[which(nda18$eventname=="baseline_year_1_arm_1"),c("subjectid",constant.vars)]

nda18=nda18[,-which(colnames(nda18)%in%constant.vars)]
nda18=merge(nda18,bl.demo,by=c("subjectid"))


nda18=merge(nda18,bl.race,by=c("subjectid"))

dim(nda18)

saveRDS(nda18, "nda2.0.1_ext.Rds")
```
Next: merge.additional.variables