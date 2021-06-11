## All variables coming from NDA tables are "factor" (without labels); need to put them back to the original data type, numeric or categorical (with category labels)
#nda3.0

#read in data from output of core_demographics.R
script.dir <- "~/Desktop/ABCD/analysis-nda3.0/RDS"
setwd(script.dir)

library(stringi)
type="non_image" 
#type="image_1"
#type="image_2"
if(type=="non_image"){
	nda3.0 = readRDS(paste0("nda3.0_demo_",type,".Rds"))
}else{
	nda3.0 = readRDS(paste0("nda3.0_orig_",type,".Rds"))
}
#categories = read.csv("~/Desktop/ABCD/analysis-nda3.0/coding choice/choices_coding_nda.3.0.csv")
categories = read.csv("choices_coding_nda.3.0.csv")
categories$choices = stri_trans_general(categories$choices, "Latin-ASCII")

#very weird category: 888: Decline to answer (map to redcap 777)
if(type=="non_image"){
	indx=which(categories[,1]%in%c("ksads_back_c_det_reason_p___888","ksads_back_c_det_reason_p_l___888"))
	if(length(indx)>0){
		for (i in indx){
			categories$choices[i]=c("0, not endorsed | 1, Decline to answer")
		}
	}
	#Spanish categories --> English
	var=paste0("ksads_back_c_det_reason_p___",1:8)
	indx=which(categories[,1]%in%var)

	var2=paste0("ksads_back_c_det_reason_p_l___",1:8)
	indx2=which(categories[,1]%in%var2)
	#categories[indx2,]

	if(length(indx)>0){
		for (i in 1:length(indx)){
			categories$choices[indx[i]]=categories$choices[indx2[i]]
		}
	
	}
}

#This loop will go throught the column names stored in the categories table and convert those columns in the data frame to factor variables.
for (kitty in categories$name) {
	#print(kitty)
    if (!(kitty %in% names(nda3.0))) next
    choices = strsplit(as.character(categories[categories$name == kitty,]$choices), "|",fixed=TRUE,useBytes=T)
    lev = levels(nda3.0[[kitty]])
    orig_levels = lev
    for (c in 1:length(choices[[1]])) {
    	#print(c)
        choice = choices[[1]][c] #choice could have multiple ","
        indx=as.numeric(gregexpr(",",choice,fixed=TRUE,useBytes=T)[[1]])
        if(length(indx)>1){
        		substr(choice, indx[1], indx[1]) <- ";"
        		choice=gsub(","," ",choice,fixed=T,useBytes=T)
        		choice=gsub(";",",",choice,fixed=T,useBytes=T)
        		
        }
        number = trimws(strsplit(choice, ",")[[1]][1])
        labels = strsplit(choice, ",")[[1]][-1]
        # I am not able to simply paste the result from strsplit, use a loop instead
        label = labels[[1]]
        if (length(labels)>1) for (i in 2:length(labels)) label = paste(label, labels[[i]], sep=",")
        label = trimws(label)
        lev[which(lev == number)] = label
    }
    nda3.0[[kitty]] = factor(nda3.0[[kitty]],levels=orig_levels, labels=lev)
    nda3.0[[kitty]][nda3.0[[kitty]] == ""] = NA
   
}

nda3.0 = droplevels(nda3.0)



#If we ignore knows categorical variables we can try to convert all other variables to numeric variables if their levels are numerical:
ncols = ncol(nda3.0)
colnames = names(nda3.0)
data_clean = nda3.0
typevec = NA
nlevvec = rep(NA,length(typevec))
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol | is.na(x)
for (coli in 3:ncols) {
  levvec = levels(as.factor(as.character(nda3.0[,coli])))
  nlev = length(levvec) 
  levvec.numeric = suppressWarnings(as.numeric(levvec))
  nnum = sum((!is.na(levvec.numeric))|(levvec=="")|(levvec=="NA")) 
  nempty = sum(levvec==""|(levvec=="NA"))
  nlevvec[coli] = nlev
  if (names(nda3.0)[coli] %in% categories$name) {
      typevec[coli] = 'Categorical'
  } else if (nnum==nlev) { # All numeric
    data_clean[,coli] = as.numeric(as.character(nda3.0[,coli]))
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
nda3.0 = data_clean

if(type=="non_image"){
	#need manual work on ksads_import_id_p and ksads_import_id_t
	#555, not administered

	nda3.0$ksads_import_id_p=as.character(nda3.0$ksads_import_id_p)
	indx=which(nda3.0$ksads_import_id_p=="555") #there is no 888
	if(length(indx)>0){
		nda3.0$ksads_import_id_p[indx]="not administered"
	}
	nda3.0$ksads_import_id_t=as.character(nda3.0$ksads_import_id_t)
	indx=which(nda3.0$ksads_import_id_t=="555") #there is no 888
	if(length(indx)>0){
		nda3.0$ksads_import_id_t[indx]="not administered"
	}
}
rm(data_clean)

########################################################
#image merging is finished here:
########################################################
if(type!="non_image"){
	dim(nda3.0)
	table(nda3.0$eventname)

	saveRDS(nda3.0, paste0("nda3.0_",type,".Rds"))
}

#Only continue if merging non-imaging tables ...	
	
##############################################################################################################
#redefine race;
############################
#code from Wes/expert
library(data.table)
dat_nms = c("subjectid",
            "demo_ethn_p", "demo_race_a_p___10", "demo_race_a_p___11","demo_race_a_p___12", "demo_race_a_p___13",
            "demo_race_a_p___14", "demo_race_a_p___15", "demo_race_a_p___16", "demo_race_a_p___17", 
            "demo_race_a_p___18", "demo_race_a_p___19", "demo_race_a_p___20", "demo_race_a_p___21", "demo_race_a_p___22","demo_race_a_p___23",
            "demo_race_a_p___24", "demo_race_a_p___25", 
            "demo_race_a_p___77", "demo_race_a_p___99")
ind_dat = which(names(nda3.0)==dat_nms[1])
for(j in 2:length(dat_nms)){
ind_dat = c(ind_dat,which(names(nda3.0)==dat_nms[j]))
}
names(nda3.0)[ind_dat]
dat = data.table(nda3.0[nda3.0$eventname=="baseline_year_1_arm_1",ind_dat])

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
dat[ (demo_race_a_p___12 %in%c("American Indian, Native American","American Indian  Native American") | demo_race_a_p___13 == "Alaska Native"), aian:=1 ]

#  NHPI: Native Hawaiian and Other Pacific
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
dat[ black == 1, race.4level:= 2]
dat[ asian == 1, race.4level:= 3]
dat[ aian == 1, race.4level:= 4]
dat[ nhpi == 1, race.4level:= 4]
dat[ other == 1, race.4level:= 4]
dat[ mixed == 1, race.4level:= 4]
dat[, table(race.4level, useNA = "if") ]

dat$race.4level <- factor(dat$race.4level,
                        levels = 1:4,
                        labels = c("White","Black","Asian","Other/Mixed"))
dat$race.eth[dat$race.eth==1] = "White"
dat$race.eth[dat$race.eth==2] = "Black"
dat$race.eth[dat$race.eth==3] = "Asian"
dat$race.eth[dat$race.eth==4] = "Other/Mixed"
dat[, table(race.4level, useNA = "if") ]


# Race 6 level
dat[ white == 1, race.6level:= 1]
dat[ black == 1, race.6level:= 2]
dat[ asian == 1, race.6level:= 3]
dat[ aian == 1, race.6level:= 4]
dat[ nhpi == 1, race.6level:= 4]
dat[ other == 1, race.6level:= 5]
dat[ mixed == 1, race.6level:= 6]
dat[, table(race.6level, useNA = "if") ]

dat$race.6level <- factor(dat$race.6level,
                        levels = 1:6,
                        labels = c("White","Black","Asian","AIAN/NHPI","Other","Mixed"))
dat$race.eth[dat$race.6level==1] = "White"
dat$race.eth[dat$race.6level==2] = "Black"
dat$race.eth[dat$race.6level==3] = "Asian"
dat$race.eth[dat$race.6level==4] = "AIAN/NHPI"
dat$race.eth[dat$race.6level==5] = "Other"
dat$race.eth[dat$race.6level==6] = "Mixed"
dat[, table(race.6level, useNA = "if") ]

     
# Hispanic
dat$hisp=NA;
indx.1=which(dat$demo_ethn_p=="Yes")
indx.0=which(dat$demo_ethn_p=="No")
dat$hisp[indx.1]=1;
dat$hisp[indx.0]=0;


dat$hisp <- factor(dat$hisp,
                        levels = 0:1,
                        labels = c("No","Yes"))
table(dat$hisp,useNA="ifany")


bl.race=dat[,c("subjectid","race.4level","race.6level","hisp")] #BOCF at the end;
##############################################################################################################
#Baseline observation carry forward (BOCF)
#For statistical modeling purpose, some variables collected at baseline, their values need to be carried forward to the follow-up visits.

#these are assumed to be invariant, BOCF
#race_ethnicity: from ACS instrument (not dervied in this rds file)
constant.vars=c("race_ethnicity","rel_relationship", "rel_family_id","rel_same_sex","rel_group_id") 

bl.demo=nda3.0[which(nda3.0$eventname=="baseline_year_1_arm_1"),c("subjectid",constant.vars)]


nda3.0=nda3.0[,-which(colnames(nda3.0)%in%constant.vars)]
nda3.0=merge(nda3.0,bl.demo,by=c("subjectid"))


nda3.0=merge(nda3.0,bl.race,by=c("subjectid"))

dim(nda3.0) #non_Image: 54594 14734
table(nda3.0$eventname)

saveRDS(nda3.0, paste0("nda3.0_ext_",type,".Rds"))

#Next: non-image: merge.additional.variables.R
#image: Done!