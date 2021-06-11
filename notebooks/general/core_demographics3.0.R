## Definition of convenience variables
# Most of these are simple re-definitions of existing columns with simplier names, other columns are re-scored versions of nda3.0 columns.

# Start by reading in the merged data from disk.
script.dir <- "~/Desktop/ABCD/analysis-nda3.0/RDS"
setwd(script.dir)

type="non_image" #only non_image merge needs core_demographics.R
nda3.0 = readRDS(paste0("nda3.0_orig_",type,".Rds"))


#site
nda3.0$abcd_site = nda3.0$site_id_l #site_id_l is in longitudianl tracking instrument


### Subjectid
nda3.0$subjectid = nda3.0$src_subject_id

### Age (in month)
#Get a better name for interview_age.
nda3.0$age = nda3.0$interview_age


### Female. 
nda3.0$female = factor(as.numeric(nda3.0$sex == "F"), levels = 0:1, labels = c("no", "yes") ) 


### Household income
household.income = nda3.0$demo_comb_income_p
household.income[nda3.0$demo_comb_income_p == "1"] = 1 # "[<50K]"
household.income[nda3.0$demo_comb_income_p == "2"] = 1 # "[<50K]"
household.income[nda3.0$demo_comb_income_p == "3"] = 1 # "[<50K]"
household.income[nda3.0$demo_comb_income_p == "4"] = 1 # "[<50K]"
household.income[nda3.0$demo_comb_income_p == "5"] = 1 # "[<50K]"
household.income[nda3.0$demo_comb_income_p == "6"] = 1 # "[<50K]"
household.income[nda3.0$demo_comb_income_p == "7"] = 2 # "[>=50K & <100K]"
household.income[nda3.0$demo_comb_income_p == "8"] = 2 # "[>=50K & <100K]"
household.income[nda3.0$demo_comb_income_p == "9"] = 3 # "[>=100K]"
household.income[nda3.0$demo_comb_income_p == "10"] = 3 # "[>=100K]"
household.income[nda3.0$demo_comb_income_p == "777"] = NA
household.income[nda3.0$demo_comb_income_p == "999"] = NA
household.income[household.income %in% c(NA, "999", "777")] = NA
nda3.0$household.income = factor( household.income, levels= 1:3, labels = c("[<50K]", "[>=50K & <100K]", "[>=100K]") )



#Here a simplified version of the highest education that results in only 5 different levels. These levels correspond to the numbers published by the American Community Survey (ACS). 
high.educ1 = nda3.0$demo_prnt_ed_p
high.educ2 = nda3.0$demo_prtnr_ed_p
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
nda3.0$high.educ = factor( high.educ, levels= 1:5, labels = c("< HS Diploma","HS Diploma/GED","Some College","Bachelor","Post Graduate Degree") )


### Marrital status
married = rep(NA, length(nda3.0$demo_prnt_marital_p))
married[nda3.0$demo_prnt_marital_p == 1] = 1
married[nda3.0$demo_prnt_marital_p %in% 2:6] = 0
nda3.0$married = factor( married, levels= 0:1, labels = c("no", "yes") )

#Add another variable that also includes couples that just live together. 
married.livingtogether = rep(NA, length(nda3.0$demo_prnt_marital_p))
married.livingtogether[nda3.0$demo_prnt_marital_p %in% c(1,6)] = 1
married.livingtogether[nda3.0$demo_prnt_marital_p %in% 2:5] = 0
nda3.0$married.or.livingtogether = factor( married.livingtogether, levels= 0:1, labels = c("no", "yes") )



### Body-Mass index
nda3.0$anthro_bmi_calc = as.numeric(as.character(nda3.0$anthro_weight_calc)) / as.numeric(as.character(nda3.0$anthro_height_calc))^2 * 703
nda3.0$anthro_bmi_calc[which(nda3.0$anthro_bmi_calc>36 | nda3.0$anthro_bmi_calc < 11)]=NA; #reset unrealistic values;
#https://www.cdc.gov/nccdphp/dnpao/growthcharts/who/examples/example4_pop_cdc_bmi.htm
# I chose to eliminate any values that were "off the chart" aka >36 or < 11. .
# Rebecca Umbach, PhD


#redefine ethnicity in categorical_extension.R

##################################################################
#these variables can be changed overtime,but need baseline values filled in follow up visits
bl.vars=c("married.or.livingtogether","married","high.educ","household.income") 
bl.demo=nda3.0[which(nda3.0$eventname=="baseline_year_1_arm_1"),c("subjectid",bl.vars)]
colnames(bl.demo)[-1]=paste0(bl.vars,".bl") #rename these variables to baseline variables

nda3.0=merge(nda3.0,bl.demo,by=c("subjectid"))
dim(nda3.0) #non_image: 54594 14731
table(nda3.0$eventname)

#Save the new data frame again.
saveRDS(nda3.0, paste0("nda3.0_demo_",type,".Rds"))

#Next: categorical_extension3.0.R


