## Definition of convenience variables
# Most of these are simple re-definitions of existing columns with simpler names, other columns are re-scored versions of nda3.0 columns.
rm(list = ls())
gc()

script.dir <- getwd()
setwd(script.dir)
print(paste0("script.dir: ", script.dir))

print("===========================================")
print("started reading file")
# only non_image merge needs core_demographics.R
type <- "non_image"
nda_4 <- readRDS(paste0("nda4.0_orig_", type, ".Rds"))
print("finished reading file")
print("===========================================")

print(dim(nda_4))

print("===========================================")
print("started processing core_demographic")
print("===========================================")
# site
print("started processing site")

# site_id_l is in longitudinal tracking instrument
nda_4$abcd_site <- nda_4$site_id_l

### Subjectid
print("started processing Subjectid")
nda_4$subjectid <- nda_4$src_subject_id

### Age (in month)
# Get a better name for interview_age.
print("started processing age")
nda_4$age <- nda_4$interview_age

### Female.
print("started processing sex (gender)")
nda_4$female <- factor(as.numeric(nda_4$sex == "F"),
  levels = 0:1,
  labels = c("no", "yes")
)

### Household income
print("started processing income")
household.income <- nda_4$demo_comb_income_p
household.income[nda_4$demo_comb_income_p == "1"] <- 1 # "[<50K]"
household.income[nda_4$demo_comb_income_p == "2"] <- 1 # "[<50K]"
household.income[nda_4$demo_comb_income_p == "3"] <- 1 # "[<50K]"
household.income[nda_4$demo_comb_income_p == "4"] <- 1 # "[<50K]"
household.income[nda_4$demo_comb_income_p == "5"] <- 1 # "[<50K]"
household.income[nda_4$demo_comb_income_p == "6"] <- 1 # "[<50K]"
household.income[nda_4$demo_comb_income_p == "7"] <- 2 # "[>=50K & <100K]"
household.income[nda_4$demo_comb_income_p == "8"] <- 2 # "[>=50K & <100K]"
household.income[nda_4$demo_comb_income_p == "9"] <- 3 # "[>=100K]"
household.income[nda_4$demo_comb_income_p == "10"] <- 3 # "[>=100K]"
household.income[nda_4$demo_comb_income_p == "777"] <- NA
household.income[nda_4$demo_comb_income_p == "999"] <- NA
household.income[household.income %in% c(NA, "999", "777")] <- NA
nda_4$household.income <- factor(
  household.income,
  levels = 1:3,
  labels = c("[<50K]", "[>=50K & <100K]", "[>=100K]")
)

# Here a simplified version of the highest education that results in only 5 different levels.
# These levels correspond to the numbers published by the American Community Survey (ACS).
print("started processing education")
high.educ1 <- nda_4$demo_prnt_ed_p
high.educ2 <- nda_4$demo_prtnr_ed_p
high.educ1[which(high.educ1 == "999")] <- NA
high.educ2[which(high.educ2 == "999")] <- NA
high.educ1[which(high.educ1 == "777")] <- NA
high.educ2[which(high.educ2 == "777")] <- NA
high.educ <- pmax(as.numeric(as.character(high.educ1)), as.numeric(as.character(high.educ2)),
  na.rm =
    T
)
idx <- which(high.educ %in% 0:12, arr.ind = TRUE)
high.educ[idx] <- 1 # "< HS Diploma"
idx <- which(high.educ %in% 13:14, arr.ind = TRUE)
high.educ[idx] <- 2 # "HS Diploma/GED"
idx <- which(high.educ %in% 15:17, arr.ind = TRUE)
high.educ[idx] <- 3 # "Some College"
idx <- which(high.educ == 18, arr.ind = TRUE)
high.educ[idx] <- 4 # "Bachelor"
idx <- which(high.educ %in% 19:21, arr.ind = TRUE)
high.educ[idx] <- 5 # "Post Graduate Degree"
high.educ[which(high.educ == "999")] <- NA
high.educ[which(high.educ == "777")] <- NA
nda_4$high.educ <- factor(
  high.educ,
  levels = 1:5,
  labels = c(
    "< HS Diploma",
    "HS Diploma/GED",
    "Some College",
    "Bachelor",
    "Post Graduate Degree"
  )
)

# Marital status
print("started processing marital status")
married <- rep(NA, length(nda_4$demo_prnt_marital_p))
married[nda_4$demo_prnt_marital_p == 1] <- 1
married[nda_4$demo_prnt_marital_p %in% 2:6] <- 0
nda_4$married <- factor(married, levels = 0:1, labels = c("no", "yes"))

# Add another variable that also includes couples that just live together.
married.livingtogether <- rep(NA, length(nda_4$demo_prnt_marital_p))
married.livingtogether[nda_4$demo_prnt_marital_p %in% c(1, 6)] <- 1
married.livingtogether[nda_4$demo_prnt_marital_p %in% 2:5] <- 0
nda_4$married.or.livingtogether <- factor(married.livingtogether,
  levels = 0:1,
  labels = c("no", "yes")
)

# Body-Mass index
print("started processing BMI")

# to calculate anthro_weight_calc, according to abcd_data_dictionary
# Calculation: if([anthro_weight3_lb] <> '',
#                 mean([anthro_weight3_lb],[anthro_weight2_lb],[anthro_weight1_lb]),
#                 mean([anthro_weight1_lb],[anthro_weight2_lb]))

nda_4$anthro_weight_calc <- ifelse(
  !is.na(nda_4$anthro_weight1_lb) & !is.na(nda_4$anthro_weight2_lb),
  ifelse(!is.na(nda_4$anthro_weight3_lb),
    (round((
      as.numeric(nda_4$anthro_weight1_lb) + as.numeric(nda_4$anthro_weight2_lb) + as.numeric(nda_4$anthro_weight3_lb)
    ) / 3, 2)),
    (round((
      as.numeric(nda_4$anthro_weight1_lb) + as.numeric(nda_4$anthro_weight2_lb)
    ) / 2, 2))
  ),
  NA
)

nda_4$anthro_bmi_calc <- as.numeric(as.character(nda_4$anthro_weight_calc)) / as.numeric(as.character(nda_4$anthro_height_calc))^
  2 * 703
nda_4$anthro_bmi_calc[which(nda_4$anthro_bmi_calc > 36 |
  nda_4$anthro_bmi_calc < 11)] <- NA
# reset unrealistic values;
# https://www.cdc.gov/nccdphp/dnpao/growthcharts/who/examples/example4_pop_cdc_bmi.htm
# I chose to eliminate any values that were "off the chart" aka >36 or < 11. .
# Rebecca Umbach, PhD
#* anthro_weight_calc was dropped in previous processing (data_merge_nda_4_0_server)

# redefine ethnicity in categorical_extension.R

##################################################################
# these variables can be changed overtime,but need baseline values filled in follow up visits
print("started processing baseline")
bl.vars <- c(
  "married.or.livingtogether",
  "married",
  "high.educ",
  "household.income",
  "demo_rel_family_id",
  "demo_rel_group_id",
  "demo_rel_relationship",
  "demo_rel_same_sex"
)
bl.demo <- nda_4[which(nda_4$eventname == "baseline_year_1_arm_1"), c("subjectid", bl.vars)]
print(paste0("initial dim for bl.demo: ", dim(bl.demo)[1], " ", dim(bl.demo)[2]))
bl.demo <- bl.demo[!duplicated(bl.demo[c("subjectid")]), ]
print(paste0(
  "dim after drop duplicates for bl.demo: ",
  dim(bl.demo)[1],
  " ",
  dim(bl.demo)[2]
))
colnames(bl.demo)[-1] <- paste0(bl.vars, ".bl") # rename these variables to baseline variables
print("===========================================")
print("started merging nda_4, bl.demo")
nda_4 <- merge(nda_4, bl.demo, by = c("subjectid"), all.x = TRUE)
dim(nda_4)
table(nda_4$eventname)

# fill values for these vars
# "race_ethnicity", "rel_relationship", "rel_family_id", "rel_same_sex", "rel_group_id"
nda_4$race_ethnicity <- nda_4$demo_race_ethnicity

nda_4$rel_relationship <- nda_4$demo_rel_relationship.bl
nda_4$demo_rel_relationship <- nda_4$demo_rel_relationship.bl

nda_4$rel_family_id <- nda_4$demo_rel_family_id.bl
nda_4$demo_rel_family_id <- nda_4$demo_rel_family_id.bl

nda_4$rel_same_sex <- nda_4$demo_rel_same_sex.bl
nda_4$demo_rel_same_sex <- nda_4$demo_rel_same_sex.bl

nda_4$rel_group_id <- nda_4$demo_rel_group_id.bl
nda_4$demo_rel_group_id <- nda_4$demo_rel_group_id.bl

print("===========================================")
print(paste0("started saving new rds: ", "nda4.0_demo_", type, ".Rds"))
saveRDS(nda_4, paste0("nda4.0_demo_", type, ".Rds"))

# Next: categorical_extension3.0.R
