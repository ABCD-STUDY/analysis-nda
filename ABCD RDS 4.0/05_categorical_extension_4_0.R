# All variables coming from NDA tables are "factor" (without labels);
# need to put them back to the original data type, numeric or categorical (with category labels)

library(stringi)
library(dplyr)
library(data.table)
rm(list = ls())
gc()

script.dir <- getwd()
setwd(script.dir)
print(paste0("script.dir: ", script.dir))

type <- "non_image"
# type = "image_1"
# type = "image_2"

print("=============================================")
print("started reading file")
print("=============================================")
print(type)
if (type == "non_image") {
  nda4 <- readRDS(paste0("nda4.0_demo_", type, ".Rds"))
  gc()
} else {
  nda4 <- readRDS(paste0("nda4.0_orig_", type, ".Rds"))
}
print(paste0("finished reading file ", type, ": ", dim(nda4)[1], " * ", dim(nda4)[2]))

categories <- read.csv("./choices_coding_nda.4.0.csv")
categories$choices <- stri_trans_general(categories$choices, "Latin-ASCII")
categories$choices <- iconv(categories$choices, "UTF-8", "ASCII", sub = "")
print(paste0(
  "finished reading choices_coding_nda.4.0.csv ",
  dim(categories)[1],
  " * ",
  dim(categories)[2]
))

print("=============================================")
print("processing very weird category: 888: Decline to answer (map to redcap 777)")
print("=============================================")
"ksads_back_c_det_reason_p___888" %in% categories$names # false
"ksads_back_c_det_reason_p_l___888" %in% categories$names # false
"ksads_back_c_det_reason_p___1" %in% categories$names # false
"ksads_back_c_det_reason_p_l___1" %in% categories$names # false

if (type == "non_image") {
  indx <- which(
    categories[, 1] %in% c(
      "ksads_back_c_det_reason_p___888",
      "ksads_back_c_det_reason_p_l___888"
    )
  )
  print(paste0("indx: ", indx))
  if (length(indx) > 0) {
    for (i in indx) {
      categories$choices[i] <- c("0, not endorsed | 1, Decline to answer")
    }
  }
  # Spanish categories --> English
  var <- paste0("ksads_back_c_det_reason_p___", 1:8)
  indx <- which(categories[, 1] %in% var)
  print(paste0("indx: ", indx))

  var2 <- paste0("ksads_back_c_det_reason_p_l___", 1:8)
  indx2 <- which(categories[, 1] %in% var2)
  print(paste0("indx2: ", indx2))

  if (length(indx) > 0) {
    for (i in 1:length(indx)) {
      categories$choices[indx[i]] <- categories$choices[indx2[i]]
    }
  }
}

print("=============================================")
print("loop go through col names stored in the categories table and convert those to factor vars")
print("=============================================")
# This loop will go through the column names stored in the categories table and
# convert those columns in the data frame to factor variables.

for (i in categories$name) {
  # i = 'ksads_suicidal_raw_1122_t'
  if (!(i %in% names(nda4))) {
    next
  }
  choices <- strsplit(as.character(categories[categories$name == i, ]$choices),
    "|",
    fixed = TRUE,
    useBytes = T
  )

  # there are invalid ones due to choices parsing issue
  if (is.na(choices)) {
    next
  }

  lev <- levels(factor(nda4[[i]]))
  orig_levels <- lev
  for (c in 1:length(choices[[1]])) {
    choice <- choices[[1]][c] # choice could have multiple ","
    indx <- as.numeric(gregexpr(",", choice, fixed = TRUE, useBytes = T)[[1]])
    if (length(indx) > 1) {
      substr(choice, indx[1], indx[1]) <- ";"
      choice <- gsub(",", " ", choice, fixed = T, useBytes = T)
      choice <- gsub(";", ",", choice, fixed = T, useBytes = T)
    }
    number <- trimws(strsplit(choice, ",")[[1]][1])
    labels <- strsplit(choice, ",")[[1]][-1]
    if (length(labels) != 0) {
      # I am not able to simply paste the result from strsplit, use a loop instead
      label <- labels[[1]]
      if (length(labels) > 1) {
        for (i in 2:length(labels)) {
          label <- paste(label, labels[[i]], sep = ",")
        }
      }
      label <- trimws(label)
      lev[which(lev == number)] <- label
    }
  }
  nda4[[i]] <- factor(nda4[[i]], levels = orig_levels, labels = lev)
  nda4[[i]][nda4[[i]] == ""] <- NA
}

nda4 <- droplevels(nda4)


# If we ignore knows categorical variables we can try to convert all other variables
# to numeric variables if their levels are numerical:
ncols <- ncol(nda4)
colnames <- names(nda4)
data_clean <- nda4
typevec <- NA
nlevvec <- rep(NA, length(typevec))
is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol | is.na(x)
  }
for (coli in 3:ncols) {
  levvec <- levels(as.factor(as.character(nda4[, coli])))
  nlev <- length(levvec)
  levvec.numeric <- suppressWarnings(as.numeric(levvec))
  nnum <- sum((!is.na(levvec.numeric)) |
    (levvec == "") | (levvec == "NA"))
  nempty <- sum(levvec == "" | (levvec == "NA"))
  nlevvec[coli] <- nlev
  if (names(nda4)[coli] %in% categories$name) {
    typevec[coli] <- "Categorical"
  } else if (nnum == nlev) {
    # All numeric
    data_clean[, coli] <- as.numeric(as.character(nda4[, coli]))
    nint <- sum(is.wholenumber(levvec.numeric))
    if (nint == nlev) {
      typevec[coli] <- "Integer"
    } else {
      typevec[coli] <- "Real"
    }
  } else if ((nnum - nempty) == 0) {
    # No numeric, other than empty string
    if (nlev == 2) {
      typevec[coli] <- "Binary"
    } else {
      typevec[coli] <- "Categorical"
    }
  } else {
    typevec[coli] <- "Ambiguous" # Inspect more closely
  }
  if (coli %% 1000 == 0) {
    cat(sprintf(
      "%5d: type=%s nlev=%d (%s)\n",
      coli,
      typevec[coli],
      nlevvec[coli],
      colnames[coli]
    ))
  }
}

nda4 <- data_clean

if (type == "non_image") {
  # need manual work on ksads_import_id_p and ksads_import_id_t
  # 555, not administered
  "ksads_import_id_p" %in% colnames(nda4) # T
  "ksads_import_id_t" %in% colnames(nda4) # T
  "555" %in% nda4$ksads_import_id_p # F
  "555" %in% nda4$ksads_import_id_t # F
  nda4$ksads_import_id_p <- as.character(nda4$ksads_import_id_p)
  indx <- which(nda4$ksads_import_id_p == "555") # there is no 888
  if (length(indx) > 0) {
    nda4$ksads_import_id_p[indx] <- "not administered"
  }
  nda4$ksads_import_id_t <- as.character(nda4$ksads_import_id_t)
  indx <- which(nda4$ksads_import_id_t == "555") # there is no 888
  if (length(indx) > 0) {
    nda4$ksads_import_id_t[indx] <- "not administered"
  }
}
rm(data_clean)
gc()

########################################################
# image merging is finished here:
########################################################
if (type != "non_image") {
  dim(nda4)
  table(nda4$eventname)
  print(dim(nda4))
  print("=============================================")
  print("started saving file nda4.Rds")
  print("=============================================")
  saveRDS(nda4, paste0("nda4.0_", type, ".Rds"))
  print(type)
  print("=============================================")
  print("finished saving file nda4.Rds")
  print("=============================================")
  quit()
}

# Only continue if merging non-imaging tables ...

############################
# redefine race;
############################
# code from Wes/expert
print("=============================================")
print("started demography processing")
print("=============================================")
dat_nms <- c(
  "subjectid",
  "demo_ethn_p",
  "demo_race_a_p___10",
  "demo_race_a_p___11",
  "demo_race_a_p___12",
  "demo_race_a_p___13",
  "demo_race_a_p___14",
  "demo_race_a_p___15",
  "demo_race_a_p___16",
  "demo_race_a_p___17",
  "demo_race_a_p___18",
  "demo_race_a_p___19",
  "demo_race_a_p___20",
  "demo_race_a_p___21",
  "demo_race_a_p___22",
  "demo_race_a_p___23",
  "demo_race_a_p___24",
  "demo_race_a_p___25",
  "demo_race_a_p___77",
  "demo_race_a_p___99"
)
ind_dat <- which(names(nda4) == dat_nms[1])
for (j in 2:length(dat_nms)) {
  ind_dat <- c(ind_dat, which(names(nda4) == dat_nms[j]))
}
names(nda4)[ind_dat]
dat <- data.table(nda4[nda4$eventname == "baseline_year_1_arm_1", ind_dat])

dat$demo_ethn_p <- as.integer(dat$demo_ethn_p)
count(dat, demo_ethn_p)

# White 'White'
dat[, white := (demo_race_a_p___10 == "White Blanca") * 1]

# Black 'Black/African American'
dat[, black := (demo_race_a_p___11 == "Black/African American Negra o afroamericana") * 1]

# Asian
# 'Asian Indian', 'Chinese', 'Filipino', 'Japanese', 'Korean', 'Vietnamese', 'Other Asian'
dat[, asian := 0]
dat[(
  demo_race_a_p___18 == "Asian Indian India asitica" |
    demo_race_a_p___19 == "Chinese China" |
    demo_race_a_p___20 == "Filipino Filipina" |
    demo_race_a_p___21 == "Japanese Japonesa" |
    demo_race_a_p___22 == "Korean Coreana" |
    demo_race_a_p___23 == "Vietnamese Vietnamita" |
    demo_race_a_p___24 == "Other Asian Otra raza asitica"
), asian := 1]

# aian: American Indian and Alaska Native
# demo_race_a_p___12 -> 'American Indian, Native American', 'American Indian  Native American'
# demo_race_a_p___13 -> 'Alaska Native'
dat[, aian := 0]
dat[(
  demo_race_a_p___12 == "American Indian  Native American India Americana  India Nativa Americana" |
    demo_race_a_p___13 == "Alaska Native Nativa de Alaska"
), aian := 1]

# NHPI: Native Hawaiian and Other Pacific
# demo_race_a_p___14 'Native Hawaiian', demo_race_a_p___15'Guamanian', demo_race_a_p___16'Samoan', demo_race_a_p___17'Other Pacific Islander'
dat[, nhpi := 0]
dat[
  demo_race_a_p___14 == "Native Hawaiian Nativa de Hawi" |
    demo_race_a_p___15 == "Guamanian Guamaniana" |
    demo_race_a_p___16 == "Samoan Samoana" |
    demo_race_a_p___17 == "Other Pacific Islander Nativa de otras islas del Pacifico",
  nhpi := 1
]

# Other
# demo_race_a_p___25 == "Other Race"
dat[, other := 0]
dat[demo_race_a_p___25 == "Other Race Otra raza", other := 1]

# Mixed
dat[, mixed := (white + black + asian + aian + nhpi + other)]
dat[, table(mixed, useNA = "if")]
dat[mixed <= 1, mixed := 0]
dat[mixed > 1, mixed := 1]
dat[, table(mixed, useNA = "if")]

# Race 4 level
dat[white == 1, race.4level := 1]
dat[black == 1, race.4level := 2]
dat[asian == 1, race.4level := 3]
dat[aian == 1, race.4level := 4]
dat[nhpi == 1, race.4level := 4]
dat[other == 1, race.4level := 4]
dat[mixed == 1, race.4level := 4]
dat[, table(race.4level, useNA = "if")]

dat$race.4level <- factor(
  dat$race.4level,
  levels = 1:4,
  labels = c("White", "Black", "Asian", "Other/Mixed")
)
# dat$race.eth
dat$race.eth[dat$race.eth == 1] <- "White"
dat$race.eth[dat$race.eth == 2] <- "Black"
dat$race.eth[dat$race.eth == 3] <- "Asian"
dat$race.eth[dat$race.eth == 4] <- "Other/Mixed"
dat[, table(race.4level, useNA = "if")]

# Race 6 level
dat[white == 1, race.6level := 1]
dat[black == 1, race.6level := 2]
dat[asian == 1, race.6level := 3]
dat[aian == 1, race.6level := 4]
dat[nhpi == 1, race.6level := 4]
dat[other == 1, race.6level := 5]
dat[mixed == 1, race.6level := 6]
dat[, table(race.6level, useNA = "if")]

dat$race.6level <- factor(
  dat$race.6level,
  levels = 1:6,
  labels = c("White", "Black", "Asian", "AIAN/NHPI", "Other", "Mixed")
)
dat$race.eth[dat$race.6level == 1] <- "White"
dat$race.eth[dat$race.6level == 2] <- "Black"
dat$race.eth[dat$race.6level == 3] <- "Asian"
dat$race.eth[dat$race.6level == 4] <- "AIAN/NHPI"
dat$race.eth[dat$race.6level == 5] <- "Other"
dat$race.eth[dat$race.6level == 6] <- "Mixed"
dat[, table(race.6level, useNA = "if")]

# Hispanic
# ref: https://nda.nih.gov/data_structure.html?short_name=pdem02
# demo_ethn_p is the alias of demo_ethn_v2
# 1 = Yes Sí; 2 = No No; 777 = Refuse to answer Niego contestar; 999 = Don't know No lo sé
dat$hisp <- NA

indx.1 <- which(dat$demo_ethn_p == "1")
indx.0 <- which(dat$demo_ethn_p == "2")
dat$hisp[indx.1] <- 1
dat$hisp[indx.0] <- 0

dat$hisp <- factor(dat$hisp, levels = 0:1, labels = c("No", "Yes"))
table(dat$hisp, useNA = "ifany")

bl.race <- dat[, c("subjectid", "race.4level", "race.6level", "hisp")] # BOCF at the end;
print(paste0("initial dim for bl.race: ", dim(bl.race)[1], " ", dim(bl.race)[2]))
# bl.race = bl.race[!duplicated(bl.race[c("subjectid")]), ]
# print(paste0('dim after drop duplicates for bl.race: ', dim(bl.race)[1], ' ', dim(bl.race)[2]))

count(bl.race, race.4level)
count(bl.race, race.6level)
count(bl.race, hisp)

##############################################################################################################
# Baseline observation carry forward (BOCF)
# For statistical modeling purpose, some variables collected at baseline, their values need to be carried forward to the follow-up visits.
# need to have the below variables for the deap app
# "sex_at_birth", "female", "rel_family_id", "race.4level", "race.6level", "race_ethnicity", "high.educ", "household.income"
# need to fill values for rel_family_id, high.educ, household.income
# these are assumed to be invariant, BOCF
# race_ethnicity: from ACS instrument (not dervied in this rds file)
#* these vars are not in nda4 colnames, so no need to run below
#* these vars taken cared of in core_demographics_4_0.R

# constant.vars = c("race_ethnicity", "rel_relationship", "rel_family_id", "rel_same_sex", "rel_group_id")
# bl.demo = nda4[which(nda4$eventname == "baseline_year_1_arm_1"), c("subjectid", constant.vars)]
# print('finished demography processing')
# print(dim(bl.demo))
# nda4 = nda4[, -which(colnames(nda4) %in% constant.vars)]
# nda4 = merge(nda4, bl.demo, by=c("subjectid"))

print("=============================================")
print("started merging nda4, bl.race")
print("=============================================")
nda4 <- merge(nda4, bl.race, by = c("subjectid"), all.x = TRUE)

dim(nda4)
table(nda4$eventname)

print("=============================================")
print("started saving file")
print("=============================================")
saveRDS(nda4, paste0("nda4.0_ext_", type, ".Rds"))

# Next: non-image: merge.additional.variables.R
# image: Done!
