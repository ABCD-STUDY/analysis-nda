library(dplyr)
library(stringi)
library(data.table)

rm(list = ls())

is_local <- F
if (is_local == T) {
  dir_non_img <- "D:\\Projects\\Projects\\RDS 4.0\\data\\non_img"
  dir_img <- "D:\\Projects\\Projects\\RDS 4.0\\data\\img"
  dir_released <- "D:\\Projects\\Projects\\RDS 4.0\\data\\released"
  script.dir <- getwd()
  setwd(script.dir)
} else {
  dir_non_img <- "/space/abcd-sync/4.0/tabulated/non_img"
  dir_img <- "/space/abcd-sync/4.0/tabulated/img"
  dir_released <- "/space/abcd-sync/4.0/tabulated/released"
  script.dir <- getwd()
  setwd(script.dir)
}
print(paste0("script.dir: ", script.dir))

# tmp_file = '//abcd_auto_postqc01.txt'
# header_a = read.table(paste0(dir_released, tmp_file), nrows = 1, header = FALSE, stringsAsFactors = FALSE)
# a = read.table(paste0(dir_released, tmp_file), skip = 2, header = FALSE)
# colnames(a) = unlist(header_a)
# colnames(a)
#######################################
# merge for non-image (or image)
#######################################
# three different types
type <- "non_image"
# type = "image_1"
# type = "image_2"

print("=============================================")
print("started reading directory and alias for: ")
print(type)
print("=============================================")

#######################################
# non-image
#######################################
non_image.tables <- list.files(path = dir_non_img)
input_list_non_image <- NULL

#######################################
# image
#######################################
# break image tables into 2 sections (too large to merge altogether)
# PART 1
image.tables <- list.files(path = dir_img)
image.tables <- gsub(".csv", "", image.tables)
# partition the image files later after removing and adding the files

# abcd_lt01 in all sections: non-image, image_1, image_2
# * abcd_lt01 only in non-image

#######################################
# released => use this section to find out file differences between folders
#######################################
# released.tables = list.files(path = dir_released)
# input_list_released = NULL
# for (i in 1:length(released.tables)) {
#   if (is_local == T) {
#     tmp.list = paste0(dir_released, "\\", released.tables[i])
#   } else {
#     tmp.list = paste0(dir_released, "/", released.tables[i])
#   }
#   input_list_released = c(input_list_released, tmp.list)
# }
# released.tables = gsub(".txt", "", released.tables)
# print(paste0('length of released.tables: ', length(released.tables)))
# print(paste0('length of duplicated released.tables: ', which(duplicated(released.tables)))) #0 or empty
# print(paste0('length of released.tables: ', length(input_list_released)))

# find different files between /non_img /img and /released folders
# existing_tables = do.call(c, list(non_image.tables, image_1.tables, image_2.tables))
# length(existing_tables)
# only_in_released = list()
# for (i in released.tables) {
#   if (!i %in% existing_tables) {
#     only_in_released = c(only_in_released, i)
#   }
# }
# length(only_in_released)
# only_in_existing = list()
# for (i in existing_tables) {
#   if (!i %in% released.tables) {
#     only_in_existing = c(only_in_existing, i)
#   }
# }
# length(only_in_existing)

#######################################
# alias
#######################################
alia <- read.csv("./NDA_DEAP_names_4.0.csv")
alia <- alia[!apply(is.na(alia) | alia == "", 1, all), ]
# fam_enviro's alias is also the name of another nda (^fes), so not renaming them
fam_enviro_nda_vars <- alia$nda[grepl("^fam_enviro", alia$nda)]
other_exclude_vars <- c("nihtbx_list_rawscore")
alias_exclusions <- c(fam_enviro_nda_vars, other_exclude_vars)
alia <- alia[!alia$nda %in% alias_exclusions, ]

# extend wide-format deap into alia
if (type == "non_image") {
  fitbit.w <- read.csv("./fitbit_wk_wide_format.deap.csv")[, -c(1, 2)]
  colnames(fitbit.w) <- colnames(alia)
  alia <- data.frame(rbind(alia, fitbit.w))
}

# not merged files
skip_orig <- c(
  "abcd_socdev_ctr01",
  "abcd_fbdpas01",
  "abcd_fbdss01",
  "abcd_fbwpas01",
  "abcd_fbwss01",
  "abcd_lt01"
)
skip_released_files <- c(
  "omics_experiments",
  "aurora01",
  "fmriresults01",
  "pabcdcovid19questionnaire01",
  "yabcdcovid19questionnaire01",
  "smarte_trial_level_behav01"
)
skip_files <- c(skip_orig, skip_released_files)
# skip_files = c('abcd_socdev_ctr01', 'abcd_fbdpas01', 'abcd_fbdss01',) <= originally removed files
# omics_experiments, no useful info, removed in legacy 'merge_data.md'
# aurora01 no eventname, visit_name all NA, removed in legacy 'merge_data.md'
# smarte_trial_level_behav01 no eventname, visit_name all NA
# fmriresults01 no eventname or visit_name, removed in legacy 'merge_data.md'
# pabcdcovid19questionnaire01, yabcdcovid19questionnaire01 covid19_cv7_arm_2 6010

# update non-image and image files here, keeps a separate file of files names for referencing
released_files_reference <- read.csv("./released_files_update_reference.csv")
if (type == "non_image") {
  # non_img
  skip_non_img <- released_files_reference %>%
    filter(
      released_files_reference$in.Existing.or.in.Released == "only in existing" &
        released_files_reference$Type == "non_img"
    ) %>%
    select(File.Name) %>%
    sapply(as.character) %>%
    as.vector()

  add_non_img <- released_files_reference %>%
    filter(
      released_files_reference$in.Existing.or.in.Released == "only in released" &
        released_files_reference$Type == "non_img"
    ) %>%
    select(File.Name) %>%
    sapply(as.character) %>%
    as.vector()

  # currently not merging 'socdev' files due to their different eventname
  soc_dev_files <- c(non_image.tables[grepl("socdev", non_image.tables)], non_image.tables[grepl("soc_dev", non_image.tables)])
  length(soc_dev_files) # 35
  non_image.tables <- non_image.tables[!(non_image.tables %in% soc_dev_files)]
  non_image.tables <- gsub(".csv", "", non_image.tables)
  non_image.tables <- non_image.tables[!(non_image.tables %in% skip_non_img)]
  non_image.tables <- c(non_image.tables, add_non_img)
  length(non_image.tables)
  for (i in 1:length(non_image.tables)) {
    if (is_local == T) {
      tmp.list <- paste0(dir_released, "\\", non_image.tables[i], ".txt")
    } else {
      tmp.list <- paste0(dir_released, "/", non_image.tables[i], ".txt")
    }
    input_list_non_image <- c(input_list_non_image, tmp.list)
  }
  print(paste0("length of non_image.tables: ", length(non_image.tables))) # 240 (including abcd_lt01) = 257 - 35 + 18
  print(paste0("length of duplicated non_image.tables: ", which(duplicated(non_image.tables)))) # 0 or empty
  print(paste0(
    "length of input_list_non_image: ",
    length(input_list_non_image)
  )) # 240 (including abcd_lt01)
} else {
  # img
  skip_img <- released_files_reference %>%
    filter(
      released_files_reference$in.Existing.or.in.Released == "only in existing" &
        released_files_reference$Type == "img"
    ) %>%
    select(File.Name) %>%
    sapply(as.character) %>%
    as.vector()

  add_img <- released_files_reference %>%
    filter(
      released_files_reference$in.Existing.or.in.Released == "only in released" &
        released_files_reference$Type == "img"
    ) %>%
    select(File.Name) %>%
    sapply(as.character) %>%
    as.vector()

  # remove skipped ones
  image.tables <- image.tables[!(image.tables %in% skip_img)]
  # add ones from /released
  image.tables <- c(image.tables, add_img)
  length(image.tables)

  # PART 1
  input_list_image_1 <- NULL
  for (i in 1:50) {
    if (is_local == T) {
      tmp.list <- paste0(dir_released, "\\", image.tables[i], ".txt")
    } else {
      tmp.list <- paste0(dir_released, "/", image.tables[i], ".txt")
    }
    input_list_image_1 <- c(input_list_image_1, tmp.list)
  }
  image_1.tables <- gsub(".txt", "", image.tables[1:50])
  print(paste0("length of image_1.tables: ", length(image_1.tables))) # 50
  print(paste0("length of duplicated image_1.tables: ", which(duplicated(image_1.tables)))) # 0 or empty
  print(paste0("length of input_list_image_1: ", length(input_list_image_1))) # 50
  # PART 2
  input_list_image_2 <- NULL
  for (i in 51:length(image.tables)) {
    if (is_local == T) {
      tmp.list <- paste0(dir_released, "\\", image.tables[i], ".txt")
    } else {
      tmp.list <- paste0(dir_released, "/", image.tables[i], ".txt")
    }
    input_list_image_2 <- c(input_list_image_2, tmp.list)
  }
  image_2.tables <- gsub(".txt", "", image.tables[51:length(image.tables)])
  print(paste0("length of image_2.tables: ", length(image_2.tables))) # 46
  print(paste0("length of duplicated image_2.tables: ", which(duplicated(image_2.tables)))) # 0 or empty
  print(paste0("length of input_list_image_2: ", length(input_list_image_2))) # 46
}

input_list <- eval(parse(text = paste0("input_list_", type))) # full file name
instrument.name <- eval(parse(text = paste0(type, ".tables"))) # file name

table <- list()

# read abcd_lt01 longitudinal tracking file as anchor (base of table)
if (is_local == T) {
  header_lt01 <- read.table(
    paste0(dir_released, "\\abcd_lt01.txt"),
    nrows = 1,
    header = FALSE,
    stringsAsFactors = FALSE
  )
  tb_lt01 <- read.table(paste0(dir_released, "\\abcd_lt01.txt"),
    skip = 2,
    header = FALSE
  )
  colnames(tb_lt01) <- unlist(header_lt01)
} else {
  header_lt01 <- read.table(
    paste0(dir_released, "/abcd_lt01.txt"),
    nrows = 1,
    header = FALSE,
    stringsAsFactors = FALSE
  )
  tb_lt01 <- read.table(paste0(dir_released, "/abcd_lt01.txt"),
    skip = 2,
    header = FALSE
  )
  colnames(tb_lt01) <- unlist(header_lt01)
}
print(paste0("tb_lt01 original dim: ", dim(tb_lt01)[1], " ", dim(tb_lt01)[2]))

if (type != "non_image") {
  # img files only have two types of eventname
  tb_lt01 <- tb_lt01[tb_lt01$eventname %in% c("baseline_year_1_arm_1", "2_year_follow_up_y_arm_1"), ]
  print(paste0("tb_lt01 new dim for img: ", dim(tb_lt01)[1], " ", dim(tb_lt01)[2]))
}

table <- tb_lt01
count(table, eventname)
emptycolumn <- list()
dup_table <- list()
skipped_merging_list <- list()

print("=============================================")
print("started reading file")
print("=============================================")
for (i in 1:length(input_list)) {
  # read file
  if (!instrument.name[i] %in% skip_files) {
    # input_list
    input <- input_list[i]
    print(paste("import: ", input, " [", i, "/", length(input_list), "]",
      sep =
        ""
    ))

    dt <- tryCatch(
      {
        # 1st line is column names
        header_a <- read.table(
          input,
          nrows = 1,
          header = FALSE,
          stringsAsFactors = FALSE
        )
        # 2nd line is description for each column name
        a <- read.table(
          input,
          skip = 2,
          sep = "\t",
          comment.char = "",
          quote = "\"",
          header = FALSE
        )
        colnames(a) <- unlist(header_a)

        a <- as.data.frame(sapply(a, function(x) {
          gsub("\"", "", x)
        }))
        names(a) <- as.list(sapply(names(a), function(x) {
          gsub("\"", "", x)
        }))
        a
      },
      error = function(e) {
        print(e)
        return(read.table(
          file = input,
          sep = "\t",
          header = TRUE
        ))
      }
    )

    print(paste0(i, " ", dim(dt)))

    # replace variable names from nda with DEAP names
    instrument <- instrument.name[i]

    # alias file is all lower-case
    colnames(dt) <- tolower(colnames(dt))

    ali <- alia[which(alia$instrument == instrument), ]
    nn <- names(dt)
    for (q in 1:length(nn)) {
      if (nn[q] %in% ali$nda) {
        colnames(dt)[q] <- as.character(ali$abcd[ali$nda == nn[q]])
      }
    }

    # change screener_arm_1 to baseline_year_1_arm_1, nda doesn't have screener
    if (instrument %in% c("abcd_screen01", "abcd_socdev_ctr01")) {
      dt$eventname <- "baseline_year_1_arm_1"
    }

    # rm few empty rows
    #* couldn't find these ids
    if (instrument == "abcd_mri01") {
      rms <- c(
        "S065_INV2ZA2LC3N_20171215",
        "S076_INV3E0WVH3G_20171129",
        "S053_INVJ9GNXGK5_20180107",
        "S076_INVWE1DE80Z_20170913",
        "S076_INVXN6HMGK8_20170913"
      )
      indx <- which(dt$mri_info_visitid %in% rms)
      if (length(indx) > 0) {
        dt <- dt[-indx, ]
      }
    }

    #* lmtp201 doesn't have src_subject_id, instead has lmt_subject_id
    if (instrument == "lmtp201") {
      colnames(dt)[which(names(dt) == "lmt_subject_id")] <-
        "src_subject_id"
    }

    dt$subjectkey <- trimws(dt$subjectkey, which = c("both"))
    dt$src_subject_id <- trimws(dt$src_subject_id, which = c("both"))

    # ==========================================================
    # process
    # ==========================================================
    # find and remove duplicated id and eventname
    if ("src_subject_id" %in% colnames(dt) &&
      "eventname" %in% colnames(dt)) {
      tmp <- dt[duplicated(dt[c("src_subject_id", "eventname")]), ]
      tmp <- tmp[c("src_subject_id", "eventname")]
      if (length(tmp) > 0) {
        tmp <- as.data.frame(tmp)
        if (length(dup_table) == 0) {
          dup_table <- tmp
          dup_table <- dup_table %>%
            mutate(instrument_name = as.character(instrument))
        } else {
          tmp <- tmp %>%
            mutate(instrument_name = as.character(instrument))
          dup_table <- rbind(dup_table, tmp)
        }
      }
      dt <- dt[!duplicated(dt[c("src_subject_id", "eventname")]), ]
      print(paste0(instrument, " ", dim(dt)[1], " ", dim(dt)[2]))
    } else {
      print("src_subject_id or eventname not in colnames")
    }

    # Drop columns introduced by NDA, they are not required in the resulting table.
    # keep dataset_id (QC purpose) and will remove later
    # if we merge tables from SYNC, then don't need this
    dt <- dt[, !(
      names(dt) %in% c(
        paste0(instrument, "_id"),
        "collection_id",
        "collection_title",
        "promoted_subjectkey",
        "subjectkey",
        "study_cohort_name",
        "dataset_id"
      )
    )]

    emptycolumns <- list()
    if (length(names(dt)) > 0) {
      emptycolumns <- append(emptycolumns, names(dt)[sapply(dt, function(x) {
        all((x == "") | (x == "NA") | is.na(x))
      })])
      dt <- dt[!sapply(dt, function(x) {
        all((x == "") | (x == "NA") | is.na(x))
      })]
    }

    emptycolumn <- append(emptycolumn, unlist(emptycolumns))
    # image_1:100; image_2: 140; non-image:965
    #* 4.0: image_1:100; image_2: 65; non-image:2510

    # rm gender/sex,interview_age and interview_date from all instrument but keeping lt01 as anchor;
    rm.vars <- c(
      "visit",
      "interview_age",
      "interview_date",
      "sex",
      "gender",
      "taskname",
      "abbrev_taskname",
      "vendor"
    )
    if (type != "non_image") {
      rm.vars <- c(
        "visit",
        "dataset_id",
        "VisitID",
        "visitid",
        "interview_age",
        "interview_date",
        "sex",
        "gender"
      )
    }

    dt <- dt[, !(colnames(dt) %in% rm.vars)]

    # check the vars, for some, only lt01 has it
    tmp_rm_vars <- c(
      "visit",
      "interview_age",
      "dataset_id",
      "VisitID",
      "visitid",
      "interview_date",
      "sex",
      "gender",
      "collection_id",
      "collection_title",
      "promoted_subjectkey",
      "subjectkey",
      "study_cohort_name",
      "taskname",
      "abbrev_taskname",
      "vendor"
    )

    for (var in tmp_rm_vars) {
      if (var %in% colnames(dt)) {
        print(paste(i, "-", instrument, "-", var))
      }
    }

    # Sometimes the "eventname" column shared in many instruments is called "visit" (but in freesqc01 "eventname" and "visit" are different,
    # it has been taken care in the NDA_DEAP_names file).
    if ("visit" %in% names(dt)) {
      print(paste0("visit in ", instrument)) # should be nothing
      quit()
    }

    if (length(grepl("data_file", colnames(dt))) > 0) {
      colnames(dt)[which(grepl("data_file", colnames(dt)))] <-
        paste0(instrument, "_", colnames(dt)[which(grepl("data_file", colnames(dt)))])
    }

    ######################################
    # check: if any table without eventname
    if (!("eventname" %in% names(dt))) {
      print(paste0("no eventname in: ", instrument))
      skipped_merging_list <- c(skipped_merging_list, instrument)
      print(paste0(instrument.name[i], " file skipped"))
    } else {
      # check: if any table without src_subject_id
      if (!("src_subject_id" %in% names(dt))) {
        print(paste0("no src_subject_id in: ", instrument))
        skipped_merging_list <- c(skipped_merging_list, instrument)
        print(paste0(instrument.name[i], " file skipped"))
      } else {
        # re-calibrate the levels in each table. Information that has been removed in previous steps
        # could have changed the factor information in each table.
        dt <- droplevels(dt)

        ############################################################
        # merge
        by.vars <- c("src_subject_id", "eventname")
        table <- table %>% left_join(dt, by = by.vars)
        gc()
        print(paste0("dim table: ", dim(table)))
      }
    }
  } else {
    skipped_merging_list <- c(skipped_merging_list, instrument)
    print(paste0(instrument.name[i], " file skipped"))
  }
}

# ==============================================================================================================
# ==============================================================================================================
print("===========================================")
print("merge completed")
print("===========================================")
print("abcd_lt01 dimension: ")
dim(tb_lt01)
print("merged table dimension: ")
dim(table)
table(table$eventname)

if (length(emptycolumn) > 0) {
  write.csv(
    file = paste0("release4.0.", type, ".empty.col.removed.csv"),
    emptycolumn,
    row.names = F
  )
  print(paste0("empty columns: ", length(emptycolumn)))
}

if (length(dup_table) > 0) {
  write.csv(
    file = paste0("release4.0.", type, ".dup_items.csv"),
    dup_table,
    row.names = F
  )
  print(paste0("duplicated items: ", dim(dup_table)))
}

if (length(skipped_merging_list) > 0) {
  write.csv(
    file = paste0("release4.0.", type, ".skipped_merging_list.csv"),
    skipped_merging_list,
    row.names = F
  )
  print(paste0("duplicated items: ", dim(skipped_merging_list)))
}

# double check duplicated variables:
colnames(table)[grepl(".x", colnames(table), fixed = T)]
colnames(table)[grepl(".y", colnames(table), fixed = T)]
colnames(table)[grepl(".1", colnames(table), fixed = T)]

# need this file to make coding choice in non-image;
# for image can use this to cross-check variables from two parts (any duplicates?)
col_names <- colnames(table)
save(file = paste0(type, "_column_names.RData"), col_names)

print("===========================================")
print("started saving file: nda4.0_orig_")
print("===========================================")
saveRDS(table, paste0("nda4.0_orig_", type, ".Rds"))

# if non_image: go to: core_demographics3.0.R
# if image: skip core_demographics3.0.R, go to categorical_extension3.0.R directly

# img 1
# [1] "merged table dimension: "
# [1] 22290 25693
#
# 2_year_follow_up_y_arm_1    baseline_year_1_arm_1
# 10414                    11876
# [1] "empty columns: 2234"
# [1] "duplicated items: 2" "duplicated items: 3"
# character(0)
# character(0)
# character(0)

# img 2
# [1] "merged table dimension: "
# [1] 22290 28143
#
# 2_year_follow_up_y_arm_1    baseline_year_1_arm_1
# 10414                    11876
# [1] "empty columns: 198"
# [1] "duplicated items: 0" "duplicated items: 3"
# character(0)
# character(0)
# character(0)
