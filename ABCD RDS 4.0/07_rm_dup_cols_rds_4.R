# remove duplicated columns from image Rds
library(dplyr)

rm(list = ls())

script.dir <- getwd()
setwd(script.dir)
print(paste0("script.dir: ", script.dir))

version <- "_v82"

# def main function
process_dup_col <- function(type, action, save_results) {
  print("=============================================")
  print("started reading Rds")
  print("=============================================")

  df <- readRDS(paste0("./nda4.0_", type, ".Rds"))
  # nda4.0_non_image.Rds
  # nda4.0_image_1.Rds
  # nda4.0_image_2.Rds

  print("=============================================")
  print("finished reading Rds")
  print("=============================================")
  print(paste0("dim of ", type, ": ", dim(df)[1], " ", dim(df)[2]))

  # set up df
  dup_count_df <- data.frame(
    non_img = integer(),
    img_1 = integer(),
    img_2 = integer()
  )
  dup_item_df <- data.frame(
    type = character(),
    col_name = character()
  )

  # dup count df
  dup_x <- colnames(df)[grepl(".x", colnames(df), fixed = T)]
  dup_y <- colnames(df)[grepl(".y", colnames(df), fixed = T)]
  dup_1 <- colnames(df)[grepl(".1", colnames(df), fixed = T)]
  print(paste0(type, "_dup_x length: ", length(dup_x)))
  print(paste0(type, "_dup_y length: ", length(dup_y)))
  print(paste0(type, "_dup_1 length: ", length(dup_1)))

  dup_count_df["dup_x", type] <- length(dup_x)
  dup_count_df["dup_y", type] <- length(dup_y)
  dup_count_df["dup_1", type] <- length(dup_1)
  print(dup_count_df)

  # dup item df
  dup_items <- c(dup_x, dup_y, dup_1)
  for (i in dup_items) {
    dup_item_df[nrow(dup_item_df) + 1, ] <- c(type, i)
  }
  print(dup_item_df)

  if (save_results == T) {
    write.csv(
      dup_count_df,
      paste0("./dup_col_count_", type, version, ".csv")
    )
    write.csv(dup_item_df,
      paste0("./dup_col_items_", type, version, ".csv"),
      row.names = FALSE
    )
  }

  if (action == "clean") {
    print("=============================================")
    print("started removing dup cols")
    print("=============================================")
    df_1 <- df[, !(names(df) %in% dup_items)]
    print(paste0("dim of new ", type, ": ", dim(df_1)[1], " ", dim(df_1)[2]))

    print("=============================================")
    print("started saving new Rds")
    print("=============================================")
    saveRDS(df_1, paste0("./nda4.0_", type, version, ".Rds"))
    rm(df_1)
  }

  rm(df)
  gc()
}


# for the full table
process_dup_col_full_table <-
  function(type, version, save_results, action) {
    print("=============================================")
    print("started reading Rds")
    print("=============================================")

    df <- readRDS(paste0("./rds4_full_table", version, ".Rds"))

    print("=============================================")
    print("finished reading Rds")
    print("=============================================")
    print(paste0("dim of ", type, ": ", dim(df)[1], " ", dim(df)[2]))

    # set up df
    dup_count_df <- data.frame(
      non_img = integer(),
      img_1 = integer(),
      img_2 = integer()
    )

    # dup count df
    dup_x <- colnames(df)[grepl(".x", colnames(df), fixed = T)]
    dup_y <- colnames(df)[grepl(".y", colnames(df), fixed = T)]
    dup_1 <- colnames(df)[grepl(".1", colnames(df), fixed = T)]
    print(paste0(type, "_dup_x length: ", length(dup_x)))
    print(paste0(type, "_dup_y length: ", length(dup_y)))
    print(paste0(type, "_dup_1 length: ", length(dup_1)))

    dup_count_df["dup_x", type] <- length(dup_x)
    dup_count_df["dup_y", type] <- length(dup_y)
    dup_count_df["dup_1", type] <- length(dup_1)
    print(dup_count_df)

    # dup item df
    dup_items <- c(dup_x, dup_y, dup_1)
    for (i in dup_items) {
      dup_item_df[nrow(dup_item_df) + 1, ] <- c(type, i)
    }
    print(dup_item_df)

    if (save_results == T) {
      write.csv(
        dup_count_df,
        paste0("./dup_col_count_", type, version, ".csv")
      )
      write.csv(dup_item_df,
        paste0("./dup_col_items_", type, version, ".csv"),
        row.names = FALSE
      )
    }

    if (action == "clean") {
      print("=============================================")
      print("started removing dup cols")
      print("=============================================")
      df_1 <- df[, !(names(df) %in% dup_items)]
      print(paste0("dim of new ", type, ": ", dim(df_1)[1], " ", dim(df_1)[2]))

      print("=============================================")
      print("started saving new Rds")
      print("=============================================")
      saveRDS(df_1, paste0("./nda4.0_", type, version, ".Rds"))
      rm(df_1)
    }

    rm(df)
    gc()
  }


# run function
for (i in c("non_image", "image_1", "image_2")) {
  # save_results = F
  save_results <- T
  check_or_clean <- "check"
  # check_or_clean = 'clean'
  process_dup_col(i, check_or_clean, save_results)
  # process_dup_col(i, check_or_clean, save_results)
}

# for the full table
# process_dup_col_full_table('full table', version, F, 'check')
# process_dup_col_full_table('full table', version, F, 'clean')












# test  =============================================================
# path_col_names_non_image_4 = './ontology/rds4_full_table_colnames_v7.RData'
# load(path_col_names_non_image_4)
# cols_4 = col_names
# rm(col_names)
# length(cols_4)


# dup count df
# dup_count_df = list()
# dup_x = cols_4[grepl(".x", cols_4, fixed = T)]
# dup_y = cols_4[grepl(".y", cols_4, fixed = T)]
# dup_1 = cols_4[grepl(".1", cols_4, fixed = T)]
# print(paste0('_dup_x length: ', length(dup_x)))
# print(paste0('_dup_y length: ', length(dup_y)))
# print(paste0('_dup_1 length: ', length(dup_1)))
# dup_x
# dup_y
# dup_1
# dup_count_df['dup_x', type] = length(dup_x)
# dup_count_df['dup_y', type] = length(dup_y)
# dup_count_df['dup_1', type] = length(dup_1)
# print(dup_count_df)






# _v6
# [1] "dim of non_image: 74167 22028"
# [1] "dim of image_1: 22298 24544"
# [1] "dim of image_2: 22290 30268"

# type                             col_name
# 1 non_image reshist_addr2_pm25_2016_annual_avg.x
# 2 non_image reshist_addr2_pm25_2016_annual_avg.y
# 3 non_image                            fes_1_p.1
# 4 non_image                           fes_2r_p.1
# 5 non_image                            fes_6_p.1
# 6 non_image                           fes_7r_p.1
# 7 non_image                            fes_9_p.1
