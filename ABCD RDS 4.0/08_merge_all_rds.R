# merge_all_rds

library(dplyr)

rm(list = ls())

script.dir <- getwd()
setwd(script.dir)
print(paste0("script.dir: ", script.dir))

print("start reading img_1")
img_1 <- readRDS("./nda4.0_image_1.Rds")
print(paste0("img_1 dim: ", dim(img_1)[1], " ", dim(img_1)[2]))
print("start reading img_2")
img_2 <- readRDS("./nda4.0_image_2.Rds")
print(paste0("img_2 dim: ", dim(img_2)[1], " ", dim(img_2)[2]))

print("=====================================")
print("start full-joining img_1 and img_2")
by.vars <- c("src_subject_id", "eventname")
img <- img_1 %>% full_join(img_2, by = by.vars)
print(paste0("img dim: ", dim(img)[1], " ", dim(img)[2]))

rm(img_1)
rm(img_2)
gc()

print("=====================================")
print("start reading non_img")
non_img <- readRDS("./nda4.0_non_image.Rds")
print(paste0("non_img dim: ", dim(non_img)[1], " ", dim(non_img)[2]))

print("=====================================")
print("start full-joining non_img and img")
table <- non_img %>% full_join(img, by = by.vars)
print(paste0("table dim: ", dim(table)[1], " ", dim(table)[2]))

rm(non_img)
rm(img)
gc()

# dup_x = colnames(table)[grepl(".x", colnames(table), fixed = T)]
# dup_y = colnames(table)[grepl(".y", colnames(table), fixed = T)]
# dup_1 = colnames(table)[grepl(".1", colnames(table), fixed = T)]
# dup_items = c(dup_x, dup_y, dup_1)
# print(paste0('number of dup cols after merging into 1 table: ', length(dup_items)))
# print(dup_items)

# print('=============================================')
# print('started removing dup cols')
# print('=============================================')
# table = table[, !(names(table) %in% dup_items)]
# print(paste0('dim of new ', ': ', dim(table)[1], ' ', dim(table)[2]))

print("=====================================")
col_names <- colnames(table)
save(file = "./rds4_full_table_colnames_v82.RData", col_names)

print("start saving table with saveRDS")
saveRDS(table, "./rds4_full_table_v82.Rds")
print("finished saving table with saveRDS")

