# library(plyr)
library(dplyr)
library(stringi)
library(data.table)

rm(list=ls())

is_local = F
if (is_local == T) {
  dir_non_img = 'D:\\Projects\\Projects\\RDS 4.0\\data\\non_img'
  dir_img = 'D:\\Projects\\Projects\\RDS 4.0\\data\\img'
  dir_released = 'D:\\Projects\\Projects\\RDS 4.0\\data\\released'
  script.dir <- "D:\\Projects\\Projects\\RDS 4.0\\analysis-nda-master\\analysis-nda-master\\notebooks\\general\\rds_4_scripts"
  setwd(script.dir)
} else {
  dir_non_img = '/space/abcd-sync/4.0/tabulated/non_img'
  dir_img = '/space/abcd-sync/4.0/tabulated/img'
  dir_released = '/space/abcd-sync/4.0/tabulated/released'
  dir_data = '/home/w3zheng/data'
  script.dir <- getwd()
  setwd(script.dir)
}
print(paste0('script.dir: ', script.dir))

type = "fitbit"

print('=============================================')
print('started reading directory and alias for:')
print(type)
print('=============================================')

#######################################
# alias 
#######################################
alia = read.csv("./NDA_DEAP_names_4.0.csv")
alia = alia[!apply(is.na(alia) | alia == "", 1, all), ]
# extend wide-format deap into alia
if (type == "non_image") {
  fitbit.w = read.csv("./fitbit_wk_wide_format.deap.csv")[, -c(1,2)]
  colnames(fitbit.w) = colnames(alia)
  alia = data.frame(rbind(alia, fitbit.w))
}

input_list = c('abcd_fbdpas01', 'abcd_fbdss01', 'abcd_fbwpas01', 'abcd_fbwss01')
instrument.name = input_list

table = list()

# read abcd_lt01 longitudinal tracking file as anchor (base of table)
if (is_local == T) {
  header_lt01 = read.table(paste0(dir_released, '\\abcd_lt01.txt'), nrows = 1, header = FALSE, stringsAsFactors = FALSE)
  tb_lt01 = read.table(paste0(dir_released, '\\abcd_lt01.txt'), skip = 2, header = FALSE)
  colnames(tb_lt01) = unlist(header_lt01)
} else {
  header_lt01 = read.table(paste0(dir_data, '/abcd_lt01.txt'), nrows = 1, header = FALSE, stringsAsFactors = FALSE)
  tb_lt01 = read.table(paste0(dir_data, '/abcd_lt01.txt'), skip = 2, header = FALSE)
  colnames(tb_lt01) = unlist(header_lt01)
}
print(paste0('tb_lt01 original dim: ', dim(tb_lt01)[1], ' ', dim(tb_lt01)[2]))

if (type != 'non_image') {
  # img files only have two types of eventname
  tb_lt01 = tb_lt01[tb_lt01$eventname %in% c('baseline_year_1_arm_1', '2_year_follow_up_y_arm_1'), ]
  print(paste0('tb_lt01 new dim for img: ', dim(tb_lt01)[1], ' ', dim(tb_lt01)[2]))
}

# table = tb_lt01
table = data.frame()
# count(table, eventname)
emptycolumn = list()
dup_table = list()
skipped_merging_list = list()

rbindx <- function(..., dfs=list(...)) {
  ns <- unique(unlist(sapply(dfs, names)))
  do.call(rbind, lapply(dfs, function(x) {
    for(n in ns[! ns %in% names(x)]) {x[[n]] <- NA}; x }))
}

print('=============================================')
print('started reading file')
print('=============================================')
for (i in 1:length(input_list)) {
  # read file
  input = input_list[i]
  if (is_local == T) {
    input = paste0(dir_released, '\\', input, '.txt')
  } else {
    input = paste0(dir_data, '/', input, '.txt')
  }
  print(paste("import: ", input, " [", i, "/", length(input_list), "]", sep=""))
  
  dt <- tryCatch (
    {
      # 1st line is column names
      header_a = read.table(input, nrows=1, header=FALSE, stringsAsFactors=FALSE)
      # 2nd line is description for each column name
      a = read.table(input, skip=2, sep="\t", comment.char="", quote="\"", header=FALSE)
      colnames(a) = unlist(header_a)
      
      a = as.data.frame(sapply(a, function(x) gsub("\"", "", x)))
      names(a) = as.list(sapply(names(a), function(x) gsub("\"", "", x)))
      a
    }, 
    error = function(e) {
      print(e)
      return(read.table(file=input, sep='\t', header=TRUE))
    }
  )
  
  print(paste0(i, ' ', dim(dt)))
  
  # replace variable names from nda with DEAP names
  instrument = instrument.name[i]
  
  # alias file is all lower-case
  colnames(dt) = tolower(colnames(dt))
  
  ali = alia[which(alia$instrument == instrument), ]
  nn = names(dt)
  for (q in 1:length(nn)) {
    if (nn[q] %in% ali$nda) {
      colnames(dt)[q] <- as.character(ali$abcd[ali$nda == nn[q]])
    }
  }
  
  dt$subjectkey = trimws(dt$subjectkey, which = c("both"))
  dt$src_subject_id = trimws(dt$src_subject_id, which = c("both"))
  
  # ==========================================================
  # process
  # ==========================================================
  # Drop columns introduced by NDA, they are not required in the resulting table.
  # keep dataset_id (QC purpose) and will remove later
  # if we merge tables from SYNC, then don't need this
  dt = dt[, !(names(dt) %in% c(paste0(instrument, "_id"),
                               "collection_id", "collection_title", "promoted_subjectkey", 
                               "subjectkey", "study_cohort_name", 'dataset_id'))]
  
  emptycolumns = list()
  if (length(names(dt)) > 0){
    emptycolumns = append(emptycolumns, names(dt)[sapply(dt, function(x) all((x=="") | (x=="NA") | is.na(x)))])
    dt = dt[!sapply(dt, function(x) all((x=="") | (x=="NA") | is.na(x)))]
  }
  
  emptycolumn = append(emptycolumn, unlist(emptycolumns))
  
  # rm gender/sex,interview_age and interview_date from all instrument but keeping lt01 as anchor;
  rm.vars = c('visit', 'interview_age', 'interview_date', 'sex', 'gender', 'taskname', 'abbrev_taskname', 'vendor')
  if (type != "non_image") {
    rm.vars = c(rm.vars, "visit", 'dataset_id', 'VisitID', 'visitid', 'interview_age', 'interview_date', 'sex', 'gender')
  }
  
  dt = dt[, !(colnames(dt) %in% rm.vars)]
  
  # check the vars, for some, only lt01 has it
  tmp_rm_vars = c('visit', 'interview_age', 'dataset_id', 'VisitID', 'visitid', "interview_date", "sex", "gender",
                  'collection_id', "collection_title", "promoted_subjectkey",
                  'subjectkey', "study_cohort_name", 'taskname', 'abbrev_taskname', 'vendor')
  
  for (var in tmp_rm_vars) {
    if (var %in% colnames(dt) ){
      print(paste(i, '-', instrument, '-', var))
    }
  }
  
  # Sometimes the "eventname" column shared in many instruments is called "visit" (but in freesqc01 "eventname" and "visit" are different,
  # it has been taken care in the NDA_DEAP_names file).
  if ('visit' %in% names(dt) ){
    print(paste0('visit in ', instrument)) #should be nothing
    quit()
  }
  
  if (length(grepl('data_file', colnames(a))) > 0) {
    colnames(a)[which(grepl('data_file', colnames(a)))] <- paste0(instrument, '_', colnames(a)[which(grepl('data_file', colnames(a)))])
  }
  
  ######################################
  #check: if any table without eventname 
  if (!('eventname' %in% names(dt))) {
    print(paste0('no eventname in: ', instrument))
    skipped_merging_list = c(skipped_merging_list, instrument)
    print(paste0(instrument.name[i], ' file skipped'))
  } else {
    #check: if any table without src_subject_id 
    if (!('src_subject_id' %in% names(dt))) {
      print(paste0('no src_subject_id in: ', instrument))
      skipped_merging_list = c(skipped_merging_list, instrument)
      print(paste0(instrument.name[i], ' file skipped'))
    } else {
      # re-calibrate the levels in each table. Information that has been removed in previous steps 
      # could have changed the factor information in each table.
      dt = droplevels(dt)
      
      ############################################################
      # merge
      # by.vars = c('src_subject_id', 'eventname')
      
      # table <- merge(table, dt, by=by.vars, all.y=T)
      # table <- rbind.fill(table, dt)
      # table <- rbind(table, dt, fill = TRUE)
      if (dim(table)[1] == 0) {
        table = dt
      } else {
        table <- rbindx(table, dt)
      }
      
      gc()
      print(paste0('dim table: ', dim(table)))
    }
  }
}


by.vars = c('src_subject_id', 'eventname')
table <- merge(table, tb_lt01, by=by.vars, all.x = T)

# ==============================================================================================================
# ==============================================================================================================
print('===========================================')
print('merge completed')
print('===========================================')
print('abcd_lt01 dimension: ')
dim(tb_lt01)
print('merged table dimension: ')
dim(table)
table(table$eventname)

if (length(emptycolumn) > 0) {
  write.csv(file=paste0('release4.0.', type, '.empty.col.removed.csv'), emptycolumn, row.names=F)
  print(paste0('empty columns: ', length(emptycolumn)))
}

if (length(dup_table) > 0) {
  write.csv(file=paste0('release4.0.', type, '.dup_items.csv'), dup_table, row.names=F)
  print(paste0('duplicated items: ', dim(dup_table)))
}

if (length(skipped_merging_list) > 0) {
  write.csv(file=paste0('release4.0.', type, '.skipped_merging_list.csv'), skipped_merging_list, row.names=F)
  print(paste0('duplicated items: ', dim(skipped_merging_list)))
}

# double check duplicated variables:
colnames(table)[grepl('.x', colnames(table), fixed=T)]
colnames(table)[grepl('.y', colnames(table), fixed=T)]
colnames(table)[grepl('.1', colnames(table), fixed=T)]

# need this file to make coding choice in non-image; 
# for image can use this to cross-check variables from two parts (any duplicates?)
col_names = colnames(table)
save(file = paste0(type, '_column_names.RData'), col_names)

print('===========================================')
print('started processing core_demographic')
print('===========================================')
nda_4 = table
rm(table)
gc()

print(dim(nda_4))

#site
print('started processing site')

#site_id_l is in longitudinal tracking instrument
nda_4$abcd_site = nda_4$site_id_l 

### Subjectid
print('started processing Subjectid')
nda_4$subjectid = nda_4$src_subject_id

### Age (in month)
#Get a better name for interview_age.
print('started processing age')
nda_4$age = nda_4$interview_age

### Female.
print('started processing sex (gender)')
nda_4$female = factor(as.numeric(nda_4$sex=="F"), levels=0:1, labels=c("no", "yes"))

dim(nda_4)

print('===========================================')
print('started processing categorical_extension')
print('===========================================')
nda4 = nda_4
rm(nda_4)
gc()

categories = read.csv("./choices_coding_nda.4.0.csv")
categories$choices = stri_trans_general(categories$choices, "Latin-ASCII")
categories$choices = iconv(categories$choices, "UTF-8", "ASCII", sub = "")
print(paste0('finished reading choices_coding_nda.4.0.csv ', dim(categories)[1], ' * ', dim(categories)[2]))


print('=============================================')
print('loop go through col names stored in the categories table and convert those to factor vars')
print('=============================================')
# This loop will go through the column names stored in the categories table and 
# convert those columns in the data frame to factor variables.

for (i in categories$name) {
  # i = 'ksads_suicidal_raw_1122_t'
  if (!(i %in% names(nda4))) next
  choices = strsplit(as.character(categories[categories$name == i, ]$choices), 
                     "|", 
                     fixed=TRUE, useBytes=T)
  
  # there are invalid ones due to choices parsing issue
  if (is.na(choices)) {
    next
  }
  
  lev = levels(factor(nda4[[i]]))
  orig_levels = lev
  for (c in 1:length(choices[[1]])) {
    choice = choices[[1]][c] #choice could have multiple ","
    indx = as.numeric(gregexpr(",", choice, fixed=TRUE, useBytes=T)[[1]])
    if (length(indx) > 1) {
      substr(choice, indx[1], indx[1]) <- ";"
      choice = gsub(",", " ", choice, fixed=T, useBytes=T)
      choice = gsub(";", ",", choice, fixed=T, useBytes=T)
    }
    number = trimws(strsplit(choice, ",")[[1]][1])
    labels = strsplit(choice, ",")[[1]][-1]
    if (length(labels) != 0) {
      # I am not able to simply paste the result from strsplit, use a loop instead
      label = labels[[1]]
      if (length(labels) > 1) for (i in 2:length(labels)) label = paste(label, labels[[i]], sep=",")
      label = trimws(label)
      lev[which(lev == number)] = label
    }
  }
  nda4[[i]] = factor(nda4[[i]], levels=orig_levels, labels=lev)
  nda4[[i]][nda4[[i]] == ""] = NA
}

nda4 = droplevels(nda4) 


# If we ignore knows categorical variables we can try to convert all other variables 
# to numeric variables if their levels are numerical:
ncols = ncol(nda4)
colnames = names(nda4)
data_clean = nda4
typevec = NA
nlevvec = rep(NA, length(typevec))
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol | is.na(x)
for (coli in 3:ncols) {
  levvec = levels(as.factor(as.character(nda4[,coli])))
  nlev = length(levvec) 
  levvec.numeric = suppressWarnings(as.numeric(levvec))
  nnum = sum((!is.na(levvec.numeric)) | (levvec=="") | (levvec=="NA")) 
  nempty = sum(levvec=="" | (levvec=="NA"))
  nlevvec[coli] = nlev
  if (names(nda4)[coli] %in% categories$name) {
    typevec[coli] = 'Categorical'
  } else if (nnum == nlev) { 
    # All numeric
    data_clean[, coli] = as.numeric(as.character(nda4[, coli]))
    nint = sum(is.wholenumber(levvec.numeric))
    if (nint == nlev) {
      typevec[coli] = 'Integer'
    } else {
      typevec[coli] = 'Real'
    }
  } else if ((nnum - nempty) == 0) { # No numeric, other than empty string
    if (nlev == 2) {
      typevec[coli] = 'Binary'
    } else {
      typevec[coli] = 'Categorical'
    }
  } else {
    typevec[coli] = 'Ambiguous' # Inspect more closely
  }
  if (coli %% 1000 == 0) {
    cat(sprintf('%5d: type=%s nlev=%d (%s)\n', coli, typevec[coli], nlevvec[coli], colnames[coli]))
  }
}

nda4 = data_clean

dim(nda4)
table(nda4$eventname)

saveRDS(nda4, paste0('nda4.0_', type, '.Rds'))

