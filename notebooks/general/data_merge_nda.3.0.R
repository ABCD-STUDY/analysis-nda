## Merge NDA3.0 data tables into two sections: non-imaging and imagine

rm(list=ls())

script.dir <- "~/Desktop/ABCD/analysis-nda3.0/RDS"
setwd(script.dir)

#######################################
##non-image (removed 4 fitbit tables due to multiple enteries; but weekly data were added back as wide-format into RDS)
my.path="./SYNC/ABCD_3.0_non_imaging_tabulated/" #abcd_fbwpas01 and abcd_fbwss01 in this folder are the wide-format already
non_image.folder=list.dirs(path = my.path, full.names = F, recursive = TRUE)
non_image.folder=non_image.folder[which(non_image.folder!="")]
non_image.tables=NULL;
for (i in 1:length(non_image.folder)){
	tbs=list.files(path=paste0(my.path,non_image.folder[i]))
	non_image.tables=c(non_image.tables,tbs)
}
length(non_image.tables) 
which(duplicated(non_image.tables))
non_image.tables=gsub(".csv","",non_image.tables)


input_list_non_image=NULL
for (i in 1:length(non_image.folder)){
	tmp.tabs=list.files(path=paste0(my.path,non_image.folder[i]))
	for (j in 1:length(tmp.tabs)){
		tmp.list=paste0(my.path,non_image.folder[i],"/",tmp.tabs[j])
		input_list_non_image=c(input_list_non_image,tmp.list)
	}
}
#total 183
#abcd_socdev_ctr01 (not merged,multiple entries, approved by Dr. Clark)
#abcd_fbdpas01 and abcd_fbdss01 (not merged,multiple entries, approved by Susan)
#######################################
#break image tables into 2 sections
#(too large to merge altogether)
image_1.tables=list.files(path="./SYNC/ABCD_3.0_imaging_tabulated_1/")
input_list_image_1=NULL
for (i in 1:length(image_1.tables)){
	tmp.list=paste0("./SYNC/ABCD_3.0_imaging_tabulated_1/",image_1.tables[i])
	input_list_image_1=c(input_list_image_1,tmp.list)
}
image_1.tables=gsub(".csv","",image_1.tables)
length(image_1.tables) #50 (including abcd_lt01)
which(duplicated(image_1.tables))

#######################################
image_2.tables=list.files(path="./SYNC/ABCD_3.0_imaging_tabulated_2/")
input_list_image_2=NULL
for (i in 1:length(image_2.tables)){
	tmp.list=paste0("./SYNC/ABCD_3.0_imaging_tabulated_2/",image_2.tables[i])
	input_list_image_2=c(input_list_image_2,tmp.list)
}
image_2.tables=gsub(".csv","",image_2.tables)
length(image_2.tables) #46 (including abcd_lt01)
which(duplicated(image_2.tables))

library(writexl)
sheets <- list("image_1" = data.frame(image_1.tables), "image_2" = data.frame(image_2.tables))
#write_xlsx(sheets, path="image.table.names_breakdown.xlsx")
#######################################


full.tables=unique(c(non_image.tables,image_1.tables,image_2.tables)) #abcd_lt01 in all sections: non-image, image_1, image_2
length(full.tables) #277
#raw data are not included
#write.csv(file="RDS3.0.all.tables.merged.csv",full.tables,row.names=F) #277 total
#####################################################################
#QC cross-check with NDA3.0
#From NDA
# abcd_instruments <- jsonlite::fromJSON("https://nda.nih.gov/api/datadictionary/v2/datastructure?source=ABCD%20Release%203.0")
# length(abcd_instruments$shortName) #293

# abcd_instruments$shortName[which(!abcd_instruments$shortName%in%full.tables)]
# full.tables[which(!full.tables%in%abcd_instruments$shortName)]

#####################################################################
#instr.excl=c("package_info","fmriresults01","genomics_sample03","aurora01","omics_experiments","error")
#which(instr.excl%in%full.tables) #should be 0

#alia = read.csv("~/Desktop/ABCD/analysis-nda3.0/DEAP aliases/NDA_DEAP_names_3.0.csv")
alia = read.csv("NDA_DEAP_names_3.0.csv")
#############################
#merge for non-image (or image)
#############################
#three different types
type="non_image"
#type="image_2"
#type="image_1"
#############################
input_list=eval(parse(text=paste0("input_list_",type)))
instrument.name=eval(parse(text=paste0(type,".tables")))
if (type=="non_image"){
	#extend wide-format deap into alia
	fitbit.w=read.csv("~/Desktop/ABCD/analysis-nda3.0/DEAP aliases/fitbit_wk_wide_format.deap.csv")[,-c(1,2)]
	colnames(fitbit.w)=colnames(alia)
	alia=data.frame(rbind(alia,fitbit.w))
	
}
#############################
tables = list()

for (p in 1:length(input_list)) {
  print(p)
  input = input_list[p]
  print(paste("import: ", input, " [", p, "/",length(input_list), "]", sep=""))
  
  dt <- tryCatch({
  	if(type=="non_image"){ #need to skip first line
    	a = read.csv(file = input, sep = ',',header = TRUE, row.names=NULL, comment.char = "", quote="\"", check.names=FALSE,skip=1)
    }else{
    	a = read.csv(file = input, sep = ',',header = TRUE, row.names=NULL, comment.char = "", quote="\"", check.names=FALSE)
    }
    a = as.data.frame(sapply(a, function(x) gsub("\"", "", x)))
    names(a) = as.list(sapply(names(a), function(x) gsub("\"", "", x)))
    a
  }, error = function(e) {
    print(e)
    return(read.table(file = input, sep = '\t',header = TRUE))
  })
  
  
  # replace variable names from nda with DEAP names
  instrument=instrument.name[p]
  
  #This is a wide-form instrument, capital letter in variable names,such as 	fit_ss_perday_sedentaryMin_wk1_wkday
  if(instrument=="abcd_fbwpas01"){ 
  	colnames(dt)=tolower(colnames(dt))
  }
  
  
  ali  = alia[which(alia$instrument == instrument),]
  nn = names(dt)
  for (q in 1:length(nn)) {
    if (nn[q] %in% ali$nda) {
      colnames(dt)[q] <- as.character(ali$abcd[ali$nda == nn[q]])
    }

  }
  if(instrument=="fhxp102"){ #these ___888 variables are repeated in fhxp201, rm from fhxp102
  	indx=which(grepl("___888",colnames(dt)))
  	dt=dt[,-indx]
  	
  }
  
  if(instrument%in%c("abcd_screen01","abcd_socdev_ctr01")){ #change screener_arm_1 to baseline_year_1_arm_1, nda dosen't have screener 
  	dt$eventname="baseline_year_1_arm_1"
  }
  
  if(instrument=="abcd_mri01"){#rm few empty rows
	rms=c("S065_INV2ZA2LC3N_20171215","S076_INV3E0WVH3G_20171129","S053_INVJ9GNXGK5_20180107","S076_INVWE1DE80Z_20170913","S076_INVXN6HMGK8_20170913")
	indx=which(dt$mri_info_visitid%in%rms)	
	if(length(indx)>0){
		dt=dt[-indx,]
	}
  }
  tables[[p]] = dt
}

len.tables=length(tables)


# if(type!="non_image"){
	# save(file=paste0(type,".tables.RData"),tables) #can save image tables here (take too long)
# }
#load(paste0(type,".tables.RData"))


# Sometimes the "eventname" column shared in many instruments is called "visit" (but in freesqc01 "eventname" and "visit" are different,
# it has been taken care in the NDA_DEAP_names file).
for (p in 1:len.tables) {
  dt = tables[[p]]
  if ("visit" %in% names(dt) ){
     print(p)
      print(instrument.name[p]) #should be nothing
  }
}


#Drop columns introduced by NDA, they are not required in the resulting table.
#keep dataset_id (QC purpose) and will remove later
#if we merge tables from SYNC, then don't need this
for (p in 1:len.tables) {
  dt = tables[[p]]
  dt = dt[,!(names(dt) %in% c(paste0(instrument.name[p],"_id"),"collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name"))]
 
  tables[[p]] = dt
}


#check columns that are all empty; rm those empty col;
emptycolumns = list()
for (p in 1:len.tables) {
  dt = tables[[p]]
  emptycolumns = append(emptycolumns,names(dt)[sapply(dt, function(x) all((x=="")|(x=="NA") | is.na(x)))])
  dt = dt[!sapply(dt, function(x) all((x=="")|(x=="NA") | is.na(x)))]
  tables[[p]] = dt
}
emptycolumn = unlist(emptycolumns)
length(emptycolumn) #for image_1:100; image_2: 140; non-image:965
if(length(emptycolumn)>0){
	write.csv(file=paste0("release3.0.",type,".empty.col.removed.csv"),emptycolumn,row.names=F)
}


# rm gender/sex,interview_age and interview_date from all instrument but keepig lt01 as anchor;
lt01.indx=which(instrument.name=="abcd_lt01"); #longitudinal tracking
rm.vars=c("visit","interview_age","interview_date","sex")
for (p in 1:len.tables) {
  dt = tables[[p]]
  if (p != lt01.indx){
  	dt = dt[,!(names(dt) %in% rm.vars)] 
  	tables[[p]] = dt
  }
}
table(tables[[lt01.indx]]$eventname)
# 1_year_follow_up_y_arm_1 18_month_follow_up_arm_1 2_year_follow_up_y_arm_1 30_month_follow_up_arm_1  6_month_follow_up_arm_1    baseline_year_1_arm_1 
#                   11235                     9911                     6571                     3601                    11398                    11878 
######################################
#check: if any table without eventname 
for (p in 1:len.tables) {
  dt = tables[[p]]
  if (!("eventname" %in% names(dt))) {
    print(p)
    print(instrument.name[p])
  }
}

#re-calibrate the levels in each table. Information that has been removed in previous steps could have changed the factor information in each table.
for (p in 1:len.tables) {
  dt = tables[[p]]
  dt = droplevels(dt)
  tables[[p]] = dt
}

# if(type!="non_image"){
	# save(file=paste0(type,".tables2.RData"),tables) #can save image tables here (take too long)
# }

############################################################
#check event (for image tables)
if(type != "non_image"){
	for(p in 1:len.tables){
		dt=tables[[p]]
		print(p)
		print(instrument.name[p])
		print(table(dt$eventname))
		cat("####################################\n")
	}
}

#check any subjectid at each event that is NOT from abcd_lt01? (any subject not supposed to be released?)
#should be nothing printing out
all.event=names(table(tables[[lt01.indx]]$eventname))
for(p in 1:len.tables){
	dt=tables[[p]]
	for (v in all.event){
		subj=as.character(dt[which(dt$eventname==v),1])
		if(length(subj)>0){
			lt01.id=as.character(tables[[lt01.indx]][which(tables[[lt01.indx]]$eventname==v),1])
			indx=which(!subj%in%lt01.id)
			if(length(indx)>0){
				print(p)
				print(instrument.name[p])
				cat("\nsubjects not from abcd_lt01 at", v,":",subj[indx],"\n####################################\n")
			}
		}
	}
}	

#check duplicates (should be nothing printing out)
require(openxlsx)
my.event=c("baseline_year_1_arm_1","1_year_follow_up_y_arm_1","2_year_follow_up_y_arm_1")
for (p in 1:len.tables) {
  	dt=tables[[p]]
  	dt$eventname=as.character(dt$eventname)
  	save.dt=NULL;
  for(i in 1:length(my.event)){
  	id =dt[which(tables[[p]]$eventname==my.event[i]),]
  	if (sum(duplicated(id[,1]))>0) {
  		#print(p)
  		print(instrument.name[p])
    	print(my.event[i])
    	cat("####################################\n")
    	indx=which(duplicated(id[,1]))
    	dup.sub=as.character(id[indx,1])
    	tb=id[which(id[,1]%in%dup.sub),]
    	save.dt=rbind(save.dt,tb)
     }
  }  
  
  if(!is.null(save.dt)){
  	save.dt=save.dt[order(save.dt$src_subject_id,save.dt$eventname),]
  	#write.xlsx(file=paste0(instrument.name[p],".dup.xlsx"),save.dt)
  }
}

############################################################
#In release 3.0, image only has baseline and 2 year data
if(type!="non_image"){
	dt=tables[[lt01.indx]]
	dt=dt[which(dt$eventname%in%c("baseline_year_1_arm_1","2_year_follow_up_y_arm_1")),]
	dt = droplevels(dt)
	tables[[lt01.indx]]=dt
}

############################################################
#Now we can merge the individual tables into a single spreadsheet. 
#The following loop performs repeated merging operations between pairs of intruments.

t2 = tables
rm(tables)
cnt=0
while ( length(t2) > 1 ) {
  cnt=cnt+1
  print(paste0("iteration : ",cnt));
  access= seq(1,length(t2)-1,2)
  for (i in access) {
    bm = dim(t2[[i]])

    by.vars=c("src_subject_id","eventname")
    t2[[i]] = merge(t2[[i]], t2[[i+1]], by=by.vars, all=TRUE)

    print(paste("rows before: ", bm[1], dim(t2[[i+1]])[1], " rows after: ",dim(t2[[i]])[1], "indices: ",i,i+1," columns: ",bm[2],"+",dim(t2[[i+1]])[2], "-",length(by.vars)," = ",dim(t2[[i]])[2]))
  }
  # for odd number of instruments add the last spreadsheet back to the list
  if (length(t2) %% 2 != 0) access = append(access,length(t2))
  # reduce the list
  t2 = t2[access]
}
nda3.0 = t2[[1]] #Non-image:14720 column; image_1: 18449 29042;image_2:18449 26606
dim(nda3.0) #non_image:54594 14718
table(nda3.0$eventname)

#double check duplicated variables:
colnames(nda3.0)[grepl(".x",colnames(nda3.0),fixed=T)]
colnames(nda3.0)[grepl(".y",colnames(nda3.0),fixed=T)]
colnames(nda3.0)[grepl(".1",colnames(nda3.0),fixed=T)]


#need this file to make coding choice in non-image; 
#for image can use this to cross-check variables from two parts (any duplicates?)
col_names=colnames(nda3.0)
save(file=paste0(type,"_column_names.RData"),col_names)


#rm dataset_id (no need, we merge from SYNC, not nda tables)
#nda3.0=nda3.0[,-which(grepl("dataset_id",colnames(nda3.0)))]


saveRDS(nda3.0, paste0("nda3.0_orig_",type,".Rds"))

#if non_image: go to: core_demographics3.0.R
#if image: skip core_demographics3.0.R, go to categorical_extension3.0.R directly
