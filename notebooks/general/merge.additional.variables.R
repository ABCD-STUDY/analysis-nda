
#################################################################################
#read in data from output of categorical_extension.R
script.dir <- "~/Desktop/ABCD/analysis-nda3.0/RDS"
setwd(script.dir)


type="non_image" #only for non_image
nda3.0.ext = readRDS(paste0("nda3.0_ext_",type,".Rds"))

#################################################################################
#read in neurocog principal component scores, nda18_w_nc_pca.csv was given by Wes
#at release2.0.1
npc.dat=read.csv("nda18_w_nc_pca.csv")[,c("src_subject_id","pc1_new","pc2_new","pc3_new")]
colnames(npc.dat)[-1]=c("neurocog_pc1", "neurocog_pc2", "neurocog_pc3")
npc.dat$eventname="baseline_year_1_arm_1"


#################################################################################
#famhx_ss BOCF under name famhx_ss_p_xxx.bl?
fam.ss.var=colnames(nda3.0.ext)[which(grepl("famhx_ss_",colnames(nda3.0.ext)))]
dat.fam.ss=nda3.0.ext[which(nda3.0.ext$eventname=="baseline_year_1_arm_1"),c("src_subject_id","eventname",fam.ss.var)]
#################################################################################
#merge dat.fam.ss and pc scores, then merge with nda3.0.ext
new.vars=merge(dat.fam.ss,npc.dat,by=c("src_subject_id","eventname"),all.x=T)

#BOCF under new name .bl
colnames(new.vars)[-c(1,2)]=paste0(colnames(new.vars)[-c(1,2)],".bl")
new.vars=new.vars[,-2]


#drop original famhx_ss;
indx=which(colnames(nda3.0.ext)%in%fam.ss.var)
if(length(indx)>0){
	nda3.0.ext=nda3.0.ext[,-indx]
}
nda3.0=merge(nda3.0.ext,new.vars,by=c("src_subject_id"),all.x=T)
dim(nda3.0) # 54594 14737

######################################
#save some information for Fangzhou
dic.fam=data.frame(cbind(deap=fam.ss.var,deap.bl=paste0(fam.ss.var,".bl")))
deap=read.csv("~/Desktop/ABCD/analysis-nda3.0/DEAP aliases/NDA_DEAP_names_3.0.csv")
deap=deap[which(deap[,3]=="abcd_fhxssp01"),]
colnames(deap)[2]="deap"
deap.fam.final=merge(dic.fam,deap,by="deap",all.x=T)
deap.fam.final=deap.fam.final[,c("nda","deap","deap.bl","instrument")]
for (i in 1:dim(deap.fam.final)[2]){
	deap.fam.final[,i]=as.character(deap.fam.final[,i])
}
indx=which(is.na(deap.fam.final$nda))
if(length(indx)>0){
	deap.fam.final$nda[indx]=deap.fam.final$deap[indx]
	deap.fam.final$instrument[indx]="abcd_fhxssp01"
}
#write.csv(file="famhx_ss dic basline carry forward.csv",deap.fam.final,row.names=F)
#################################################################################
#for internal DEAP development only
nda3.0$event_name=nda3.0$eventname
nda3.0$interview_datetime=nda3.0$interview_date
nda3.0$sex_at_birth=nda3.0$sex 

#################################################################################
#genetic data BOCF (baseline observation carry forward) directly under the same names
var=colnames(nda3.0)[which(grepl("^genetic_",colnames(nda3.0)))]
dat.bs=nda3.0[which(nda3.0$eventname=="baseline_year_1_arm_1"),c("subjectid",var)]

indx=which(colnames(nda3.0)%in%var)
if(length(indx)>0){
	nda3.0=nda3.0[,-indx]
}
nda3.0=merge(nda3.0,dat.bs,by="subjectid",all.x=T)
dim(nda3.0)#non_image:54594 14740 
table(nda3.0$eventname)

saveRDS(nda3.0, paste0("nda3.0_",type,".Rds"))

